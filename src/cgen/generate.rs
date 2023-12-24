use std::{fs::File, io::Write, iter};

use camino::Utf8PathBuf;
use pretty::RcDoc as D;
use rustc_hash::FxHashMap;
use ulid::Ulid;
use ustr::{ustr, Ustr};

use crate::{
    cgen::{
        builtin::BinOpData,
        name_gen::LocalNames,
        ty::CTy,
        util::{
            self, assign, attr, block, block_, bool_value, goto_stmt, if_stmt,
            stmt, str_value, struct_lit, unit_value, NEST,
        },
    },
    db::{Db, StructId, StructInfo, StructKind},
    middle::{Pat, UnOp},
    mir::{
        Block, Body, Const, Fn, FnSig, FnSigId, Global, GlobalKind, Inst, Mir,
        StaticGlobal, ValueId, ValueKind,
    },
    target::TargetMetrics,
    ty::{Ty, TyKind},
};

const PRELUDE: &str = include_str!("../../rt/jin.c");

pub struct Generator<'db> {
    pub db: &'db mut Db,
    pub mir: &'db Mir,
    pub types: Vec<D<'db>>,
    pub fn_decls: Vec<D<'db>>,
    pub globals: Vec<D<'db>>,
    pub fn_defs: Vec<D<'db>>,
    pub target_metrics: TargetMetrics,
    pub struct_names: FxHashMap<StructId, Ustr>,
    pub struct_destroy_fns: FxHashMap<StructId, Ustr>,
    pub defining_types: bool,
}

#[derive(Debug, Clone)]
pub struct FnState<'db> {
    pub body: &'db Body,
    pub local_names: LocalNames,
}

impl<'db> FnState<'db> {
    pub fn new(body: &'db Body) -> Self {
        Self { body, local_names: LocalNames::new() }
    }
}

impl<'db> Generator<'db> {
    pub fn run(mut self) -> Utf8PathBuf {
        self.define_types();
        self.define_struct_destroys();
        self.predefine_fns();
        self.define_globals();
        self.define_fns();
        let main_fn = self.codegen_main_fn();
        self.write_to_file(main_fn)
    }

    fn write_to_file(self, main_fn: D<'db>) -> Utf8PathBuf {
        const WIDTH: usize = 80;

        let path = self.db.output_path().with_extension("c");
        let mut file =
            File::create(self.db.output_path().with_extension("c")).unwrap();

        file.write_all(PRELUDE.as_bytes()).unwrap();
        file.write_all(b"\n").unwrap();

        let types = D::intersperse(self.types, D::hardline());
        let fn_decls = D::intersperse(self.fn_decls, D::hardline());
        let globals = D::intersperse(self.globals, D::hardline());
        let fn_defs =
            D::intersperse(self.fn_defs, D::hardline().append(D::hardline()));

        let final_doc = D::intersperse(
            [types, fn_decls, globals, fn_defs, main_fn],
            D::hardline().append(D::hardline()),
        );

        final_doc.render(WIDTH, &mut file).expect("writing to work");

        path
    }

    pub fn codegen_main_fn(&self) -> D<'db> {
        let main_fn_name = &self.mir.fn_sigs
            [self.mir.main_fn.expect("to have a main fn")]
        .name;

        let mut statements: Vec<D> = vec![];

        // Global variable initialization
        statements.extend(self.get_global_init_order().into_iter().map(|id| {
            let glob = &self.mir.globals[id];
            stmt(|| {
                assign(
                    D::text(glob.name.as_str()),
                    D::text(global_init_fn_name(glob)).append(D::text("()")),
                )
            })
        }));

        // Call entry point
        statements.push(stmt(|| {
            D::text(main_fn_name.as_str()).append(D::text("()"))
        }));

        // return 0
        statements.push(stmt(|| D::text("return 0")));

        D::text("int main() {")
            .append(
                D::hardline()
                    .append(D::intersperse(statements, D::hardline()))
                    .nest(NEST)
                    .group(),
            )
            .append(D::hardline())
            .append(D::text("}"))
    }

    pub fn define_types(&mut self) {
        self.defining_types = true;

        for struct_info in &self.db.structs {
            let name = ustr(&self.db[struct_info.def_id].qpath.join_with("_"));
            self.struct_names.insert(struct_info.id, name);
        }

        for struct_info in &self.db.structs {
            let doc = self.codegen_struct_def(struct_info);
            self.types.push(stmt(|| doc));
        }

        self.defining_types = false;
    }

    pub fn define_struct_destroys(&mut self) {
        for sid in self.db.structs.keys() {
            self.create_struct_destroy_entry(sid);
        }

        for sid in self.db.structs.keys() {
            self.codegen_struct_destroy(sid);
        }
    }

    pub fn predefine_fns(&mut self) {
        for sig in self.mir.fn_sigs.values() {
            let doc = self.codegen_fn_sig(sig);
            self.fn_decls.push(stmt(|| doc));
        }
    }

    pub fn define_globals(&mut self) {
        for glob in self.mir.globals.values() {
            let decl = glob.ty.cdecl(self, D::text(glob.name.as_str()));

            let glob_doc = match &glob.kind {
                GlobalKind::Static { .. } => decl,
                GlobalKind::Extern => {
                    D::text("extern").append(D::space()).append(decl)
                }
            };

            self.globals.push(stmt(|| glob_doc));

            let GlobalKind::Static(StaticGlobal { body, result: value }) =
                &glob.kind
            else {
                return;
            };

            let mut state = FnState::new(body);

            let return_stmt = stmt(|| {
                D::text("return")
                    .append(D::space())
                    .append(self.value(&state, *value))
            });

            let block_docs: Vec<D> = body
                .blocks()
                .iter()
                .map(|blk| self.codegen_block(&mut state, blk))
                .chain(iter::once(return_stmt))
                .collect();

            let init_fn_doc = D::text("FORCE_INLINE")
                .append(D::space())
                .append(glob.ty.cdecl(self, D::text(global_init_fn_name(glob))))
                .append(D::text("()"))
                .append(D::space())
                .append(block_(
                    || {
                        D::intersperse(
                            block_docs,
                            D::hardline().append(D::hardline()),
                        )
                        .group()
                    },
                    0,
                ));

            self.globals.push(stmt(|| init_fn_doc));
        }
    }

    fn codegen_struct_def(&self, struct_info: &StructInfo) -> D<'db> {
        let name = D::text(self.struct_names[&struct_info.id].as_str());

        D::text("typedef")
            .append(D::space())
            .append(D::text("struct"))
            .append(D::space())
            .append(name.clone())
            .append(D::space())
            .append(block(|| {
                D::intersperse(
                    struct_info.fields.iter().map(|f| {
                        stmt(|| {
                            f.ty.cdecl(self, D::text(f.name.name().as_str()))
                        })
                    }),
                    D::hardline(),
                )
            }))
            .append(D::space())
            .append(name)
    }

    fn codegen_fn_sig(&self, sig: &FnSig) -> D<'db> {
        let fn_ty = sig.ty.as_fn().expect("a function type");

        let initial = if sig.is_extern {
            D::text("extern").append(D::space())
        } else {
            D::nil()
        };

        let sig_doc = fn_ty.ret.cdecl(self, D::text(sig.name.as_str())).append(
            D::text("(")
                .append(
                    D::intersperse(
                        sig.params.iter().map(|p| {
                            let name = match &p.pat {
                                Pat::Name(name) => name.word.name(),
                                Pat::Discard(_) => {
                                    ustr(&Ulid::new().to_string())
                                }
                            };

                            p.ty.cdecl(self, D::text(name.as_str()))
                        }),
                        D::text(",").append(D::space()),
                    )
                    .nest(2)
                    .group(),
                )
                .append(if sig.is_c_variadic {
                    D::text(", ...")
                } else {
                    D::nil()
                })
                .append(D::text(")")),
        );

        let mut attr_docs = vec![];

        if fn_ty.ret.is_never() {
            attr_docs.push(attr("noreturn"));
        }

        let attrs = if attr_docs.is_empty() {
            D::nil()
        } else {
            D::space().append(D::intersperse(attr_docs, D::space()))
        };

        initial.append(sig_doc).append(attrs).group()
    }

    pub fn define_fns(&mut self) {
        for fun in self.mir.fns.values() {
            let doc = self.codegen_fn_def(fun);
            self.fn_defs.push(doc);
        }

        for (sid, sig) in &self.mir.struct_ctors {
            let doc = self.codegen_struct_ctor(*sid, *sig);
            self.fn_defs.push(stmt(|| doc));
        }
    }

    fn codegen_fn_def(&mut self, fun: &'db Fn) -> D<'db> {
        let sig = &self.mir.fn_sigs[fun.sig];

        let mut state = FnState::new(&fun.body);

        for param in &sig.params {
            match &param.pat {
                Pat::Name(name) => {
                    state.local_names.insert(name.id, self.db[name.id].name);
                }
                Pat::Discard(_) => (),
            }
        }

        let block_docs: Vec<D> = fun
            .body
            .blocks()
            .iter()
            .map(|blk| self.codegen_block(&mut state, blk))
            .collect();

        self.codegen_fn_sig(sig).append(D::space()).append(block_(
            || {
                D::intersperse(block_docs, D::hardline().append(D::hardline()))
                    .group()
            },
            0,
        ))
    }

    fn codegen_struct_ctor(
        &mut self,
        sid: StructId,
        sig_id: FnSigId,
    ) -> D<'db> {
        let struct_info = &self.db[sid];
        let sig = &self.mir.fn_sigs[sig_id];

        let statements = match self.db[sid].kind {
            StructKind::Ref => {
                let alloc_doc =
                    util::call_alloc(D::text(self.struct_names[&sid].as_str()));

                let lit_name = D::text("this");
                let lit_decl_doc = VariableDoc::assign(
                    self,
                    struct_info.ty(),
                    lit_name.clone(),
                    alloc_doc,
                );

                let struct_init_doc = D::intersperse(
                    struct_info.fields.iter().map(|f| {
                        stmt(|| {
                            let name = f.name.name().as_str();

                            util::assign(
                                util::member(lit_name.clone(), name, true),
                                D::text(name),
                            )
                        })
                    }),
                    D::hardline(),
                );

                let return_doc = stmt(|| {
                    D::text("return").append(D::space()).append(lit_name)
                });

                D::intersperse(
                    [lit_decl_doc, struct_init_doc, return_doc],
                    D::hardline(),
                )
            }
            StructKind::Extern => {
                let struct_lit_doc = struct_lit(
                    struct_info
                        .fields
                        .iter()
                        .map(|f| {
                            let name = f.name.name().as_str();
                            (name, D::text(name))
                        })
                        .collect(),
                );

                let lit_name = D::text("x");
                let lit_decl_doc = VariableDoc::assign(
                    self,
                    struct_info.ty(),
                    lit_name.clone(),
                    struct_lit_doc,
                );

                let return_doc = stmt(|| {
                    D::text("return").append(D::space()).append(lit_name)
                });

                D::intersperse([lit_decl_doc, return_doc], D::hardline())
            }
        };

        self.codegen_fn_sig(sig).append(D::space()).append(block(|| statements))
    }

    fn create_struct_destroy_entry(&mut self, sid: StructId) {
        let struct_info = &self.db[sid];
        let fn_name = ustr(
            &self.db[struct_info.def_id]
                .qpath
                .clone()
                .child(ustr("destroy"))
                .join_with("_"),
        );
        self.struct_destroy_fns.insert(sid, fn_name);
    }

    fn codegen_struct_destroy(&mut self, sid: StructId) {
        let struct_info = &self.db[sid];

        if struct_info.kind.is_ref() {
            let param = D::text("this");

            let destroy_fields = D::intersperse(
                struct_info.fields.iter().filter_map(|f| {
                    self.get_ty_destroy_fn(f.ty).map(|fn_name| {
                        Self::codegen_destroy_call(
                            fn_name,
                            util::member(param.clone(), f.name.as_str(), true),
                        )
                    })
                }),
                D::hardline(),
            );

            let dealloc_this = stmt(|| util::call_dealloc(param.clone()));

            let statements =
                D::intersperse([destroy_fields, dealloc_this], D::hardline());

            let fn_name = self.struct_destroy_fns[&sid];
            let fn_decl_doc = D::text(format!("void {fn_name}"))
                .append(D::text("("))
                .append(sid.cty(self))
                .append(D::space())
                .append(param)
                .append(D::text(")"));

            let fn_def_doc = fn_decl_doc
                .clone()
                .append(D::space())
                .append(block(|| statements));

            self.fn_decls.push(stmt(|| fn_decl_doc));
            self.fn_defs.push(fn_def_doc);
        }
    }

    fn codegen_block(
        &mut self,
        state: &mut FnState<'db>,
        blk: &'db Block,
    ) -> D<'db> {
        D::text(blk.display_name())
            .append(D::text(":;"))
            .append(D::hardline())
            .append(
                D::intersperse(
                    blk.insts.iter().map(|i| self.codegen_inst(state, i)),
                    D::hardline(),
                )
                .group(),
            )
            .nest(NEST)
            .group()
    }

    fn codegen_inst(
        &mut self,
        state: &mut FnState<'db>,
        inst: &'db Inst,
    ) -> D<'db> {
        match inst {
            Inst::Local { value, init } => {
                let value = state.body.value(*value);
                let name = match value.kind {
                    ValueKind::Register(name) => {
                        self.register_name(value.id, name)
                    }
                    ValueKind::Local(id) => state
                        .local_names
                        .insert_unique(id, self.db[id].name)
                        .to_string(),
                    _ => unreachable!(),
                };

                VariableDoc::assign(
                    self,
                    value.ty,
                    D::text(name),
                    self.value(state, *init),
                )
            }
            Inst::Store { value, target } => stmt(|| {
                assign(self.value(state, *target), self.value(state, *value))
            }),
            Inst::Destroy { value, destroy_flag, span: _ } => {
                let destroy_call =
                    self.codegen_destroy(state, *value).unwrap_or_else(|| {
                        unreachable!(
                            "tried to destroy value by mistake: {:?}",
                            state.body.value(*value)
                        )
                    });

                if let &Some(destroy_flag) = destroy_flag {
                    util::if_stmt(
                        self.value(state, destroy_flag),
                        destroy_call,
                        None,
                    )
                } else {
                    destroy_call
                }
            }
            Inst::Br { target } => goto_stmt(state.body.block(*target)),
            Inst::BrIf { cond, then, otherwise } => if_stmt(
                self.value(state, *cond),
                goto_stmt(state.body.block(*then)),
                otherwise.map(|o| goto_stmt(state.body.block(o))),
            ),
            Inst::If { value, cond, then, otherwise } => {
                self.value_assign(state, *value, || {
                    self.value(state, *cond)
                        .append(D::space())
                        .append(D::text("?"))
                        .append(D::space())
                        .append(self.value(state, *then))
                        .append(D::space())
                        .append(D::text(":"))
                        .append(D::space())
                        .append(self.value(state, *otherwise))
                })
            }
            Inst::Return { value } => stmt(|| {
                D::text("return")
                    .append(D::space())
                    .append(self.value(state, *value))
            }),
            Inst::Call { value, callee, args } => {
                self.value_assign(state, *value, || {
                    self.value(state, *callee)
                        .append(D::text("("))
                        .append(
                            D::intersperse(
                                args.iter()
                                    .copied()
                                    .map(|a| self.value(state, a)),
                                D::text(",").append(D::space()),
                            )
                            .nest(1)
                            .group(),
                        )
                        .append(D::text(")"))
                })
            }
            Inst::Binary { value, lhs, rhs, op, span } => self.codegen_bin_op(
                state,
                &BinOpData {
                    target: *value,
                    lhs: *lhs,
                    rhs: *rhs,
                    op: *op,
                    ty: state.body.value(*value).ty,
                    span: *span,
                },
            ),
            Inst::Unary { value, inner, op } => {
                let inner_ty = state.body.value(*inner).ty;
                let op_str = match op {
                    UnOp::Neg => op.as_str(),
                    UnOp::Not => {
                        if inner_ty.is_bool() {
                            "!"
                        } else {
                            "~"
                        }
                    }
                };

                self.value_assign(state, *value, || {
                    D::text(op_str).append(self.value(state, *inner))
                })
            }
            Inst::Cast { value, inner, target, span } => {
                self.codegen_cast(state, *value, *inner, *target, *span)
            }
            Inst::StrLit { value, lit } => {
                self.value_assign(state, *value, || str_value(lit))
            }
        }
    }

    fn codegen_destroy(
        &self,
        state: &FnState<'db>,
        value: ValueId,
    ) -> Option<D<'db>> {
        let value_ty = state.body.value(value).ty;
        self.get_ty_destroy_fn(value_ty).map(|fn_name| {
            Self::codegen_destroy_call(fn_name, self.value(state, value))
        })
    }

    fn get_ty_destroy_fn(&self, ty: Ty) -> Option<Ustr> {
        match ty.kind() {
            TyKind::Struct(sid) => self.struct_destroy_fns.get(sid).copied(),
            _ => None,
        }
    }

    fn codegen_destroy_call(destroy_fn: Ustr, value: D<'db>) -> D<'db> {
        stmt(|| {
            D::text(destroy_fn.as_str())
                .append(D::text("("))
                .append(value)
                .append(D::text(")"))
        })
    }

    pub fn value_assign(
        &self,
        state: &FnState<'db>,
        id: ValueId,
        f: impl FnOnce() -> D<'db>,
    ) -> D<'db> {
        let value = state.body.value(id);
        VariableDoc::assign(self, value.ty, self.value(state, id), f())
    }

    pub fn value_decl(&self, state: &FnState<'db>, id: ValueId) -> D<'db> {
        let value = state.body.value(id);
        VariableDoc::decl(self, value.ty, self.value(state, value.id))
    }

    pub fn value(&self, state: &FnState<'db>, id: ValueId) -> D<'db> {
        match &state.body.value(id).kind {
            ValueKind::Register(name) => D::text(self.register_name(id, *name)),
            ValueKind::Local(id) => {
                D::text(state.local_names.get(*id).unwrap().as_str())
            }
            ValueKind::Global(id) => {
                D::text(self.mir.globals[*id].name.as_str())
            }
            ValueKind::Fn(id) => D::text(self.mir.fn_sigs[*id].name.as_str()),
            ValueKind::Const(value) => codegen_const_value(value),
            ValueKind::Member(value, member) => util::member(
                self.value(state, *value),
                member,
                state.body.value(*value).ty.is_ptr(self),
            ),
        }
    }

    fn register_name(&self, value: ValueId, name: Option<Ustr>) -> String {
        format!("{}{}", name.unwrap_or(ustr("v")), value)
    }
}

fn codegen_const_value(value: &Const) -> D<'_> {
    match value {
        Const::Str(value) => str_value(value),
        Const::Int(value) => D::text(value.to_string()),
        Const::Float(value) => D::text(value.to_string()),
        Const::Bool(value) => bool_value(*value),
        Const::Unit => unit_value(),
    }
}

struct VariableDoc<'a> {
    ty: Ty,
    name: D<'a>,
    value: Option<D<'a>>,
}

impl<'a> VariableDoc<'a> {
    fn decl(cx: &Generator<'a>, ty: Ty, name: D<'a>) -> D<'a> {
        Self { ty, name, value: None }.into_doc(cx)
    }

    fn assign(cx: &Generator<'a>, ty: Ty, name: D<'a>, value: D<'a>) -> D<'a> {
        Self { ty, name, value: Some(value) }.into_doc(cx)
    }

    fn into_doc(self, cx: &Generator<'a>) -> D<'a> {
        stmt(|| {
            let decl = self.ty.cdecl(cx, self.name);

            let value = if let Some(value) = self.value {
                D::space().append(D::text("=")).append(D::space()).append(value)
            } else {
                D::nil()
            };

            decl.append(value)
        })
    }
}

fn global_init_fn_name(glob: &Global) -> String {
    format!("{}_init", glob.name.as_str())
}
