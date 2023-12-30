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
            stmt, str_value, unit_value, NEST,
        },
    },
    db::{Adt, AdtId, AdtKind, Db, StructDef},
    middle::{Pat, UnOp},
    mir::{
        Block, Body, Const, Fn, FnSig, Global, GlobalKind, Inst, Mir,
        StaticGlobal, ValueId, ValueKind,
    },
    target::TargetMetrics,
    ty::{Ty, TyKind},
};

const PRELUDE: &str = include_str!("../../rt/jin.c");
pub const DATA_FIELD: &str = "data";
pub const REFCNT_FIELD: &str = "refcnt";

pub struct Generator<'db> {
    pub db: &'db mut Db,
    pub mir: &'db Mir,
    pub types: Vec<D<'db>>,
    pub fn_decls: Vec<D<'db>>,
    pub globals: Vec<D<'db>>,
    pub fn_defs: Vec<D<'db>>,
    pub target_metrics: TargetMetrics,
    pub adt_names: FxHashMap<AdtId, Ustr>,
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

        for struct_def in &self.db.adts {
            let name = ustr(&self.db[struct_def.def_id].qpath.join_with("_"));
            self.adt_names.insert(struct_def.id, name);
        }

        for adt in &self.db.adts {
            self.types.push(self.codegen_adt(adt));
        }

        self.defining_types = false;
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

    fn codegen_adt(&self, adt: &Adt) -> D<'db> {
        match &adt.kind {
            AdtKind::Struct(struct_def) => self.codegen_struct_def(struct_def),
        }
    }

    fn codegen_struct_def(&self, struct_def: &StructDef) -> D<'db> {
        let adt_name = self.adt_names[&struct_def.id];

        let data_name = D::text(format!("{adt_name}__data"));

        let data_typedef = stmt(|| {
            D::text("typedef")
                .append(D::space())
                .append(D::text("struct"))
                .append(D::space())
                .append(data_name.clone())
                .append(D::space())
                .append(block(|| {
                    D::intersperse(
                        struct_def.fields.iter().map(|f| {
                            stmt(|| {
                                f.ty.cdecl(
                                    self,
                                    D::text(f.name.name().as_str()),
                                )
                            })
                        }),
                        D::hardline(),
                    )
                }))
                .append(D::space())
                .append(data_name.clone())
        });

        let rc_name = D::text(adt_name.as_str());

        let rc_typedef = stmt(|| {
            D::text("typedef")
                .append(D::space())
                .append(D::text("struct"))
                .append(D::space())
                .append(rc_name.clone())
                .append(D::space())
                .append(block(|| {
                    D::intersperse(
                        [
                            stmt(|| {
                                data_name
                                    .append(D::space())
                                    .append(D::text(DATA_FIELD))
                            }),
                            stmt(|| {
                                D::text("usize")
                                    .append(D::space())
                                    .append(D::text(REFCNT_FIELD))
                            }),
                        ],
                        D::hardline(),
                    )
                }))
                .append(D::space())
                .append(rc_name)
        });

        D::intersperse([data_typedef, rc_typedef], D::hardline())
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
    }

    fn codegen_fn_def(&mut self, fun: &'db Fn) -> D<'db> {
        let sig = &self.mir.fn_sigs[fun.sig];

        let mut state = FnState::new(&fun.body);

        for param in &sig.params {
            match &param.pat {
                Pat::Name(name) => {
                    // The parameter's name id could be INVALID when it's generated ad-hoc.
                    // For example, in type constructor parameters.
                    if !name.id.is_invalid() {
                        state
                            .local_names
                            .insert(name.id, self.db[name.id].name);
                    }
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
            Inst::StackAlloc { value, init } => {
                let value = state.body.value(*value);

                let name = match value.kind {
                    ValueKind::Register(name) => {
                        Self::register_name(value.id, name)
                    }
                    ValueKind::Local(id) => state
                        .local_names
                        .insert_unique(id, self.db[id].name)
                        .to_string(),
                    _ => unreachable!(),
                };

                match *init {
                    Some(init) => VariableDoc::assign(
                        self,
                        value.ty,
                        D::text(name),
                        self.value(state, init),
                    ),
                    None => VariableDoc::decl(self, value.ty, D::text(name)),
                }
            }
            Inst::Alloc { value } => {
                let ty_doc = match state.body.value(*value).ty.kind() {
                    TyKind::Adt(sid) => D::text(self.adt_names[sid].as_str()),
                    kind => panic!("unexpected type {kind:?} in Inst::Alloc"),
                };

                let value_doc = self
                    .value_assign(state, *value, || util::call_alloc(ty_doc));

                let zero_refcnt =
                    stmt(|| self.refcnt_field(state, *value).append(" = 0"));

                D::intersperse([value_doc, zero_refcnt], D::hardline())
            }
            Inst::Store { value, target } => stmt(|| {
                assign(self.value(state, *target), self.value(state, *value))
            }),
            Inst::Free { value, destroy_flag, span } => {
                let stmts = self.refcheck_and_free(state, *value, *span);

                if let &Some(destroy_flag) = destroy_flag {
                    util::if_stmt(self.value(state, destroy_flag), stmts, None)
                } else {
                    stmts
                }
            }
            Inst::IncRef { value, target } => {
                let target_doc = self.value(state, *target);
                let incref = stmt(|| {
                    util::field(target_doc.clone(), REFCNT_FIELD, true)
                        .append(" += 1")
                });

                D::intersperse(
                    [incref, self.value_assign(state, *value, || target_doc)],
                    D::hardline(),
                )
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
                    UnOp::Ref(_) => unreachable!(),
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
            ValueKind::Register(name) => {
                D::text(Self::register_name(id, *name))
            }
            ValueKind::Name(name) => D::text(name.as_str()),
            ValueKind::Local(id) => {
                D::text(state.local_names.get(*id).unwrap().as_str())
            }
            ValueKind::Global(id) => {
                D::text(self.mir.globals[*id].name.as_str())
            }
            ValueKind::Fn(id) => D::text(self.mir.fn_sigs[*id].name.as_str()),
            ValueKind::Const(value) => codegen_const_value(value),
            ValueKind::Field(value, field) => self.field(state, *value, field),
        }
    }

    fn register_name(value: ValueId, name: Option<Ustr>) -> String {
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

pub struct VariableDoc<'a> {
    ty: Ty,
    name: D<'a>,
    value: Option<D<'a>>,
}

impl<'a> VariableDoc<'a> {
    pub fn decl(cx: &Generator<'a>, ty: Ty, name: D<'a>) -> D<'a> {
        Self { ty, name, value: None }.into_doc(cx)
    }

    pub fn assign(
        cx: &Generator<'a>,
        ty: Ty,
        name: D<'a>,
        value: D<'a>,
    ) -> D<'a> {
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
