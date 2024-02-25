use std::{fs::File, io::Write, iter};

use camino::Utf8PathBuf;
use compiler_data_structures::index_vec::Key as _;
use pretty::RcDoc as D;
use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use compiler_core::{
    db::{AdtField, AdtId, AdtKind, Db, StructDef, StructKind, UnionDef, UnionKind, VariantId},
    mangle,
    middle::{CallConv, Pat, UnOp},
    target::TargetMetrics,
    ty::{fold::TyFolder, Instantiation, Ty},
    word::Word,
};
use compiler_mir::{
    Block, Body, Const, Fn, FnSig, Global, GlobalKind, Inst, Mir, RtCallKind, StaticGlobal,
    ValueId, ValueKind,
};

use crate::{
    builtin::BinOpData,
    name_gen::LocalNames,
    ty::CTy,
    util::{
        self, assign, attr, block, block_, bool_value, goto_stmt, stmt, str_value, unit_value, NEST,
    },
};

pub const DATA_FIELD: &str = "data";
pub const START_FIELD: &str = "start";
pub const REFCNT_FIELD: &str = "refcnt";
pub const TAG_FIELD: &str = "tag";

pub struct Generator<'db> {
    pub db: &'db mut Db,
    pub mir: &'db Mir,
    pub types: Vec<D<'db>>,
    pub rc_types: Vec<D<'db>>,
    pub fn_decls: Vec<D<'db>>,
    pub consts: Vec<D<'db>>,
    pub globals: Vec<D<'db>>,
    pub fn_defs: Vec<D<'db>>,
    pub target_metrics: TargetMetrics,
    pub adt_names: FxHashMap<Ty, Ustr>,
}

#[derive(Debug, Clone)]
pub struct GenState<'db> {
    pub name: Ustr,
    pub body: &'db Body,
    pub local_names: LocalNames,
    pub param_names: Vec<Ustr>,
}

impl<'db> GenState<'db> {
    pub fn new(name: Ustr, body: &'db Body) -> Self {
        Self { name, body, local_names: LocalNames::new(), param_names: vec![] }
    }

    pub fn ty_of(&self, value: ValueId) -> Ty {
        self.body.value(value).ty
    }

    pub fn value_is_slice(&self, value: ValueId) -> bool {
        self.ty_of(value).auto_deref().is_slice_like()
    }
}

impl<'db> Generator<'db> {
    pub fn run(mut self) -> Utf8PathBuf {
        self.predefine_fns();
        self.define_globals();
        self.define_fns();
        let main_fn = self.codegen_main_fn();
        self.write_to_file(main_fn)
    }

    fn write_to_file(self, main_fn: D<'db>) -> Utf8PathBuf {
        const WIDTH: usize = 80;

        let path = self.db.output_path().with_extension("c");
        let mut file = File::create(self.db.output_path().with_extension("c")).unwrap();

        file.write_all(b"#include \"jinrt.h\"\n\n").unwrap();

        let decls = D::intersperse(
            self.types
                .into_iter()
                .chain(self.rc_types)
                .chain(self.fn_decls)
                .chain(self.consts)
                .chain(self.globals),
            D::hardline(),
        );
        let fns = D::intersperse(self.fn_defs, D::hardline().append(D::hardline()));

        let final_doc = D::intersperse([decls, fns, main_fn], D::hardline().append(D::hardline()));

        final_doc.render(WIDTH, &mut file).expect("writing to work");

        path
    }

    pub fn codegen_main_fn(&self) -> D<'db> {
        let main_fn_name =
            &self.mir.fn_sigs[self.mir.main_fn.expect("to have a main fn")].mangled_name;

        let mut statements: Vec<D> = vec![];

        // Initialize runtime
        statements.push(stmt(|| util::call(D::text("jinrt_init"), [])));

        // Initialize global variables
        statements.extend(self.get_global_init_order().into_iter().map(|id| {
            let glob = &self.mir.globals[id];
            stmt(|| {
                assign(
                    D::text(glob.name.as_str()),
                    D::text(global_init_fn_name(glob)).append(D::text("()")),
                )
            })
        }));

        // Create new backtrace
        statements.push(stmt(|| D::text("jinrt_backtrace *backtrace = jinrt_backtrace_new()")));

        // Call entry point
        statements
            .push(stmt(|| util::call(D::text(main_fn_name.as_str()), [D::text("backtrace")])));

        // Free backtrace
        statements.push(stmt(|| D::text("jinrt_backtrace_free(backtrace)")));

        // return 0
        statements.push(stmt(|| D::text("return 0")));

        D::text("int main() {")
            .append(
                D::hardline().append(D::intersperse(statements, D::hardline())).nest(NEST).group(),
            )
            .append(D::hardline())
            .append(D::text("}"))
    }

    pub fn predefine_fns(&mut self) {
        for sig in self.mir.fn_sigs.values() {
            let doc = self.codegen_fn_sig(sig);
            self.fn_decls.push(stmt(|| doc));
        }
    }

    pub fn define_globals(&mut self) {
        for glob in self.mir.globals.values() {
            let name_doc = D::text(glob.name.as_str());

            match &glob.kind {
                GlobalKind::Const(value) => {
                    let doc =
                        VariableDoc::assign(self, glob.ty, name_doc, codegen_const_value(value));
                    self.consts.push(doc);
                }
                GlobalKind::Extern => {
                    let doc = stmt(|| {
                        D::text("extern").append(D::space()).append(glob.ty.cdecl(self, name_doc))
                    });
                    self.globals.push(doc);
                }
                GlobalKind::Static(StaticGlobal { body, result }) => {
                    let decl_doc = stmt(|| glob.ty.cdecl(self, name_doc));
                    self.globals.push(decl_doc);

                    let mut state = GenState::new(glob.name, body);

                    let return_stmt = stmt(|| {
                        D::text("return").append(D::space()).append(self.value(&state, *result))
                    });

                    let block_docs: Vec<D> = body
                        .blocks()
                        .iter()
                        .map(|b| self.codegen_block(&mut state, b))
                        .chain(iter::once(return_stmt))
                        .collect();

                    let init_fn_doc = D::text("FORCE_INLINE")
                        .append(D::space())
                        .append(glob.ty.cdecl(self, D::text(global_init_fn_name(glob))))
                        .append(D::text("()"))
                        .append(D::space())
                        .append(block_(
                            || {
                                D::intersperse(block_docs, D::hardline().append(D::hardline()))
                                    .group()
                            },
                            0,
                        ));

                    self.globals.push(stmt(|| init_fn_doc));
                }
            };
        }
    }

    pub fn get_or_create_adt(&mut self, ty: Ty, adt_id: AdtId, targs: &[Ty]) -> Ustr {
        if let Some(name) = self.adt_names.get(&ty) {
            *name
        } else {
            self.codegen_adt(ty, adt_id, targs);
            self.adt_names[&ty]
        }
    }

    fn codegen_adt(&mut self, ty: Ty, adt_id: AdtId, targs: &[Ty]) {
        let adt = &self.db[adt_id];

        let adt_name = ustr(&mangle::mangle_adt(self.db, adt, targs));
        self.adt_names.insert(ty, adt_name);

        let instantiation = adt.instantiation(targs);

        match &adt.kind {
            AdtKind::Struct(struct_def) => {
                self.codegen_struct_def(adt_name, &struct_def.clone(), &instantiation);
            }
            AdtKind::Union(union_def) => {
                self.codegen_union_def(adt_name, &union_def.clone(), &instantiation);
            }
        }
    }

    fn codegen_struct_typedef(
        &mut self,
        name: D<'db>,
        fields: &[AdtField],
        instantiation: &Instantiation,
    ) -> D<'db> {
        let mut folder = instantiation.folder();

        stmt(|| {
            D::text("typedef struct")
                .append(D::space())
                .append(name.clone())
                .append(D::space())
                .append(block(|| {
                    util::stmts(
                        fields.iter().map(|f| {
                            folder.fold(f.ty).cdecl(self, D::text(f.name.name().as_str()))
                        }),
                    )
                }))
                .append(D::space())
                .append(name)
        })
    }

    fn codegen_struct_def(
        &mut self,
        adt_name: Ustr,
        struct_def: &StructDef,
        instantiation: &Instantiation,
    ) {
        match struct_def.kind {
            StructKind::Ref => {
                let data_name = D::text(format!("{adt_name}__data"));
                let data_tydef = self.codegen_struct_typedef(
                    data_name.clone(),
                    &struct_def.fields,
                    instantiation,
                );
                self.rc_types.push(data_tydef);
                self.codegen_rc_wrapper_tydef(D::text(adt_name.as_str()), data_name);
            }
            StructKind::Value => {
                let tydef = self.codegen_struct_typedef(
                    D::text(adt_name.as_str()),
                    &struct_def.fields,
                    instantiation,
                );
                self.types.push(tydef);
            }
        }
    }

    fn codegen_union_def(
        &mut self,
        adt_name: Ustr,
        union_def: &UnionDef,
        instantiation: &Instantiation,
    ) {
        match union_def.kind {
            UnionKind::Ref => {
                let data_name = D::text(format!("{adt_name}__data"));
                let data_tydef =
                    self.codegen_union_typedef(data_name.clone(), union_def, instantiation);
                self.rc_types.push(data_tydef);
                self.codegen_rc_wrapper_tydef(D::text(adt_name.as_str()), data_name);
            }
            UnionKind::Value => {
                let tydef = self.codegen_union_typedef(
                    D::text(adt_name.as_str()),
                    union_def,
                    instantiation,
                );
                self.types.push(tydef);
            }
        }
    }

    fn codegen_union_typedef(
        &mut self,
        adt_name: D<'db>,
        union_def: &UnionDef,
        instantiation: &Instantiation,
    ) -> D<'db> {
        let variants_union = stmt(|| {
            D::text("union").append(D::space()).append(block(|| {
                util::stmts(union_def.variants.iter().map(|variant_id| {
                    let variant = &self.db[*variant_id];
                    let name = variant_name(variant.name, variant.id);
                    let fields = variant.fields.clone();

                    D::text("struct")
                        .append(D::space())
                        .append(block(|| {
                            util::stmts(fields.iter().map(|f| {
                                instantiation
                                    .fold(f.ty)
                                    .cdecl(self, D::text(f.name.name().as_str()))
                            }))
                        }))
                        .append(D::space())
                        .append(D::text(name))
                }))
            }))
        });

        stmt(|| {
            D::text("typedef struct")
                .append(D::space())
                .append(adt_name.clone())
                .append(D::space())
                .append(block(|| {
                    let tag = stmt(|| D::text("u8").append(D::space()).append(D::text(TAG_FIELD)));

                    D::intersperse([tag, variants_union], D::hardline())
                }))
                .append(D::space())
                .append(adt_name)
        })
    }

    fn codegen_rc_wrapper_tydef(&mut self, name: D<'db>, data_name: D<'db>) {
        let tydef = D::text("typedef")
            .append(D::space())
            .append(D::text("struct"))
            .append(D::space())
            .append(name.clone())
            .append(D::space())
            .append(block(|| {
                util::stmts([
                    D::text("u32").append(D::space()).append(D::text(REFCNT_FIELD)),
                    data_name.append(D::space()).append(D::text(DATA_FIELD)),
                ])
            }))
            .append(D::space())
            .append(name);

        self.rc_types.push(stmt(|| tydef));
    }

    fn codegen_fn_sig(&mut self, sig: &FnSig) -> D<'db> {
        let fn_ty = sig.ty.as_fn().expect("a function type");

        let initial =
            if fn_ty.is_extern() { D::text("extern").append(D::space()) } else { D::nil() };

        let mut params = vec![];

        if fn_ty.callconv == CallConv::Jin {
            params.push(D::text("jinrt_backtrace *backtrace"));
        }

        params.extend(sig.params.iter().enumerate().map(|(idx, param)| {
            let name = param_name(&param.pat, idx);
            param.ty.cdecl(self, D::text(name))
        }));

        let sig_doc =
            fn_ty.ret.cdecl(self, D::text(sig.mangled_name.as_str())).append(util::group(
                D::intersperse(params, D::text(",").append(D::space()))
                    .nest(2)
                    .group()
                    .append(if fn_ty.is_c_variadic() { D::text(", ...") } else { D::nil() }),
            ));

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

        let mut state = GenState::new(sig.display_name, &fun.body);

        for (idx, param) in sig.params.iter().enumerate() {
            let name = param_name(&param.pat, idx);
            state.param_names.push(ustr(&name));
        }

        let block_docs: Vec<D> =
            fun.body.blocks().iter().map(|b| self.codegen_block(&mut state, b)).collect();

        let sig_doc = {
            let inline =
                if sig.is_inline { D::text("inline").append(D::space()) } else { D::nil() };
            inline.append(self.codegen_fn_sig(sig))
        };

        sig_doc.append(D::space()).append(block_(
            || D::intersperse(block_docs, D::hardline().append(D::hardline())).group(),
            0,
        ))
    }

    fn codegen_block(&mut self, state: &mut GenState<'db>, block: &'db Block) -> D<'db> {
        D::text(block.display_name())
            .append(D::text(":;"))
            .append(D::hardline())
            .append(self.codegen_insts(state, &block.insts))
            .nest(NEST)
            .group()
    }

    fn codegen_insts(&mut self, state: &mut GenState<'db>, insts: &'db [Inst]) -> D<'db> {
        let mut docs = vec![];

        for inst in insts {
            docs.push(self.codegen_inst(state, inst));

            if let Inst::Return { .. } = inst {
                // There's no need to continue lowering instructions after a return instruction
                break;
            }
        }

        D::intersperse(docs, D::hardline()).group()
    }

    #[allow(clippy::too_many_lines)]
    fn codegen_inst(&mut self, state: &mut GenState<'db>, inst: &'db Inst) -> D<'db> {
        match inst {
            Inst::StackAlloc { value, init } => self.codegen_inst_stackalloc(state, *value, *init),
            Inst::Alloc { value } => self.alloc_value(state, *value),
            Inst::SliceAlloc { value, cap } => self.alloc_slice(state, *value, *cap),
            Inst::SliceIndex { value, slice, index, span } => {
                let slice_index = self
                    .value_assign(state, *value, |this| this.slice_index(state, *slice, *index));

                self.maybe_slice_index_boundscheck(state, *slice, *index, slice_index, *span)
            }
            Inst::SliceSlice { value, slice, low, high, span } => {
                self.value_assign(state, *value, |this| {
                    util::call(
                        D::text("jinrt_slice_slice"),
                        [
                            D::text("backtrace"),
                            this.value(state, *slice),
                            this.sizeof_slice_elem(state, *slice),
                            this.value(state, *low),
                            this.value(state, *high),
                            this.create_stackframe_value(state, *span),
                        ],
                    )
                })
            }
            Inst::SliceStore { slice, index, value, span } => {
                let slice_index = self.slice_index(state, *slice, *index);
                let slice_store = stmt(|| util::assign(slice_index, self.value(state, *value)));

                self.maybe_slice_index_boundscheck(state, *slice, *index, slice_store, *span)
            }
            Inst::Store { value, target } => {
                stmt(|| assign(self.value(state, *target), self.value(state, *value)))
            }
            Inst::Free { value, traced, span } => self.free(state, *value, *traced, *span),
            Inst::IncRef { value } => {
                let value_doc = self.value(state, *value);

                stmt(|| {
                    if state.value_is_slice(*value) {
                        util::call(D::text("jinrt_slice_incref"), [value_doc])
                    } else {
                        util::field(value_doc, REFCNT_FIELD, true).append(" += 1")
                    }
                })
            }
            Inst::DecRef { value } => {
                let value_doc = self.value(state, *value);

                stmt(|| {
                    if state.value_is_slice(*value) {
                        util::call(D::text("jinrt_slice_decref"), [value_doc])
                    } else {
                        util::field(value_doc, REFCNT_FIELD, true).append(" -= 1")
                    }
                })
            }
            Inst::Br { target } => goto_stmt(state.body.block(*target)),
            Inst::BrIf { cond, then, otherwise } => util::if_stmt(
                self.value(state, *cond),
                goto_stmt(state.body.block(*then)),
                otherwise.map(|o| goto_stmt(state.body.block(o))),
            ),
            Inst::Switch { cond, blocks } => util::switch_stmt(
                self.value(state, *cond),
                blocks
                    .iter()
                    .enumerate()
                    .map(|(idx, &b)| (D::text(idx.to_string()), goto_stmt(state.body.block(b)))),
            ),
            Inst::Return { value } => {
                stmt(|| D::text("return").append(D::space()).append(self.value(state, *value)))
            }
            Inst::Call { value, callee, args, span } => {
                let mut arg_docs = vec![];

                let traced = state.ty_of(*callee).as_fn().unwrap().callconv == CallConv::Jin;

                if traced {
                    arg_docs.push(D::text("backtrace"));
                }

                arg_docs.extend(args.iter().copied().map(|a| self.value(state, a)));

                let call = self.value_assign(state, *value, |this| {
                    util::call(this.value(state, *callee), arg_docs)
                });

                if traced {
                    self.with_stack_frame(state, call, *span)
                } else {
                    call
                }
            }
            Inst::RtCall { value, kind, span } => {
                let traced = kind.traced();

                let mut args = vec![];

                if traced {
                    args.push(D::text("backtrace"));
                }

                match kind {
                    RtCallKind::SliceGrow { slice, new_cap } => {
                        args.push(self.value(state, *slice));
                        args.push(self.sizeof_slice_elem(state, *slice));
                        args.push(self.value(state, *new_cap));
                    }
                }

                if traced {
                    args.push(self.create_stackframe_value(state, *span));
                }

                self.value_assign(state, *value, |_| util::call(D::text(kind.as_str()), args))
            }
            Inst::Binary { value, lhs, rhs, op, span } => self.codegen_bin_op(
                state,
                &BinOpData {
                    target: *value,
                    lhs: *lhs,
                    rhs: *rhs,
                    op: *op,
                    ty: state.ty_of(*lhs),
                    span: *span,
                },
            ),
            Inst::Unary { value, inner, op } => {
                let inner_ty = state.ty_of(*inner);
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

                self.value_assign(state, *value, |this| {
                    D::text(op_str).append(this.value(state, *inner))
                })
            }
            Inst::Convert { value, source, target, span } => {
                self.codegen_convert(state, *value, *source, *target, *span)
            }
            Inst::Cast { value, source, target, .. } => {
                self.value_assign(state, *value, |this| {
                    let target_doc = target.cty(this).append(D::text("*"));
                    let source_doc = util::addr(this.value(state, *source));
                    let cast = util::cast(target_doc, source_doc);
                    util::deref(cast)
                })
            }
            Inst::StrLit { value, lit } => self.value_assign(state, *value, |_| str_value(lit)),
            Inst::Destroy { .. } => unreachable!(),
        }
    }

    fn codegen_inst_stackalloc(
        &mut self,
        state: &mut GenState<'db>,
        value: ValueId,
        init: Option<ValueId>,
    ) -> D<'db> {
        let value = state.body.value(value);

        let name = match &value.kind {
            ValueKind::Register(name) => Self::register_name(value.id, *name),
            ValueKind::Local(id) => {
                state.local_names.insert_unique(*id, self.db[*id].name).to_string()
            }
            kind => unreachable!("{kind:?}"),
        };

        match init {
            Some(init_value) => {
                let doc = self.value(state, init_value);
                VariableDoc::assign(self, value.ty, D::text(name), doc)
            }
            None => VariableDoc::decl(self, value.ty, D::text(name)),
        }
    }

    pub fn value_assign(
        &mut self,
        state: &GenState<'db>,
        id: ValueId,
        f: impl FnOnce(&mut Self) -> D<'db>,
    ) -> D<'db> {
        let assigned = self.value(state, id);
        let doc = f(self);
        VariableDoc::assign(self, state.ty_of(id), assigned, doc)
    }

    pub fn value(&mut self, state: &GenState<'db>, id: ValueId) -> D<'db> {
        match &state.body.value(id).kind {
            ValueKind::Register(name) => D::text(Self::register_name(id, *name)),
            ValueKind::Param(_, idx) => D::text(state.param_names[*idx].as_str()),
            ValueKind::Local(id) => D::text(state.local_names.get(*id).unwrap().as_str()),
            ValueKind::Global(id) => D::text(self.mir.globals[*id].name.as_str()),
            ValueKind::Fn(id) => {
                let sig = &self.mir.fn_sigs[*id];
                D::text(sig.mangled_name.as_str())
            }
            ValueKind::Const(value) => codegen_const_value(value),
            ValueKind::Field(value, field) => self.field(state, *value, field),
            ValueKind::Variant(value, id) => self.variant_field(state, *value, *id),
            ValueKind::Deref(value) => util::deref(self.value(state, *value)),
        }
    }

    fn register_name(value: ValueId, name: Option<Ustr>) -> String {
        if let Some(name) = name {
            format!("{}${}", name, value)
        } else {
            format!("v{}", value)
        }
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
    pub fn decl(cx: &mut Generator<'a>, ty: Ty, name: D<'a>) -> D<'a> {
        Self { ty, name, value: None }.into_doc(cx)
    }

    pub fn assign(cx: &mut Generator<'a>, ty: Ty, name: D<'a>, value: D<'a>) -> D<'a> {
        Self { ty, name, value: Some(value) }.into_doc(cx)
    }

    fn into_doc(self, cx: &mut Generator<'a>) -> D<'a> {
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

fn param_name(pat: &Pat, index: usize) -> String {
    match pat {
        Pat::Name(name) => format!("{}$p{}", name.word, index),
        Pat::Discard(_) => format!("${index}"),
    }
}

pub fn variant_name(name: Word, id: VariantId) -> String {
    format!("{}${}", name, id.data())
}
