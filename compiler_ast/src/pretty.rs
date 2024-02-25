use std::io;

use compiler_core::{
    db::UnionKind,
    middle::{BinOp, IsUfcs},
    word::Word,
};

use crate::{
    CallArg, CharKind, Expr, ExternImport, ExternLet, Fn, FnKind, FnParam, FnSig, Import,
    ImportTree, Item, Let, Module, TyDef, TyDefKind, TyExpr,
};

pub(super) fn print_module(module: &Module, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PrettyCx { builder: ptree::TreeBuilder::new(module.name.join()) };

    for item in &module.items {
        item.pretty_print(&mut cx);
    }

    let tree = cx.builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PrettyCx {
    builder: ptree::TreeBuilder,
}

trait PrettyPrint {
    fn pretty_print(&self, cx: &mut PrettyCx);
}

impl PrettyPrint for Expr {
    #[allow(clippy::too_many_lines)]
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            Self::Let(let_) => let_.pretty_print(cx),
            Self::Fn { params, ret, body, .. } => {
                cx.builder.begin_child("fn expr".to_string());

                print_fn_params(cx, params);
                print_fn_ret(cx, ret.as_ref());

                body.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Assign { lhs, rhs, op, .. } => {
                cx.builder.begin_child(format!("{}=", op.map(BinOp::as_str).unwrap_or_default()));
                lhs.pretty_print(cx);
                cx.builder.add_empty_child("to".to_string());
                rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Swap { lhs, rhs, .. } => {
                cx.builder.begin_child(":=".to_string());
                lhs.pretty_print(cx);
                cx.builder.add_empty_child("and".to_string());
                rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Return { expr, .. } => {
                cx.builder.begin_child("return".to_string());

                if let Some(expr) = expr {
                    expr.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::If { cond, then, otherwise, .. } => {
                cx.builder.begin_child("if".to_string());

                cx.builder.begin_child("cond".to_string());
                cond.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.begin_child("then".to_string());
                then.pretty_print(cx);
                cx.builder.end_child();

                if let Some(otherwise) = otherwise {
                    cx.builder.begin_child("else".to_string());
                    otherwise.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            Self::Match { expr, arms: _, .. } => {
                cx.builder.begin_child("match".to_string());

                cx.builder.begin_child("expr".to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.begin_child("cases".to_string());
                cx.builder.add_empty_child("TODO: write cases".to_string());
                cx.builder.end_child();

                cx.builder.end_child();
            }
            Self::Loop { cond, expr, .. } => {
                cx.builder.begin_child("loop".to_string());

                if let Some(cond) = cond {
                    cx.builder.begin_child("cond".to_string());
                    cond.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.begin_child("expr".to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.end_child();
            }
            Self::Break { .. } => {
                cx.builder.add_empty_child("break".to_string());
            }
            Self::Block { exprs, .. } => {
                cx.builder.begin_child("block".to_string());

                for expr in exprs {
                    expr.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::Unsafe { expr, .. } => {
                cx.builder.begin_child("unsafe".to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Call { callee, args, .. } => {
                print_call(cx, callee, args, None, None);
            }
            Self::MethodCall { expr, method, targs: ty_args, args, .. } => {
                print_call(cx, expr, args, ty_args.as_deref(), Some(*method));
            }
            Self::Unary { expr, op, .. } => {
                cx.builder.begin_child(op.to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Binary { lhs, rhs, op, .. } => {
                cx.builder.begin_child(op.to_string());
                lhs.pretty_print(cx);
                rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Cast { expr, target, .. } => {
                cx.builder.begin_child("cast".to_string());
                expr.pretty_print(cx);
                cx.builder.begin_child("to".to_string());
                target.pretty_print(cx);
                cx.builder.end_child();
                cx.builder.end_child();
            }
            Self::Field { expr, field, .. } => {
                cx.builder.begin_child(format!("field `{field}`"));
                expr.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Index { expr, index, .. } => {
                cx.builder.begin_child("index".to_string());
                expr.pretty_print(cx);
                index.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Slice { expr, low, high, .. } => {
                cx.builder.begin_child("slice".to_string());
                expr.pretty_print(cx);

                if let Some(low) = low {
                    low.pretty_print(cx);
                }

                if let Some(high) = high {
                    high.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::Name { word, .. } => {
                cx.builder.add_empty_child(format!("`{word}`"));
            }
            Self::SliceLit { exprs, .. } => {
                cx.builder.begin_child("slice lit".to_string());

                for expr in exprs {
                    expr.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::SliceLitCap { cap, .. } => {
                cx.builder.begin_child("slice lit w/ cap".to_string());
                cap.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::BoolLit { value, .. } => {
                cx.builder.add_empty_child(format!("bool: {value}"));
            }
            Self::IntLit { value, .. } => {
                cx.builder.add_empty_child(format!("int: {value}"));
            }
            Self::FloatLit { value, .. } => {
                cx.builder.add_empty_child(format!("float: {value}"));
            }
            Self::StrLit { value, .. } => {
                cx.builder.add_empty_child(format!("str: {value}"));
            }
            Self::CharLit { value, kind, .. } => match kind {
                CharKind::Char => {
                    cx.builder.add_empty_child(format!("char: {value}"));
                }
                CharKind::Byte => {
                    cx.builder.add_empty_child(format!("byte char: {value}"));
                }
            },
            Self::UnitLit { .. } => {
                cx.builder.add_empty_child("unit".to_string());
            }
        }
    }
}

impl PrettyPrint for Item {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            Self::Fn(x) => x.pretty_print(cx),
            Self::Let(x) => x.pretty_print(cx),
            Self::Type(x) => x.pretty_print(cx),
            Self::Import(x) => x.pretty_print(cx),
            Self::ExternLet(x) => x.pretty_print(cx),
            Self::ExternImport(x) => x.pretty_print(cx),
            Self::Assoc(tyname, item) => {
                cx.builder.begin_child(format!("associated item to `{tyname}`"));
                item.pretty_print(cx);
                cx.builder.end_child();
            }
        }
    }
}

impl PrettyPrint for Fn {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("fn {}", self.sig.word));
        self.sig.pretty_print(cx);

        match &self.kind {
            FnKind::Bare { body } => body.pretty_print(cx),
            FnKind::Extern { callconv, is_c_variadic } => {
                cx.builder.add_empty_child(format!(
                    "extern \"{}\" {}",
                    callconv,
                    if *is_c_variadic { "(variadic)" } else { "" }
                ));
            }
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for FnSig {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        print_fn_params(cx, &self.params);
        print_fn_ret(cx, self.ret.as_ref());
    }
}

impl PrettyPrint for Let {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("let {}", self.pat));

        if let Some(ty) = &self.ty_expr {
            cx.builder.begin_child("type".to_string());
            ty.pretty_print(cx);
            cx.builder.end_child();
        }

        self.value.pretty_print(cx);

        cx.builder.end_child();
    }
}

impl PrettyPrint for TyDef {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("type {}", self.word));

        match &self.kind {
            TyDefKind::Struct(sdef) => {
                cx.builder.add_empty_child("struct".to_string());
                cx.builder.begin_child("fields".to_string());

                for field in &sdef.fields {
                    cx.builder.begin_child(field.name.to_string());
                    field.ty_expr.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            TyDefKind::Union(udef) => {
                cx.builder.add_empty_child(format!(
                    "{}union",
                    match udef.kind {
                        UnionKind::Ref => "ref ",
                        UnionKind::Value => "",
                    }
                ));

                for variant in &udef.variants {
                    cx.builder.begin_child(variant.name.to_string());

                    for field in &variant.fields {
                        cx.builder.begin_child(field.name.to_string());
                        field.ty_expr.pretty_print(cx);
                        cx.builder.end_child();
                    }

                    cx.builder.end_child();
                }
            }
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for Import {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child("import".to_string());
        self.tree.pretty_print(cx);
        cx.builder.end_child();
    }
}

impl PrettyPrint for ImportTree {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            ImportTree::Group(imports) => {
                for import in imports {
                    import.pretty_print(cx);
                }
            }
            ImportTree::Path(name, next) => {
                cx.builder.add_empty_child(name.to_string());
                next.pretty_print(cx);
            }
            ImportTree::Name(name, alias) => {
                cx.builder.add_empty_child(format!(
                    "{}{}",
                    name,
                    if let Some(alias) = alias { format!(" as {alias}") } else { String::new() }
                ));
            }
            ImportTree::Glob(is_ufcs, _) => {
                cx.builder
                    .add_empty_child(if *is_ufcs == IsUfcs::Yes { "?" } else { "*" }.to_string());
            }
        }
    }
}

impl PrettyPrint for ExternLet {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("let extern {}", self.word));

        cx.builder.begin_child("type".to_string());
        self.ty_expr.pretty_print(cx);
        cx.builder.end_child();

        cx.builder.end_child();
    }
}

impl PrettyPrint for ExternImport {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.add_empty_child(format!("import extern \"{}\"", self.lib));
    }
}

impl PrettyPrint for TyExpr {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            TyExpr::Fn(f) => {
                cx.builder.begin_child("fn type".to_string());

                if !f.params.is_empty() {
                    cx.builder.begin_child("params".to_string());

                    for param in &f.params {
                        param.pretty_print(cx);
                    }

                    cx.builder.end_child();
                }

                if let Some(ret) = &f.ret {
                    cx.builder.begin_child("ret".to_string());
                    ret.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            TyExpr::Slice(inner, _) => {
                cx.builder.begin_child("slice".to_string());
                inner.pretty_print(cx);
                cx.builder.end_child();
            }
            TyExpr::Ref(inner, mutability, _) => {
                cx.builder.begin_child(format!("ref ({mutability})"));
                inner.pretty_print(cx);
                cx.builder.end_child();
            }
            TyExpr::RawPtr(pointee, _) => {
                cx.builder.begin_child("raw ptr".to_string());
                pointee.pretty_print(cx);
                cx.builder.end_child();
            }
            TyExpr::Path(path, targs, _) => {
                cx.builder.begin_child(format!(
                    "path: {}",
                    path.iter().map(ToString::to_string).collect::<Vec<_>>().join(".")
                ));

                if let Some(targs) = targs {
                    cx.builder.begin_child("type args".to_string());
                    for arg in targs {
                        arg.pretty_print(cx);
                    }
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            TyExpr::Unit(_) => {
                cx.builder.add_empty_child("unit".to_string());
            }
            TyExpr::Hole(_) => {
                cx.builder.add_empty_child("_".to_string());
            }
        }
    }
}

fn print_call(
    cx: &mut PrettyCx,
    expr: &Expr,
    args: &[CallArg],
    ty_args: Option<&[TyExpr]>,
    method: Option<Word>,
) {
    cx.builder.begin_child(if let Some(m) = method {
        format!("method call `{m}`")
    } else {
        "call".to_string()
    });
    expr.pretty_print(cx);

    if let Some(ty_args) = ty_args {
        cx.builder.begin_child("type args".to_string());
        for arg in ty_args {
            arg.pretty_print(cx);
        }
        cx.builder.end_child();
    }

    if !args.is_empty() {
        cx.builder.begin_child("args".to_string());
        for arg in args {
            match arg {
                CallArg::Positional(expr) => expr.pretty_print(cx),
                CallArg::Named(name, expr) => {
                    cx.builder.begin_child(name.to_string());
                    expr.pretty_print(cx);
                    cx.builder.end_child();
                }
            }
        }
        cx.builder.end_child();
    }

    cx.builder.end_child();
}

fn print_fn_params(cx: &mut PrettyCx, params: &[FnParam]) {
    if !params.is_empty() {
        cx.builder.begin_child("params".to_string());

        for param in params {
            cx.builder.add_empty_child(format!("{}", param.pat));
            if let Some(ty) = &param.ty_expr {
                ty.pretty_print(cx);
            }
        }

        cx.builder.end_child();
    }
}

fn print_fn_ret(cx: &mut PrettyCx, ret: Option<&TyExpr>) {
    if let Some(ret) = ret {
        cx.builder.begin_child("ret".to_string());
        ret.pretty_print(cx);
        cx.builder.end_child();
    }
}
