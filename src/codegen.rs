use crate::{ast::*, ty::*};

pub fn codegen(module: Module) -> String {
    let mut cg = Codegen {
        prelude: String::new(),
        declarations: String::new(),
        definitions: String::new(),
        indent: 0,
    };

    cg.gen(module);

    format!(
        "{}\n\n{}\n\n{}",
        cg.prelude, cg.declarations, cg.definitions
    )
}

struct Codegen {
    prelude: String,
    declarations: String,
    definitions: String,
    indent: usize,
}

impl Codegen {
    fn gen(&mut self, module: Module) {
        self.prelude.push_str(
            r#"#include <stdint.h>
typedef void never;"#,
        );

        self.definitions.push_str(
            r#"int main() {
    main_main();
    return 0;
}

"#,
        );

        for binding in &module.bindings {
            let definitions = self.visit_binding(binding);
            self.definitions.push_str(&definitions);
        }
    }

    fn push_declaration(&mut self, decl: &str) {
        self.declarations.reserve(decl.len() + 1);
        self.declarations.push_str(decl);
        self.declarations.push(';');
    }

    // fn push_definition(&mut self, def: &str) {
    //     const SUFFIX: &str = "\n\n";
    //     self.definitions.reserve(def.len() + SUFFIX.len());
    //     self.definitions.push_str(def);
    //     self.definitions.push_str(SUFFIX);
    // }
}

impl AstVisitor<String> for Codegen {
    fn visit_binding(&mut self, binding: &Binding) -> String {
        match &binding.kind {
            BindingKind::Fun { name, fun } => {
                let decl = format!(
                    "{} {}()",
                    c_type(fun.ty.as_ref().unwrap().kind.as_fun().unwrap().ret.as_ref()),
                    name
                );

                self.push_declaration(&decl);

                let body = self.visit(&fun.body);
                let body_str = format!("\t{body};\n");

                format!("{decl} {{\n{body_str}}}")
            }
        }
    }

    fn visit_fun(&mut self, fun: &Fun) -> String {
        todo!("anonymous functions")
        // let decl = format!(
        //     "{} {}()",
        //     c_type(fun.ty.as_ref().unwrap().as_fun().ret.as_ref()),
        //     fun.name
        // );
        //
        // self.add_declaration(&decl);
        //
        // let body = self.visit(&fun.body);
        // let body_str = format!("\t{body};\n");
        //
        // format!("{decl} {{\n{body_str}}}\n\n")
    }

    fn visit_ret(&mut self, ret: &Ret) -> String {
        let value = if let Some(value) = ret.value.as_ref() {
            self.visit(value)
        } else {
            "".to_string()
        };

        format!("return {}", value)
    }

    fn visit_lit(&mut self, lit: &Lit) -> String {
        match lit.kind {
            LitKind::Int(value) => value.to_string(),
        }
    }
}

fn c_type(ty: &Ty) -> String {
    match &ty.kind {
        TyKind::Int(int) => match int {
            IntTy::Int => "intptr_t",
        }
        .to_string(),
        TyKind::Fun(_) => todo!(),
        TyKind::Unit | TyKind::Never => "void".to_string(),
        TyKind::Var(_) => panic!("unexpected type: {ty}"),
    }
}
