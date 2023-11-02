mod generate;
#[cfg(windows)]
mod microsoft_craziness;
mod ty;
mod util;

use std::process::Command;

use camino::{Utf8Path, Utf8PathBuf};
use execute::Execute;
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    OptimizationLevel,
};
use pretty::RcDoc;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::UstrMap;

use crate::{
    cgen::generate::Generator,
    db::{Db, ExternLib},
    mir::Mir,
    target::{Arch, Os, TargetMetrics},
};

pub fn codegen(db: &mut Db, mir: &Mir) -> Utf8PathBuf {
    db.time.start("cgen");
    let c_output_path =
        Generator { db, mir, fn_decls: vec![], globals: vec![], fn_defs: vec![] }.run();
    db.time.stop();

    todo!()
    // build_exe(db, &target_machine, &module)
}

// fn build_exe(db: &mut Db, target_machine: &TargetMachine, module: &Module) -> Utf8PathBuf {
//     let output_path = db.output_path();
//
//     if db.build_options().should_emit(EmitOption::LlvmIr) {
//         module.print_to_file(output_path.with_extension("ll")).expect("printing llvm ir to work");
//     }
//
//     let (object_file, output_file) = if db.target_metrics().os == Os::Windows {
//         (output_path.with_extension("obj"), output_path.with_extension("exe"))
//     } else {
//         (output_path.with_extension("o"), output_path.with_extension(""))
//     };
//
//     db.time.start("link");
//     target_machine
//         .write_to_file(module, FileType::Object, object_file.as_std_path())
//         .expect("writing the object file to work");
//
//     link(db.target_metrics(), &db.extern_libs, &output_file, &object_file);
//     db.time.stop();
//
//     let _ = std::fs::remove_file(object_file);
//
//     output_file
// }
//
// fn link(
//     target_metrics: &TargetMetrics,
//     extern_libs: &FxHashSet<ExternLib>,
//     exe_file: &Utf8Path,
//     object_file: &Utf8Path,
// ) {
//     let link_flags = match target_metrics.arch {
//         Arch::Amd64 => match target_metrics.os {
//             Os::Windows => vec!["/machine:x64"],
//             Os::Linux | Os::FreeBSD => vec!["-arch x86-64"],
//             _ => vec![],
//         },
//         Arch::_386 => match target_metrics.os {
//             Os::Windows => vec!["/machine:x86"],
//             Os::Darwin => panic!("unsupported architecture"),
//             Os::Linux | Os::FreeBSD => vec!["-arch x86"],
//             _ => vec![],
//         },
//         Arch::Arm64 => match target_metrics.os {
//             Os::Darwin => vec!["-arch arm64"],
//             Os::Linux => vec!["-arch aarch64"],
//             _ => vec![],
//         },
//         Arch::Wasm32 | Arch::Wasm64 => {
//             let mut link_flags = vec!["--allow-undefined"];
//
//             if matches!(target_metrics.arch, Arch::Wasm64) {
//                 link_flags.push("-mwas64");
//             }
//
//             if matches!(target_metrics.os, Os::Freestanding) {
//                 link_flags.push("--no-entry");
//             }
//
//             link_flags
//         }
//     };
//
//     let mut lib_paths = FxHashSet::<String>::default();
//     let mut libs = FxHashSet::<String>::default();
//
//     for lib in extern_libs {
//         match lib {
//             ExternLib::Sys(lib_name) => {
//                 libs.insert(lib_name.clone());
//             }
//             ExternLib::Path { search_path, name } => {
//                 lib_paths.insert(search_path.to_string());
//                 libs.insert(name.clone());
//             }
//         }
//     }
//
//     #[cfg(windows)]
//     {
//         let find_result = unsafe { microsoft_craziness::find_visual_studio_and_windows_sdk() };
//
//         if let Some(path) = &find_result.windows_sdk_ucrt_library_path {
//             lib_paths.push(path.to_string().unwrap());
//         }
//
//         if let Some(path) = &find_result.windows_sdk_um_library_path {
//             lib_paths.push(path.to_string().unwrap());
//         }
//
//         if let Some(path) = &find_result.vs_library_path {
//             lib_paths.push(path.to_string().unwrap());
//         }
//
//         Command::new("lld-link")
//             .arg(format!("/out:{}", executable_file.to_str().unwrap()))
//             .arg("/entry:mainCRTStartup")
//             .arg("/defaultlib:libcmt")
//             .arg("/nologo")
//             .arg("/incremental:no")
//             .arg("/opt:ref")
//             .arg("/threads:8")
//             .arg("/subsystem:CONSOLE")
//             .args(lib_paths.iter().map(|path| format!("/libpath:{}", path)))
//             .arg(object_file.to_str().unwrap())
//             .args(libs)
//             .args(link_flags)
//             .execute_output()
//             .expect("linking to work");
//     }
//
//     #[cfg(not(windows))]
//     Command::new("clang")
//         .arg("-Wno-unused-command-line-argument")
//         .arg(object_file.as_str())
//         .arg(format!("-o{exe_file}"))
//         .args(lib_paths.iter().map(|path| format!("-L{}", path)))
//         .args(libs.iter().map(|path| format!("-l{}", path)))
//         .arg("-fuse-ld=mold")
//         .arg("-lc")
//         .arg("-lm")
//         .arg("-no-pie")
//         .args(link_flags)
//         .execute_output()
//         .expect("linking to work");
// }
