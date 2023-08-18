#[cfg(windows)]
mod microsoft_craziness;

use std::{collections::HashMap, path::PathBuf};

use inkwell::{
    context::Context,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    OptimizationLevel,
};

use crate::{
    common::{target::Arch, time::time},
    db::Database,
    mir::Mir,
};

pub fn codegen(db: &Database, mir: &Mir) -> PathBuf {
    let context = Context::create();
    let module =
        context.create_module(db.main_source().path().file_stem().unwrap().to_str().unwrap());
    let builder = context.create_builder();

    let target_metrics = db.build_options().target_metrics;

    match &target_metrics.arch {
        Arch::Amd64 | Arch::_386 => Target::initialize_x86(&InitializationConfig::default()),
        Arch::Arm64 => Target::initialize_aarch64(&InitializationConfig::default()),
        Arch::Wasm32 | Arch::Wasm64 => {
            Target::initialize_webassembly(&InitializationConfig::default())
        }
    }

    let triple = TargetTriple::create(target_metrics.target_triplet);
    let target = Target::from_triple(&triple).unwrap();
    let host_cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();

    let target_machine = target
        .create_target_machine(
            &triple,
            host_cpu.to_str().unwrap(),
            features.to_str().unwrap(),
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("to create a LLVM TargetMachine");

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&target_machine.get_triple());

    let mut cx = Generator {
        db,
        mir,
        context: &context,
        module: &module,
        builder: &builder,
        ptr_sized_int_type: context.ptr_sized_int_type(&target_machine.get_target_data(), None),
        functions: HashMap::default(),
    };

    let print_times = db.build_options().print_times;

    time(print_times, "emit llvm ir", || cx.run());

    if let Err(e) = cx.module.verify() {
        cx.module.print_to_file("fail.ll").unwrap();
        panic!("{}", e);
    }

    time(print_times, "llvm opt", || cx.optimize());

    todo!()

    // build_executable(
    //     &workspace.build_options,
    //     &target_machine,
    //     &target_metrics,
    //     &module,
    //     &cg.extern_libraries,
    // )
}

// fn build_executable(
//     build_options: &BuildOptions,
//     target_machine: &TargetMachine,
//     target_metrics: &TargetMetrics,
//     module: &Module,
//     extern_libraries: &HashSet<ast::ExternLibrary>,
// ) -> PathBuf {
//     let output_path = build_options.output_file.as_ref().unwrap_or(&build_options.source_file);
//
//     if let Some(parent_dir) = output_path.parent() {
//         let _ = std::fs::create_dir_all(parent_dir);
//     }
//
//     if build_options.codegen_options.emit_llvm_ir() {
//         module.print_to_file(output_path.with_extension("ll")).unwrap();
//     }
//
//     let object_file = if target_metrics.os == Os::Windows {
//         output_path.with_extension("obj")
//     } else {
//         output_path.with_extension("o")
//     };
//
//     let output_file = if target_metrics.os == Os::Windows {
//         output_path.with_extension("exe")
//     } else {
//         output_path.with_extension("")
//     };
//
//     time! { build_options.emit_times, "write obj",
//         target_machine
//             .write_to_file(module, FileType::Object, &object_file)
//             .unwrap()
//     };
//
//     time! { build_options.emit_times, "link",
//         link(target_metrics, &output_file, &object_file, extern_libraries)
//     }
//
//     let _ = std::fs::remove_file(object_file);
//
//     output_file.absolutize().unwrap().to_path_buf()
// }
//
// fn link(
//     target_metrics: &TargetMetrics,
//     executable_file: &Path,
//     object_file: &Path,
//     extern_libraries: &HashSet<ast::ExternLibrary>,
// ) {
//     let link_flags = match target_metrics.arch {
//         Arch::Amd64 => match target_metrics.os {
//             Os::Windows => vec!["/machine:x64"],
//             Os::Linux | Os::FreeBSD => vec!["-arch x86-64"],
//             _ => vec![],
//         },
//         Arch::_386 => match target_metrics.os {
//             Os::Windows => vec!["/machine:x86"],
//             Os::Darwin => panic!("unsupported architecture"), // TODO: this needs to be a proper diagnostic
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
//     let mut lib_paths = vec![];
//     let mut libs = vec![];
//
//     for lib in extern_libraries.iter() {
//         match lib {
//             ast::ExternLibrary::System(lib_name) => {
//                 if !is_libc(lib_name) {
//                     libs.push(lib_name.clone())
//                 }
//             }
//             ast::ExternLibrary::Path(path) => {
//                 lib_paths.push(path.lib_dir().to_str().unwrap().to_string());
//                 libs.push(path.lib_name().to_str().unwrap().to_string());
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
//             .unwrap();
//     }
//
//     #[cfg(not(windows))]
//     Command::new("clang")
//         .arg("-Wno-unused-command-line-argument")
//         .arg(object_file.to_str().unwrap())
//         .arg(format!("-o{}", executable_file.to_str().unwrap()))
//         .args(lib_paths.iter().map(|path| format!("-L{}", path)))
//         .arg("-lc")
//         .arg("-lm")
//         .args(libs.iter().map(|path| format!("-l:{}", path)))
//         .arg("-no-pie")
//         .args(link_flags)
//         .execute_output()
//         .unwrap();

fn is_libc(lib: &str) -> bool {
    lib.eq_ignore_ascii_case("c")
}

#[allow(dead_code)]
#[repr(u32)]
pub enum CallingConv {
    C = 0,
    Fast = 8,
    Cold = 9,
    Ghc = 10,
    HiPE = 11,
    WebKitJs = 12,
    AnyReg = 13,
    PreserveMost = 14,
    PreserveAll = 15,
    Swift = 16,
    CxxFastTls = 17,
    Tail = 18,
    CfguardCheck = 19,
    SwiftTail = 20,
    X86StdCall = 64,
    X86FastCall = 65,
    ArmApcs = 66,
    ArmAapcs = 67,
    ArmAapcsVfp = 68,
    Msp430Intr = 69,
    X86ThisCall = 70,
    PtxKernel = 71,
    PtxDevice = 72,
    SpirFunc = 75,
    SpirKernel = 76,
    IntelOclBi = 77,
    X86_64SysV = 78,
    Win64 = 79,
    X86VectorCall = 80,
    Hhvm = 81,
    HhvmC = 82,
    X86Intr = 83,
    AvrIntr = 84,
    AvrSignal = 85,
    AvrBuiltin = 86,
    AmdgpuVs = 87,
    AmdgpuGs = 88,
    AmdgpuPs = 89,
    AmdgpuCs = 90,
    AmdgpuKernel = 91,
    X86RegCall = 92,
    AmdgpuHs = 93,
    Msp430Builtin = 94,
    AmdgpuLs = 95,
    AmdgpuEs = 96,
    Aarch64VectorCall = 97,
    Aarch64SveVectorCall = 98,
    WasmEmscriptenInvoke = 99,
    AmdgpuGfx = 100,
    M68kIntr = 101,
    MaxID = 1023,
}
