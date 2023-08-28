mod generate;
mod inkwell_ext;
#[cfg(windows)]
mod microsoft_craziness;
mod ty;
mod util;

use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    process::Command,
};

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

use crate::{
    common::target::{Arch, Os, TargetMetrics},
    db::{build_options::EmitOption, Db},
    llvm::generate::Generator,
    mir::Mir,
};

pub fn codegen(db: &mut Db, mir: &Mir) -> PathBuf {
    let target_machine = create_target_machine(db).expect("to create a LLVM TargetMachine");

    let context = Context::create();

    let module = context.create_module(&db.main_source().file_name());

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&target_machine.get_triple());

    let builder = context.create_builder();

    let mut cx = Generator {
        db,
        mir,
        context: &context,
        module: &module,
        builder: &builder,
        isize_ty: context.ptr_sized_int_type(&target_machine.get_target_data(), None),
        symbol_values: HashMap::default(),
    };

    cx.db.timings.start("llvm generation");
    cx.run();
    cx.db.timings.stop();

    if let Err(e) = cx.module.verify() {
        cx.module.print_to_file("fail.ll").expect("printing llvm module to file to work");
        panic!("{}", e);
    }

    // db.timings.start(print_times, "llvm optimizations")
    // optimize(cx.module);
    // db.timings.stop();

    build_exe(db, &target_machine, &module)
}

fn create_target_machine(db: &Db) -> Option<TargetMachine> {
    let target_metrics = db.build_options().target_metrics;

    match &target_metrics.arch {
        Arch::Amd64 | Arch::_386 => Target::initialize_x86(&InitializationConfig::default()),
        Arch::Arm64 => Target::initialize_aarch64(&InitializationConfig::default()),
        Arch::Wasm32 | Arch::Wasm64 => {
            Target::initialize_webassembly(&InitializationConfig::default());
        }
    }

    let triple = TargetTriple::create(target_metrics.target_triplet);
    let target = Target::from_triple(&triple).expect("llvm triple to be valid");
    let host_cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();

    target.create_target_machine(
        &triple,
        host_cpu.to_str().expect("to have a valid host cpu"),
        features.to_str().expect("to have valid cpu features"),
        OptimizationLevel::Default,
        RelocMode::Default,
        CodeModel::Default,
    )
}

fn optimize(module: &Module) {
    let pass_manager_builder = PassManagerBuilder::create();

    pass_manager_builder.set_optimization_level(OptimizationLevel::Default);
    pass_manager_builder.set_size_level(1);

    let pass_manager = PassManager::create(());
    pass_manager_builder.populate_module_pass_manager(&pass_manager);
    pass_manager.run_on(module);
}

fn build_exe(db: &mut Db, target_machine: &TargetMachine, module: &Module) -> PathBuf {
    let output_path = db.output_path();

    if let Some(parent_dir) = output_path.parent() {
        let _ = fs::create_dir_all(parent_dir);
    }

    if db.build_options().should_emit(EmitOption::LlvmIr) {
        module.print_to_file(output_path.with_extension("ll")).expect("printing llvm ir to work");
    }

    let (object_file, output_file) = if db.build_options().target_metrics.os == Os::Windows {
        (output_path.with_extension("obj"), output_path.with_extension("exe"))
    } else {
        (output_path.with_extension("o"), output_path.with_extension(""))
    };

    db.timings.start("link");
    target_machine
        .write_to_file(module, FileType::Object, &object_file)
        .expect("writing the object file to work");

    link(&db.build_options().target_metrics, &output_file, &object_file);
    db.timings.stop();

    let _ = std::fs::remove_file(object_file);

    output_file
}

fn link(target_metrics: &TargetMetrics, exe_file: &Path, object_file: &Path) {
    let link_flags = match target_metrics.arch {
        Arch::Amd64 => match target_metrics.os {
            Os::Windows => vec!["/machine:x64"],
            Os::Linux | Os::FreeBSD => vec!["-arch x86-64"],
            _ => vec![],
        },
        Arch::_386 => match target_metrics.os {
            Os::Windows => vec!["/machine:x86"],
            Os::Darwin => panic!("unsupported architecture"),
            Os::Linux | Os::FreeBSD => vec!["-arch x86"],
            _ => vec![],
        },
        Arch::Arm64 => match target_metrics.os {
            Os::Darwin => vec!["-arch arm64"],
            Os::Linux => vec!["-arch aarch64"],
            _ => vec![],
        },
        Arch::Wasm32 | Arch::Wasm64 => {
            let mut link_flags = vec!["--allow-undefined"];

            if matches!(target_metrics.arch, Arch::Wasm64) {
                link_flags.push("-mwas64");
            }

            if matches!(target_metrics.os, Os::Freestanding) {
                link_flags.push("--no-entry");
            }

            link_flags
        }
    };

    let lib_paths = HashSet::<String>::new();
    let libs = HashSet::<String>::new();

    // TODO: externally linked libraries
    // for lib in extern_libraries.iter() {
    //     match lib {
    //         ast::ExternLibrary::System(lib_name) => {
    //             if !is_libc(lib_name) {
    //                 libs.push(lib_name.clone())
    //             }
    //         }
    //         ast::ExternLibrary::Path(path) => {
    //             lib_paths.push(path.lib_dir().to_str().unwrap().to_string());
    //             libs.push(path.lib_name().to_str().unwrap().to_string());
    //         }
    //     }
    // }

    #[cfg(windows)]
    {
        let find_result = unsafe { microsoft_craziness::find_visual_studio_and_windows_sdk() };

        if let Some(path) = &find_result.windows_sdk_ucrt_library_path {
            lib_paths.push(path.to_string().unwrap());
        }

        if let Some(path) = &find_result.windows_sdk_um_library_path {
            lib_paths.push(path.to_string().unwrap());
        }

        if let Some(path) = &find_result.vs_library_path {
            lib_paths.push(path.to_string().unwrap());
        }

        Command::new("lld-link")
            .arg(format!("/out:{}", executable_file.to_str().unwrap()))
            .arg("/entry:mainCRTStartup")
            .arg("/defaultlib:libcmt")
            .arg("/nologo")
            .arg("/incremental:no")
            .arg("/opt:ref")
            .arg("/threads:8")
            .arg("/subsystem:CONSOLE")
            .args(lib_paths.iter().map(|path| format!("/libpath:{}", path)))
            .arg(object_file.to_str().unwrap())
            .args(libs)
            .args(link_flags)
            .execute_output()
            .unwrap();
    }

    #[cfg(not(windows))]
    Command::new("clang")
        .arg("-Wno-unused-command-line-argument")
        .arg(object_file.to_string_lossy().as_ref())
        .arg(format!("-o{}", exe_file.display()))
        .args(lib_paths.iter().map(|path| format!("-L{}", path)))
        .arg("-lc")
        .arg("-lm")
        .args(libs.iter().map(|path| format!("-l:{}", path)))
        .arg("-no-pie")
        .args(link_flags)
        .execute_output()
        .expect("clang to succeed");
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
