mod builtin;
mod generate;
mod global_init;
#[cfg(windows)]
mod microsoft_craziness;
mod name_gen;
mod ty;
mod util;

use std::process::Command;

use camino::{Utf8Path, Utf8PathBuf};
use execute::Execute;
use rustc_hash::{FxHashMap, FxHashSet};

use compiler_core::{
    db::{build_options::EmitOption, Db, ExternLib},
    target::{Arch, Os},
};
use compiler_mir::Mir;

use crate::generate::Generator;

pub fn codegen(db: &mut Db, mir: &Mir) -> Utf8PathBuf {
    let c_file_path = db.time("Code generation", |db| {
        Generator {
            target_metrics: *db.target_metrics(),
            db,
            mir,
            types: vec![],
            rc_types: vec![],
            fn_decls: vec![],
            globals: vec![],
            fn_defs: vec![],
            adt_names: FxHashMap::default(),
        }
        .run()
    });

    build_exe(db, &c_file_path)
}

fn build_exe(db: &mut Db, c_file_path: &Utf8Path) -> Utf8PathBuf {
    let output_path = db.output_path();

    let exe_file_path = if db.target_metrics().os == Os::Windows {
        output_path.with_extension("exe")
    } else {
        output_path.with_extension("")
    };

    db.time("Zig CC", |db| compile(db, c_file_path, &exe_file_path));

    if !db.build_options().should_emit(EmitOption::C) {
        let _ = std::fs::remove_file(c_file_path);
    }

    exe_file_path
}

fn compile(db: &Db, c_file_path: &Utf8Path, exe_file_path: &Utf8Path) {
    let libs = Libraries::new(db);
    let target_metrics = db.target_metrics();

    let link_flags = match target_metrics.arch {
        Arch::Amd64 => match target_metrics.os {
            Os::Windows => vec!["/machine:x64"],
            Os::Linux | Os::FreeBSD => vec!["-march=x86_64"],
            _ => vec![],
        },
        Arch::_386 => match target_metrics.os {
            Os::Windows => vec!["/machine:x86"],
            Os::Darwin => panic!("unsupported architecture"),
            Os::Linux | Os::FreeBSD => vec!["-march=x86"],
            _ => vec![],
        },
        Arch::Arm64 => match target_metrics.os {
            Os::Darwin => vec!["-march=arm64"],
            Os::Linux => vec!["-march=aarch64"],
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

    //     #[cfg(windows)]
    //     {
    //         let find_result = unsafe {
    // microsoft_craziness::find_visual_studio_and_windows_sdk() };
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
    //             .arg(format!("/out:{}", exe_file_path))
    //             .arg("/entry:mainCRTStartup")
    //             .arg("/defaultlib:libcmt")
    //             .arg("/nologo")
    //             .arg("/incremental:no")
    //             .arg("/opt:ref")
    //             .arg("/threads:8")
    //             .arg("/subsystem:CONSOLE")
    //             .args(lib_paths.iter().map(|path| format!("/libpath:{}", path)))
    //             .arg(c_file_path.to_str().unwrap())
    //             .args(libs)
    //             .args(link_flags)
    //             .execute_output()
    //             .expect("linking to work");
    //     }

    //     #[cfg(not(windows))]

    let mut cmd = Command::new("zig");

    cmd
        .arg("cc")
        .arg(c_file_path)
        .arg(format!("-o{exe_file_path}"))
        .arg("-std=c99")
        .arg("-w")
        .args(libs.paths.into_iter().map(|path| format!("-L{path}")))
        .args(libs.libs.into_iter().map(|path| format!("-l{path}")))
        .args(libs.includes.into_iter().map(|path| format!("-I{path}")))
        .arg("-lc")
        .arg("-no-pie")
        // .arg("-static")
        // .arg("-O1")
        .arg("-g")
        .args(link_flags);

    //println!("{:?}", cmd);

    cmd.execute_output().expect("linking to work");
}

#[derive(Debug)]
struct Libraries {
    paths: FxHashSet<String>,
    libs: FxHashSet<String>,
    includes: FxHashSet<String>,
}

impl Libraries {
    fn new(db: &Db) -> Self {
        let mut this = Libraries {
            paths: FxHashSet::default(),
            libs: FxHashSet::default(),
            includes: FxHashSet::default(),
        };

        // C standard library
        this.libs.insert("c".to_string());
        this.libs.insert("m".to_string());

        // Add runtime library
        let rt_path = compiler_helpers::current_exe_dir().join("rt");
        this.paths.insert(rt_path.to_string());
        this.includes.insert(rt_path.to_string());
        this.libs.insert("jinrt".to_string());

        // Add libraries included by the user
        for lib in &db.extern_libs {
            match lib {
                ExternLib::Sys(lib_name) => {
                    this.libs.insert(lib_name.clone());
                }
                ExternLib::Path { search_path, name } => {
                    this.paths.insert(search_path.to_string());
                    this.libs.insert(name.clone());
                }
            }
        }

        this
    }
}
