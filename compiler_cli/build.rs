use std::{env, fs, io, path::Path, process::Command};

use fs_extra::dir;

fn main() {
    let debug = env::var("DEBUG").map(|d| d == "true").unwrap_or_default();
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir = Path::new(&manifest_dir);
    let ws_dir = manifest_dir.parent().unwrap();

    let target = if debug { ws_dir.join("target/debug") } else { ws_dir.join("target/release") };

    if !target.exists() {
        return;
    }

    copy_std(ws_dir, &target).expect("copying std failed");
    build_and_copy_rt(ws_dir, &target).expect("building rt failed");
}

fn copy_std(pwd: &Path, target: &Path) -> fs_extra::error::Result<()> {
    let std = pwd.join("std");

    dir::copy(
        &std,
        target,
        &dir::CopyOptions {
            overwrite: true,
            skip_exist: false,
            buffer_size: 64000,
            copy_inside: false,
            content_only: false,
            depth: 0,
        },
    )?;

    println!("cargo:rerun-if-changed={}/**/*", std.display());
    Ok(())
}

fn build_and_copy_rt(pwd: &Path, target: &Path) -> io::Result<()> {
    const SUPPORTED_TRIPLES: &[&str] = &["x86_64-linux-musl"];

    let rt = pwd.join("rt");

    let target_dir = target.join("rt");
    let _ = fs::create_dir(&target_dir);

    let header = rt.join("jinrt.h");

    fs::copy(&header, target_dir.join("jinrt.h"))?;
    println!("cargo:rerun-if-changed={}", header.display());

    for triple in SUPPORTED_TRIPLES {
        let output = Command::new("zig")
            .current_dir(&rt)
            .arg("build")
            .arg(format!("-Dtarget={triple}"))
            .output()?;

        if !output.status.success() {
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            panic!("`zig build` failed");
        }

        let lib_name = format!("libjinrt_{triple}.a");
        let lib_path = rt.join(format!("zig-out/lib/{lib_name}"));
        fs::copy(&lib_path, target_dir.join(lib_name))?;
        println!("cargo:rerun-if-changed={}", lib_path.display());
    }

    Ok(())
}
