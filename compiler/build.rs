use std::{
    env, fs, io,
    path::{Path, PathBuf},
    process::Command,
};

use fs_extra::dir;

fn main() {
    let debug = env::var("DEBUG").map(|d| d == "true").unwrap_or_default();

    let pwd = PathBuf::from(env::var("PWD").unwrap());
    let target = if debug { pwd.join("target/debug") } else { pwd.join("target/release") };
    assert!(target.exists());

    copy_std(&pwd, &target).expect("copying std failed");
    build_rt(&pwd).expect("building rt failed");
    copy_rt(&pwd, &target).expect("copying rt failed");
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

fn build_rt(pwd: &Path) -> io::Result<()> {
    let rt = pwd.join("rt");
    Command::new("zig").current_dir(rt).arg("build").output()?;
    Ok(())
}

fn copy_rt(pwd: &Path, target: &Path) -> io::Result<()> {
    let rt = pwd.join("rt");

    let target = target.join("rt");
    let _ = fs::create_dir(&target);

    let header = rt.join("jinrt.h");
    let lib = rt.join("zig-out/lib/libjinrt.a");

    fs::copy(&header, target.join("jinrt.h"))?;
    fs::copy(&lib, target.join("libjinrt.a"))?;

    println!("cargo:rerun-if-changed={}", header.display());
    println!("cargo:rerun-if-changed={}", lib.display());

    Ok(())
}
