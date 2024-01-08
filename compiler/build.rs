use std::path::PathBuf;

use fs_extra::dir;

const STD: &str = "std";

fn main() {
    let manifest_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();

    let std_dir = manifest_dir.join(STD);
    let target_dir =
        PathBuf::from(std::env::var("CARGO_BUILD_TARGET_DIR").unwrap());

    let options = dir::CopyOptions {
        overwrite: true,
        skip_exist: false,
        buffer_size: 64000,
        copy_inside: false,
        content_only: false,
        depth: 0,
    };

    dir::copy(&std_dir, target_dir, &options).unwrap();

    println!("cargo:rerun-if-changed={}", std_dir.display());
}
