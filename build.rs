// use fs_extra::dir;

// const STDLIB: &str = "stdlib";

fn main() {
    // TODO: stdlib
    // let cwd = std::env::current_dir().unwrap();
    //
    // let stdlib_dir = cwd.join(STDLIB);
    //
    // #[cfg(debug_assertions)]
    // let target_dir = cwd.join("target/debug");
    //
    // #[cfg(not(debug_assertions))]
    // let target_dir = cwd.join("target/release");
    //
    // if target_dir.exists() {
    //     let options = dir::CopyOptions {
    //         overwrite: true,
    //         skip_exist: false,
    //         buffer_size: 64000,
    //         copy_inside: false,
    //         content_only: false,
    //         depth: 0,
    //     };
    //
    //     dir::copy(stdlib_dir, target_dir, &options).unwrap();
    // }
    //
    // println!("cargo:rerun-if-changed=./{}", STDLIB);
}
