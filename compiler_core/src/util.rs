use camino::Utf8PathBuf;

pub fn current_exe_dir() -> Utf8PathBuf {
    Utf8PathBuf::from_path_buf(std::env::current_exe().unwrap().parent().unwrap().to_path_buf())
        .unwrap()
}
