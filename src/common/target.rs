use std::env;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetPlatform {
    Windows386,
    WindowsAmd64,
    Linux386,
    LinuxAmd64,
    LinuxArm64,
    DarwinAmd64,
    DarwinArm64,
    FreeBSD386,
    FreeBSDAmd64,
    EssenceAmd64,
    FreestandingWasm32,
    JsWasm32,
    WasiWasm32,
}

#[allow(unused)]
impl TargetPlatform {
    pub fn is_windows(&self) -> bool {
        matches!(
            self,
            TargetPlatform::Windows386 | TargetPlatform::WindowsAmd64
        )
    }

    pub fn is_linux(&self) -> bool {
        matches!(
            self,
            TargetPlatform::Linux386
                | TargetPlatform::LinuxAmd64
                | TargetPlatform::LinuxArm64
        )
    }

    pub fn is_darwin(&self) -> bool {
        matches!(
            self,
            TargetPlatform::DarwinAmd64 | TargetPlatform::DarwinArm64
        )
    }

    pub fn is_free_bsd(&self) -> bool {
        matches!(
            self,
            TargetPlatform::FreeBSD386 | TargetPlatform::FreeBSDAmd64
        )
    }

    pub fn is_essence(&self) -> bool {
        matches!(self, TargetPlatform::EssenceAmd64)
    }

    pub fn is_wasm(&self) -> bool {
        matches!(
            self,
            TargetPlatform::FreestandingWasm32
                | TargetPlatform::JsWasm32
                | TargetPlatform::WasiWasm32
        )
    }

    pub fn metrics(&self) -> TargetMetrics {
        match self {
            TargetPlatform::Windows386 => TargetMetrics {
                os: Os::Windows,
                arch: Arch::_386,
                word_size: 4,
                max_align: 8,
                target_triplet: "i386-pc-windows-msvc",
                target_data_layout: "",
            },
            TargetPlatform::WindowsAmd64 => TargetMetrics {
                os: Os::Windows,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-pc-windows-msvc",
                target_data_layout: "e-m:w-i64:64-f80:128-n8:16:32:64-S128",
            },
            TargetPlatform::Linux386 => TargetMetrics {
                os: Os::Linux,
                arch: Arch::_386,
                word_size: 4,
                max_align: 8,
                target_triplet: "i386-pc-linux-gnu",
                target_data_layout: "",
            },
            TargetPlatform::LinuxAmd64 => TargetMetrics {
                os: Os::Linux,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-pc-linux-gnu",
                target_data_layout: "e-m:w-i64:64-f80:128-n8:16:32:64-S128",
            },
            TargetPlatform::LinuxArm64 => TargetMetrics {
                os: Os::Linux,
                arch: Arch::Arm64,
                word_size: 8,
                max_align: 16,
                target_triplet: "aarch64-linux-elf",
                target_data_layout:
                    "e-m:e-i8:8:32-i16:32-i64:64-i128:128-n32:64-S128",
            },
            TargetPlatform::DarwinAmd64 => TargetMetrics {
                os: Os::Darwin,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-apple-darwin",
                target_data_layout: "e-m:o-i64:64-f80:128-n8:16:32:64-S128",
            },
            TargetPlatform::DarwinArm64 => TargetMetrics {
                os: Os::Darwin,
                arch: Arch::Arm64,
                word_size: 8,
                max_align: 16,
                target_triplet: "arm64-apple-macosx11.0.0",
                target_data_layout: "e-m:o-i64:64-i128:128-n32:64-S128",
            },
            TargetPlatform::FreeBSD386 => TargetMetrics {
                os: Os::FreeBSD,
                arch: Arch::_386,
                word_size: 4,
                max_align: 8,
                target_triplet: "i386-unknown-freebsd-elf",
                target_data_layout: "",
            },
            TargetPlatform::FreeBSDAmd64 => TargetMetrics {
                os: Os::FreeBSD,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-unknown-freebsd-elf",
                target_data_layout: "e-m:w-i64:64-f80:128-n8:16:32:64-S128",
            },
            TargetPlatform::EssenceAmd64 => TargetMetrics {
                os: Os::Essence,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-pc-none-elf",
                target_data_layout: "",
            },
            TargetPlatform::FreestandingWasm32 => TargetMetrics {
                os: Os::Freestanding,
                arch: Arch::Wasm32,
                word_size: 4,
                max_align: 8,
                target_triplet: "wasm32-freestanding-js",
                target_data_layout: "",
            },
            TargetPlatform::JsWasm32 => TargetMetrics {
                os: Os::Js,
                arch: Arch::Wasm32,
                word_size: 4,
                max_align: 8,
                target_triplet: "wasm32-js-js",
                target_data_layout: "",
            },
            TargetPlatform::WasiWasm32 => TargetMetrics {
                os: Os::Wasi,
                arch: Arch::Wasm32,
                word_size: 4,
                max_align: 8,
                target_triplet: "wasm32-wasi-js",
                target_data_layout: "",
            },
        }
    }

    pub fn current() -> Result<Self, &'static str> {
        match env::consts::OS {
            "linux" => Ok(Self::LinuxAmd64),
            "windows" => Ok(Self::WindowsAmd64),
            os => Err(os),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct TargetMetrics {
    pub os: Os,
    pub arch: Arch,
    pub word_size: usize,
    pub max_align: usize,
    pub target_triplet: &'static str,
    pub target_data_layout: &'static str,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Os {
    Windows,
    Darwin,
    Linux,
    Essence,
    FreeBSD,
    Wasi,
    Js,
    Freestanding,
}

#[allow(unused)]
impl Os {
    pub fn name(self) -> &'static str {
        match self {
            Os::Windows => "windows",
            Os::Darwin => "darwin",
            Os::Linux => "linux",
            Os::Essence => "essence",
            Os::FreeBSD => "freebsd",
            Os::Wasi => "wasi",
            Os::Js => "js",
            Os::Freestanding => "freestanding",
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Arch {
    Amd64,
    _386,
    Arm64,
    Wasm32,

    #[allow(unused)]
    Wasm64,
}

#[allow(unused)]
impl Arch {
    pub fn name(self) -> &'static str {
        match self {
            Arch::Amd64 => "amd64",
            Arch::_386 => "386",
            Arch::Arm64 => "arm64",
            Arch::Wasm32 => "wasm32",
            Arch::Wasm64 => "wasm64",
        }
    }

    pub fn endianness(self) -> Endianness {
        match self {
            Arch::Amd64
            | Arch::_386
            | Arch::Arm64
            | Arch::Wasm32
            | Arch::Wasm64 => Endianness::Little,
        }
    }
}

#[allow(unused)]
pub enum Endianness {
    Little,
    Big,
}

#[allow(unused)]
impl Endianness {
    pub fn name(&self) -> &str {
        match self {
            Endianness::Little => "little",
            Endianness::Big => "big",
        }
    }
}
