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
        matches!(self, Self::Windows386 | Self::WindowsAmd64)
    }

    pub fn is_linux(&self) -> bool {
        matches!(self, Self::Linux386 | Self::LinuxAmd64 | Self::LinuxArm64)
    }

    pub fn is_darwin(&self) -> bool {
        matches!(self, Self::DarwinAmd64 | Self::DarwinArm64)
    }

    pub fn is_free_bsd(&self) -> bool {
        matches!(self, Self::FreeBSD386 | Self::FreeBSDAmd64)
    }

    pub fn is_essence(&self) -> bool {
        matches!(self, Self::EssenceAmd64)
    }

    pub fn is_wasm(&self) -> bool {
        matches!(self, Self::FreestandingWasm32 | Self::JsWasm32 | Self::WasiWasm32)
    }

    pub fn metrics(&self) -> TargetMetrics {
        match self {
            Self::Windows386 => TargetMetrics {
                os: Os::Windows,
                arch: Arch::_386,
                word_size: 4,
                max_align: 8,
                target_triplet: "i386-pc-windows-msvc",
                target_data_layout: "",
            },
            Self::WindowsAmd64 => TargetMetrics {
                os: Os::Windows,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-pc-windows-msvc",
                target_data_layout: "e-m:w-i64:64-f80:128-n8:16:32:64-S128",
            },
            Self::Linux386 => TargetMetrics {
                os: Os::Linux,
                arch: Arch::_386,
                word_size: 4,
                max_align: 8,
                target_triplet: "i386-pc-linux-gnu",
                target_data_layout: "",
            },
            Self::LinuxAmd64 => TargetMetrics {
                os: Os::Linux,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-pc-linux-gnu",
                target_data_layout: "e-m:w-i64:64-f80:128-n8:16:32:64-S128",
            },
            Self::LinuxArm64 => TargetMetrics {
                os: Os::Linux,
                arch: Arch::Arm64,
                word_size: 8,
                max_align: 16,
                target_triplet: "aarch64-linux-elf",
                target_data_layout: "e-m:e-i8:8:32-i16:32-i64:64-i128:128-n32:64-S128",
            },
            Self::DarwinAmd64 => TargetMetrics {
                os: Os::Darwin,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-apple-darwin",
                target_data_layout: "e-m:o-i64:64-f80:128-n8:16:32:64-S128",
            },
            Self::DarwinArm64 => TargetMetrics {
                os: Os::Darwin,
                arch: Arch::Arm64,
                word_size: 8,
                max_align: 16,
                target_triplet: "arm64-apple-macosx11.0.0",
                target_data_layout: "e-m:o-i64:64-i128:128-n32:64-S128",
            },
            Self::FreeBSD386 => TargetMetrics {
                os: Os::FreeBSD,
                arch: Arch::_386,
                word_size: 4,
                max_align: 8,
                target_triplet: "i386-unknown-freebsd-elf",
                target_data_layout: "",
            },
            Self::FreeBSDAmd64 => TargetMetrics {
                os: Os::FreeBSD,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-unknown-freebsd-elf",
                target_data_layout: "e-m:w-i64:64-f80:128-n8:16:32:64-S128",
            },
            Self::EssenceAmd64 => TargetMetrics {
                os: Os::Essence,
                arch: Arch::Amd64,
                word_size: 8,
                max_align: 16,
                target_triplet: "x86_64-pc-none-elf",
                target_data_layout: "",
            },
            Self::FreestandingWasm32 => TargetMetrics {
                os: Os::Freestanding,
                arch: Arch::Wasm32,
                word_size: 4,
                max_align: 8,
                target_triplet: "wasm32-freestanding-js",
                target_data_layout: "",
            },
            Self::JsWasm32 => TargetMetrics {
                os: Os::Js,
                arch: Arch::Wasm32,
                word_size: 4,
                max_align: 8,
                target_triplet: "wasm32-js-js",
                target_data_layout: "",
            },
            Self::WasiWasm32 => TargetMetrics {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TargetMetrics {
    pub os: Os,
    pub arch: Arch,
    pub word_size: usize,
    pub max_align: usize,
    pub target_triplet: &'static str,
    pub target_data_layout: &'static str,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
            Self::Windows => "windows",
            Self::Darwin => "darwin",
            Self::Linux => "linux",
            Self::Essence => "essence",
            Self::FreeBSD => "freebsd",
            Self::Wasi => "wasi",
            Self::Js => "js",
            Self::Freestanding => "freestanding",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
            Self::Amd64 => "amd64",
            Self::_386 => "386",
            Self::Arm64 => "arm64",
            Self::Wasm32 => "wasm32",
            Self::Wasm64 => "wasm64",
        }
    }

    pub fn endianness(self) -> Endianness {
        match self {
            Self::Amd64 | Self::_386 | Self::Arm64 | Self::Wasm32 | Self::Wasm64 => {
                Endianness::Little
            }
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
            Self::Little => "little",
            Self::Big => "big",
        }
    }
}
