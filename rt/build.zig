const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "jinrt",
        .root_source_file = .{ .path = "jinrt.zig" },
        .target = target,
        .optimize = optimize,
    });

    switch (optimize) {
        .Debug, .ReleaseSafe => lib.bundle_compiler_rt = true,
        .ReleaseFast, .ReleaseSmall => {},
    }

    lib.linkLibC();

    b.installArtifact(lib);
}
