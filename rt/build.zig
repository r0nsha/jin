const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const libjinrt = b.addStaticLibrary(.{
        .name = "jinrt",
        .root_source_file = .{ .path = "jinrt.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(libjinrt);
}
