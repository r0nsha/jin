const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const triple = try target.result.linuxTriple(allocator);
    defer allocator.free(triple);

    const name = try std.fmt.allocPrint(allocator, "jinrt_{s}", .{triple});

    const lib = b.addStaticLibrary(.{
        .name = name,
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
