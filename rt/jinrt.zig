const std = @import("std");
const testing = std.testing;

const cstr = [*:0]const u8;

const Location = extern struct {
    path: cstr,
    line: u32,
    column: u32,
};

var alloc: std.mem.Allocator = undefined;

export fn jinrt_init() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};
    alloc = gpa.allocator();
}

export fn jinrt_panic(msg: cstr) noreturn {
    std.debug.panic("panic at '{s}'\n", .{msg});
}

export fn jinrt_panic_at(msg: cstr, loc: Location) noreturn {
    std.debug.panic("panic at '{s}', {s}:{}:{}\n", .{ msg, loc.path, loc.line, loc.column });
}

export fn jinrt_alloc(size: usize) *anyopaque {
    const p = alloc.alloc(u8, size);
    const x = p catch jinrt_panic(@as(cstr, "out of memory"));
    return x.ptr;
}
