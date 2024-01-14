const std = @import("std");
const testing = std.testing;

const cstr = [*:0]const u8;

const Location = extern struct {
    path: cstr,
    line: u32,
    column: u32,
};

export fn jinrt_panic(msg: cstr) noreturn {
    std.debug.panic("panic at '{s}'\n", .{msg});
}

export fn jinrt_panic_at(msg: cstr, loc: Location) noreturn {
    std.debug.panic("panic at '{s}', {s}:{}:{}\n", .{ msg, loc.path, loc.line, loc.column });
}

export fn jinrt_alloc(size: usize) *anyopaque {
    const p = std.c.malloc(size);
    const x = p orelse jinrt_panic(@as(cstr, "out of memory"));
    return x;
}
