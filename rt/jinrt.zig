const std = @import("std");
const testing = std.testing;

const unit = extern struct {};
const cstr = [*:0]const u8;
const str = extern struct { ptr: cstr, len: usize };
const anyrc = extern struct {
    refcnt: usize,
    data: unit,
};

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
    std.debug.print("panic at '{s}'\n", .{msg});
    std.process.exit(1);
}

export fn jinrt_panic_at(msg: cstr, loc: Location) noreturn {
    std.debug.print("panic at '{s}', {s}:{}:{}\n", .{ msg, loc.path, loc.line, loc.column });
    std.process.exit(1);
}

export fn jinrt_alloc(size: usize) *anyopaque {
    const memory = std.c.malloc(size);
    // const p = alloc.alloc(u8, size);
    // const x = p catch jinrt_panic(@as(cstr, "out of memory"));
    // return x.ptr;
    const p = memory orelse jinrt_panic(@as(cstr, "out of memory"));
    return p;
}

export fn jinrt_free(obj: *anyrc, fmt: cstr, loc: Location) void {
    if (obj.refcnt != 0) {
        jinrt_panic_at(fmt, loc);
    }

    std.c.free(obj);
    // alloc.free(memory);
}

export fn jinrt_strcmp(a: str, b: str) bool {
    return std.mem.eql(u8, str_slice(a), str_slice(b));
}

inline fn str_slice(s: str) []const u8 {
    return s.ptr[0..s.len];
}
