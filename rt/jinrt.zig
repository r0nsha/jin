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

const Backtrace = std.ArrayList(StackFrame);

const StackFrame = extern struct {
    file: cstr,
    function: cstr,
    line: u32,
};

export fn jinrt_init() void {}

const panic_fmt = "panic at '{s}'";

export fn jinrt_panic(msg: cstr) noreturn {
    std.debug.print(panic_fmt ++ "\n", .{msg});
    std.process.exit(1);
}

export fn jinrt_panic_at(msg: cstr, loc: Location) noreturn {
    std.debug.print(panic_fmt ++ ", {s}:{}:{}\n", .{ msg, loc.path, loc.line, loc.column });
    std.process.exit(1);
}

export fn jinrt_alloc(size: usize) *anyopaque {
    const memory = std.c.malloc(size);
    const p = memory orelse oom();
    return p;
}

export fn jinrt_free(obj: *anyrc, tyname: cstr, loc: Location) void {
    if (obj.refcnt != 0) {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const msg = std.fmt.allocPrint(
            gpa.allocator(),
            "cannot destroy a value of type `{s}` as it still has {} reference(s)",
            .{ tyname, obj.refcnt },
        ) catch oom();
        defer gpa.allocator().free(msg);
        // zig fmt: off
        jinrt_panic_at(@ptrCast(cstr, msg.ptr), loc);
    }

    std.c.free(obj);
}

export fn jinrt_strcmp(a: str, b: str) bool {
    return std.mem.eql(u8, str_slice(a), str_slice(b));
}

inline fn str_slice(s: str) []const u8 {
    return s.ptr[0..s.len];
}

inline fn oom() noreturn {
    jinrt_panic(@as(cstr, "out of memory"));
}
