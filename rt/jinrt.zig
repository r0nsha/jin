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

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s}:{}:{}", .{ self.path, self.line, self.column });
    }
};

const Backtrace = struct {
    frames: std.ArrayList(StackFrame),

    const Self = @This();

    fn push(self: *Self, frame: StackFrame) !void {
        try self.frames.append(frame);
    }

    fn pop(self: *Self) ?StackFrame {
        return self.frames.popOrNull();
    }

    fn print(self: *Self) void {
        std.debug.print("Stack trace (most recent call comes last):\n", .{});
        std.debug.print("frames: {}\n", .{self.frames.items.len});
        for (self.frames.items) |frame| {
            std.debug.print("{}", .{frame});
        }
    }
};

const StackFrame = struct {
    file: cstr,
    line: u32,
    in: cstr,

    const Self = @This();

    fn print(self: *Self) void {
        std.debug.print("{s}:{} in {s}", .{ self.file, self.line, self.in });
    }
};

export fn jinrt_init() void {}

const panic_fmt = "panic at '{s}'";

export fn jinrt_panic(backtrace: *Backtrace, msg: cstr) noreturn {
    _ = msg;
    backtrace.print();
    // std.debug.print(panic_fmt ++ "\n", .{msg});
    std.process.exit(1);
}

export fn jinrt_panic_at(backtrace: *Backtrace, msg: cstr, loc: Location) noreturn {
    _ = loc;
    _ = msg;
    backtrace.print();
    // std.debug.print(panic_fmt ++ ", {}\n", .{ msg, loc });
    std.process.exit(1);
}

export fn jinrt_alloc(size: usize) *anyopaque {
    const memory = std.c.malloc(size);
    const p = memory orelse std.debug.panic("out of memory", .{});
    return p;
}

export fn jinrt_free(backtrace: *Backtrace, obj: *anyrc, tyname: cstr, loc: Location) void {
    if (obj.refcnt != 0) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "cannot destroy a value of type `{s}` as it still has {} reference(s)",
            .{ tyname, obj.refcnt },
        ) catch unreachable;
        // zig fmt: off
        jinrt_panic_at(backtrace, @ptrCast(cstr,msg.ptr), loc);
        // zig fmt: on
    }

    std.c.free(obj);
}

export fn jinrt_strcmp(a: str, b: str) bool {
    return std.mem.eql(u8, str_slice(a), str_slice(b));
}

export fn jinrt_backtrace_new() *Backtrace {
    const backtrace = std.heap.c_allocator.create(Backtrace) catch unreachable;
    backtrace.* = Backtrace{
        .frames = std.ArrayList(StackFrame).init(std.heap.c_allocator),
    };
    return backtrace;
}

export fn jinrt_backtrace_push(backtrace: *Backtrace, file: cstr, line: u32, in: cstr) void {
    backtrace.push(StackFrame{ .file = file, .line = line, .in = in }) catch unreachable;
}

export fn jinrt_backtrace_pop(backtrace: *Backtrace) void {
    _ = backtrace.pop() orelse {
        std.debug.panic("jinrt_backtrace_pop: backtrace is empty", .{});
    };
}

inline fn str_slice(s: str) []const u8 {
    return s.ptr[0..s.len];
}
