const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const unit = extern struct {};
const cstr = [*:0]const u8;
const str = extern struct { ptr: cstr, len: usize };

const anyrc = extern struct {
    refcnt: usize,
    data: unit,
};

const anyarray = extern struct {
    data: *anyopaque,
    cap: usize,
    refcnt: usize,

    const Self = @This();

    fn init(elem_size: usize, cap: usize) Self {
        const data = alloc_raw(anyopaque, elem_size * cap);
        return Self{ .data = data, .cap = cap, .refcnt = 0 };
    }
};

const anyslice = extern struct {
    array: ?*anyarray,
    start: usize,
    len: usize,

    const Self = @This();

    fn init(elem_size: usize, cap: usize) Self {
        if (cap == 0) {
            return anyslice.empty(null);
        }

        const array = alloc_raw(anyarray, @sizeOf(anyarray));
        array.* = anyarray.init(elem_size, cap);

        return Self.empty(array);
    }

    fn empty(array: ?*anyarray) Self {
        return anyslice{ .array = array, .start = 0, .len = 0 };
    }
};

const Backtrace = struct {
    frames: std.ArrayList(StackFrame),

    const Self = @This();

    fn init(allocator: Allocator) !*Self {
        const this = allocator.create(Self) catch unreachable;
        this.* = Self{
            .frames = std.ArrayList(StackFrame).init(allocator),
        };
        return this;
    }

    fn deinit(self: *Self) void {
        self.frames.deinit();
    }

    fn push(self: *Self, frame: StackFrame) !void {
        try self.frames.append(frame);
    }

    fn pop(self: *Self) ?StackFrame {
        return self.frames.popOrNull();
    }

    fn print(self: *Self) void {
        std.debug.print("Stack trace (most recent call comes last):\n", .{});
        for (self.frames.items, 0..) |frame, idx| {
            std.debug.print("    {}: {}\n", .{ idx, frame });
        }
    }
};

const StackFrame = extern struct {
    file: cstr,
    line: u32,
    in: cstr,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s}:{} in {s}", .{ self.file, self.line, self.in });
    }
};

export fn jinrt_init() void {}

export fn jinrt_panic_at(backtrace: *Backtrace, msg: cstr, frame: StackFrame) noreturn {
    backtrace.push(frame) catch unreachable;
    backtrace.print();
    const tid = std.Thread.getCurrentId();
    std.debug.print("Thread {} panicked: {s}\n", .{ tid, msg });
    std.process.exit(1);
}

export fn jinrt_alloc(size: usize) *anyrc {
    const p = alloc_raw(anyrc, size);
    p.refcnt = 0;
    return p;
}

export fn jinrt_free(
    backtrace: *Backtrace,
    obj: *anyrc,
    tyname: cstr,
    frame: StackFrame,
) void {
    refcheck(backtrace, obj.refcnt, tyname, frame);
    std.c.free(obj);
}

export fn jinrt_slice_alloc(elem_size: usize, cap: usize) anyslice {
    return anyslice.init(elem_size, cap);
}

export fn jinrt_slice_incref(s: anyslice) void {
    if (s.array) |a| a.refcnt += 1;
}

export fn jinrt_slice_decref(s: anyslice) void {
    if (s.array) |a| a.refcnt -= 1;
}

export fn jinrt_slice_ptr(s: anyslice) ?*anyopaque {
    return if (s.array) |a| a.data else null;
}

export fn jinrt_slice_cap(s: anyslice) usize {
    return if (s.array) |a| a.cap else 0;
}

export fn jinrt_slice_free(
    backtrace: *Backtrace,
    slice: anyslice,
    tyname: cstr,
    frame: StackFrame,
) void {
    if (slice.array) |array| {
        if (array.cap == 0) {
            return;
        }

        refcheck(backtrace, array.refcnt, tyname, frame);
        std.c.free(array.data);
        std.c.free(array);
    }
}

export fn jinrt_strcmp(a: str, b: str) bool {
    return std.mem.eql(u8, str_slice(a), str_slice(b));
}

export fn jinrt_backtrace_new() *Backtrace {
    return Backtrace.init(std.heap.c_allocator) catch unreachable;
}

export fn jinrt_backtrace_free(backtrace: *Backtrace) void {
    backtrace.deinit();
    std.c.free(backtrace);
}

export fn jinrt_backtrace_push(backtrace: *Backtrace, frame: StackFrame) void {
    backtrace.push(frame) catch unreachable;
}

export fn jinrt_backtrace_pop(backtrace: *Backtrace) void {
    _ = backtrace.pop() orelse {
        std.debug.panic("jinrt_backtrace_pop: backtrace is empty", .{});
    };
}

inline fn str_slice(s: str) []const u8 {
    return s.ptr[0..s.len];
}

fn alloc_raw(comptime T: type, size: usize) *T {
    const memory = std.c.malloc(size);
    const p = memory orelse std.debug.panic("out of memory", .{});
    return @alignCast(@ptrCast(p));
}

fn refcheck(backtrace: *Backtrace, refcnt: usize, tyname: cstr, frame: StackFrame) void {
    if (refcnt != 0) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "cannot destroy a value of type `{s}` as it still has {} reference(s)",
            .{ tyname, refcnt },
        ) catch unreachable;
        jinrt_panic_at(backtrace, @ptrCast(msg.ptr), frame);
    }
}
