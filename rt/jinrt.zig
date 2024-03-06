const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const cstr = [*:0]const u8;
const Refcnt = u32;
const Unit = extern struct {};

const Rc = extern struct {
    refcnt: Refcnt,
    data: void,
};

const RcBuf = extern struct {
    data: [*]u8,
    cap: usize,
    refcnt: Refcnt,

    const Self = @This();

    fn init(elem_size: usize, cap: usize) Self {
        const data: [*]u8 = @ptrCast(alloc_raw(void, elem_size * cap));
        return Self{ .data = data, .cap = cap, .refcnt = 0 };
    }
};

const RcSlice = extern struct {
    array: ?*RcBuf,
    start: ?[*]u8,
    len: usize,

    const Self = @This();

    fn init(elem_size: usize, cap: usize) Self {
        if (cap == 0) {
            return Self.empty(null);
        }

        const array = alloc_raw(RcBuf, @sizeOf(RcBuf));
        array.* = RcBuf.init(elem_size, cap);

        return Self.empty(array);
    }

    fn empty(array: ?*RcBuf) Self {
        return Self{
            .array = array,
            .start = if (array) |a| a.data else null,
            .len = 0,
        };
    }

    fn grow(self: *Self, elem_size: usize, new_cap: usize) void {
        if (self.array) |array| {
            const slice_offset = @intFromPtr(array.data) - @intFromPtr(self.start.?);
            const new_data: [*]u8 = @ptrCast(std.c.realloc(array.data, elem_size * new_cap));
            array.data = new_data;
            self.start = new_data + slice_offset;
            array.cap = new_cap;
        } else {
            self.* = Self.init(elem_size, new_cap);
        }
    }

    fn slice(
        self: Self,
        elem_size: usize,
        low: usize,
        high: usize,
    ) Self {
        if (self.start) |start| {
            return Self{
                .array = self.array,
                .start = start + (low * elem_size),
                .len = high - low,
            };
        } else {
            return self;
        }
    }

    fn as_slice(self: Self) ?[]u8 {
        return if (self.start) |start| start[0..self.len] else null;
    }
};

const Str = RcSlice;

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

export fn jinrt_panic_raw(backtrace: *Backtrace, msg: cstr, frame: StackFrame) noreturn {
    backtrace.push(frame) catch unreachable;
    backtrace.print();
    const tid = std.Thread.getCurrentId();
    std.debug.print("Thread {} panicked: {s}\n", .{ tid, msg });
    std.process.exit(1);
}

export fn jinrt_panic(backtrace: *Backtrace, msg: Str, frame: StackFrame) noreturn {
    backtrace.push(frame) catch unreachable;
    backtrace.print();
    const tid = std.Thread.getCurrentId();
    std.debug.print("Thread {} panicked: {s}\n", .{ tid, msg.as_slice().? });
    std.process.exit(1);
}

export fn jinrt_alloc(size: usize) *Rc {
    const p = alloc_raw(Rc, size);
    p.refcnt = 0;
    return p;
}

export fn jinrt_free(
    backtrace: *Backtrace,
    obj: *Rc,
    tyname: cstr,
    frame: StackFrame,
) void {
    refcheck(backtrace, obj.refcnt, tyname, frame);
    std.c.free(obj);
}

export fn jinrt_slice_alloc(elem_size: usize, cap: usize) RcSlice {
    return RcSlice.init(elem_size, cap);
}

export fn jinrt_slice_free(
    backtrace: *Backtrace,
    slice: RcSlice,
    tyname: cstr,
    frame: StackFrame,
) void {
    if (slice.array) |a| {
        if (a.cap == 0) return;
        refcheck(backtrace, a.refcnt, tyname, frame);
        std.c.free(a.data);
        std.c.free(a);
    }
}

export fn jinrt_slice_incref(s: RcSlice) void {
    if (s.array) |a| a.refcnt += 1;
}

export fn jinrt_slice_decref(s: RcSlice) void {
    if (s.array) |a| a.refcnt -= 1;
}

export fn jinrt_slice_index_boundscheck(
    backtrace: *Backtrace,
    slice: RcSlice,
    index: usize,
    frame: StackFrame,
) void {
    if (index >= slice.len) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "index out of bounds: len is {} but index is {}",
            .{ slice.len, index },
        ) catch unreachable;
        jinrt_panic_raw(backtrace, @ptrCast(msg.ptr), frame);
    }
}

export fn jinrt_slice_slice(
    backtrace: *Backtrace,
    slice: RcSlice,
    elem_size: usize,
    low: usize,
    high: usize,
    frame: StackFrame,
) RcSlice {
    if (low > high) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "low bound ({}) must be lower than high bound ({})",
            .{ low, high },
        ) catch unreachable;
        jinrt_panic_raw(backtrace, @ptrCast(msg.ptr), frame);
    }

    if (high > slice.len) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "slice out of bounds: high bound is {} but len is {}",
            .{ high, slice.len },
        ) catch unreachable;
        jinrt_panic_raw(backtrace, @ptrCast(msg.ptr), frame);
    }

    return slice.slice(elem_size, low, high);
}

export fn jinrt_slice_grow(
    backtrace: *Backtrace,
    slice: *RcSlice,
    elem_size: usize,
    new_cap: usize,
    frame: StackFrame,
) Unit {
    if (slice.array) |a| {
        if (new_cap <= a.cap) {
            return Unit{};
        }

        if (a.refcnt > 1) {
            const msg = std.fmt.allocPrint(
                std.heap.c_allocator,
                "cannot grow a slice as it still has {} reference(s)",
                .{a.refcnt},
            ) catch unreachable;
            jinrt_panic_raw(backtrace, @ptrCast(msg.ptr), frame);
        }
    }

    slice.grow(elem_size, new_cap);
    return Unit{};
}

export fn jinrt_slice_cap(slice: *RcSlice) usize {
    return if (slice.array) |a| a.cap else slice.len;
}

export fn jinrt_str_cmp(a: Str, b: Str) bool {
    if (a.as_slice()) |as| {
        if (b.as_slice()) |bs| {
            return std.mem.eql(u8, as, bs);
        }
    }

    return false;
}

export fn jinrt_slice_utf8_validate(slice: RcSlice) bool {
    // TODO:
    _ = slice;
    return true;
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

inline fn alloc_raw(comptime T: type, size: usize) *T {
    const memory = std.c.malloc(size);
    const p = memory orelse std.debug.panic("out of memory", .{});
    return @alignCast(@ptrCast(p));
}

inline fn refcheck(backtrace: *Backtrace, refcnt: u32, tyname: cstr, frame: StackFrame) void {
    if (refcnt > 0) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "cannot destroy a value of type `{s}` as it still has {} reference(s)",
            .{ tyname, refcnt },
        ) catch unreachable;
        jinrt_panic_raw(backtrace, @ptrCast(msg.ptr), frame);
    }
}
