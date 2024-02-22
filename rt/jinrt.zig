const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const cstr = [*:0]const u8;
const Str = extern struct { data: cstr, len: usize };
const Refcnt = u32;

const Rc = extern struct {
    refcnt: Refcnt,
    data: void,
};

const RcBuf = extern struct {
    data: [*]u8,
    refcnt: Refcnt,

    const Self = @This();

    fn init(elem_size: usize, cap: usize) Self {
        const data: [*]u8 = @ptrCast(alloc_raw(void, elem_size * cap));
        return Self{ .data = data, .refcnt = 0 };
    }
};

const RcSlice = extern struct {
    array: ?*RcBuf,
    start: ?[*]u8,
    len: usize,
    cap: usize,

    const Self = @This();

    fn init(elem_size: usize, cap: usize) Self {
        if (cap == 0) {
            return Self.empty(null, 0);
        }

        const array = alloc_raw(RcBuf, @sizeOf(RcBuf));
        array.* = RcBuf.init(elem_size, cap);

        return Self.empty(array, cap);
    }

    fn empty(array: ?*RcBuf, cap: usize) Self {
        return Self{
            .array = array,
            .start = if (array) |a| a.data else null,
            .len = 0,
            .cap = cap,
        };
    }

    fn grow(self: Self, elem_size: usize, new_cap: usize) Self {
        if (self.array) |array| {
            // Allocate a new backing buffer for the array
            const new_data: [*]u8 = @ptrCast(alloc_raw(void, elem_size * new_cap));

            // Memcpy the old slice to the new backing buffer
            const new_data_slice = new_data[0..(elem_size * self.len)];
            const start_slice = self.start.?[0..(elem_size * self.len)];
            @memcpy(new_data_slice, start_slice);

            std.c.free(array.data);
            array.data = new_data;

            return Self{
                .array = array,
                .start = array.data,
                .len = self.len,
                .cap = new_cap,
            };
        } else {
            return Self.init(elem_size, new_cap);
        }
    }

    fn slice(
        self: Self,
        elem_size: usize,
        low: usize,
        high: usize,
    ) Self {
        if (self.start) |start| {
            const new_start = start[(low * elem_size)..(high * elem_size)];
            return Self{
                .array = self.array,
                .start = new_start.ptr,
                .len = high - low,
                .cap = self.cap - low,
            };
        } else {
            return self;
        }
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
    if (slice.cap == 0) return;
    if (slice.array) |array| {
        refcheck(backtrace, array.refcnt, tyname, frame);
        std.c.free(array.data);
        std.c.free(array);
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
        jinrt_panic_at(backtrace, @ptrCast(msg.ptr), frame);
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
        jinrt_panic_at(backtrace, @ptrCast(msg.ptr), frame);
    }

    if (high > slice.len) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "slice out of bounds: high bound is {} but len is {}",
            .{ high, slice.len },
        ) catch unreachable;
        jinrt_panic_at(backtrace, @ptrCast(msg.ptr), frame);
    }

    return slice.slice(elem_size, low, high);
}

export fn jinrt_slice_grow(
    backtrace: *Backtrace,
    slice: RcSlice,
    elem_size: usize,
    new_cap: usize,
    frame: StackFrame,
) RcSlice {
    if (new_cap <= slice.cap) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "grow out of bounds: cap is {} but new cap is {}",
            .{ slice.cap, new_cap },
        ) catch unreachable;
        jinrt_panic_at(backtrace, @ptrCast(msg.ptr), frame);
    }

    return slice.grow(elem_size, new_cap);
}

export fn jinrt_slice_cap(slice: RcSlice) usize {
    return slice.cap;
    // return if (slice.array) |a| a.cap else 0;
}

export fn jinrt_strcmp(a: Str, b: Str) bool {
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

inline fn str_slice(s: Str) []const u8 {
    return s.data[0..s.len];
}

inline fn alloc_raw(comptime T: type, size: usize) *T {
    const memory = std.c.malloc(size);
    const p = memory orelse std.debug.panic("out of memory", .{});
    return @alignCast(@ptrCast(p));
}

inline fn refcheck(backtrace: *Backtrace, refcnt: u32, tyname: cstr, frame: StackFrame) void {
    if (refcnt != 0) {
        const msg = std.fmt.allocPrint(
            std.heap.c_allocator,
            "cannot destroy a value of type `{s}` as it still has {} reference(s)",
            .{ tyname, refcnt },
        ) catch unreachable;
        jinrt_panic_at(backtrace, @ptrCast(msg.ptr), frame);
    }
}
