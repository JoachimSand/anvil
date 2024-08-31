const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const air_mod = @import("air.zig");
const Air = air_mod.Air;
const AirState = air_mod.AirState;
const AirInst = air_mod.AirInst;
// Val: Comptime value. Type is unknown. E.g. 32
// TypedVal: Comptime value with known type. E.g. 32 with type u8

// Type: Comptime type. E.g. u8 , [5]bool
// TypeVal

// a : u32 = 1;
// %1 = constant(u64, 1);
// %2 = constant(u32, 1);

// Bin := struct {
//     a : u32,
//     b : u32,
// };
// node : Bin = Bin.{a = 1; b = 2};
// %1 = constant(type, struct {a : u32, b : u32});
// %2 = constant(type, struct {a : u32, b : u32});
// %3 = constant(struct { a : u32, b : u32}, )

// Enums
// We know that since an enum is accessed here
// %1 = get_element_ptr &enum { field_a : u32, field_b : u32}, from .. by 1

// Represents a compile-time known type
pub const Value = union(enum) {
    pub const Index = u32;

    const RefStart = 4294967040;
    pub const IndexRef = enum(Index) {
        boolean_typ = RefStart,
        unknown_int_typ,
        u64_typ,
        u32_typ,
        u16_typ,
        u8_typ,
        i64_typ,
        i32_typ,
        i16_typ,
        i8_typ,
        void_typ,
        typ_typ,
        own_typ,
        ref_typ,
        stackref_typ,
        opaque_typ,
        _,

        pub fn is_ref(ref: *const Value.IndexRef) bool {
            if (@intFromEnum(ref.*) < Value.RefStart) {
                return true;
            } else {
                return false;
            }
        }

        pub fn format(ref: *const IndexRef, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            if (ref.is_ref()) {
                try std.fmt.format(writer, "%val_ref.{d}", .{@intFromEnum(ref.*)});
            } else {
                try std.fmt.format(writer, "{s}", .{@tagName(ref.*)});
            }
        }
    };

    const List = std.MultiArrayList(Value);
    const Field = struct { var_name: Air.StringIndex, field_type: Value.IndexRef };

    const TirAggregrate = struct {
        fields_start: Index,
        fields_end: Index,
    };

    unknown_int_val: i64,
    u64_val: u64,
    u32_val: u32,
    u16_val: u16,
    u8_val: u8,
    i64_val: i64,
    i32_val: i32,
    i16_val: i16,
    i8_val: i8,
    null_val,
    boolean_val: bool,

    // recursive_ptr: struct { deref_type: IndexRef, cap: IndexRef },
    ptr_typ: struct { deref_type: IndexRef, cap: IndexRef },
    struct_typ: TirAggregrate,
    struct_field_typ: Field,
    mut_struct_field_typ: Field,
    enum_typ: TirAggregrate,
    enum_field_typ: Field,

    array_typ: struct {
        size: u32,
        element_type: Value.IndexRef,
    },
};

// allow_cap_coercion: If set to true, if a has an ownership ptr type and b has a ref ptr type,
// accept equality
fn type_ref_eq(s: *TirState, a: Value.IndexRef, b: Value.IndexRef, allow_cap_coercion: bool) bool {
    if (a == b) {
        return true;
    } else {
        if (a == .opaque_typ or b == .opaque_typ) {
            return true;
        }
        if (a.is_ref() and b.is_ref()) {
            const a_type = s.tir.values.get(@intFromEnum(a));
            const b_type = s.tir.values.get(@intFromEnum(b));

            if (a_type == .ptr_typ and b_type == .ptr_typ) {
                if (a_type.ptr_typ.deref_type == .opaque_typ or b_type.ptr_typ.deref_type == .opaque_typ) {
                    return true;
                }
                if (a_type.ptr_typ.deref_type == b_type.ptr_typ.deref_type) {
                    if (a_type.ptr_typ.cap == b_type.ptr_typ.cap) {
                        return true;
                    } else if (allow_cap_coercion and a_type.ptr_typ.cap == .own_typ and b_type.ptr_typ.cap == .ref_typ) {
                        return true;
                    } else {
                        return false;
                    }
                }
            } else if (a_type == .enum_typ and b_type == .enum_typ) {
                return true;
            }
        }
        print("Types are not equal: \n", .{});
        print_type(&s.tir, a) catch unreachable;
        print("\n", .{});
        print_type(&s.tir, b) catch unreachable;
        print("\n", .{});
        return false;
    }
}
fn type_ref_eq_stackref_coerce(s: *TirState, a: Value.IndexRef, b: Value.IndexRef) bool {
    if (a == b) {
        return true;
    } else {
        if (a == .opaque_typ or b == .opaque_typ) {
            return true;
        }
        if (a.is_ref() and b.is_ref()) {
            const a_type = s.tir.values.get(@intFromEnum(a));
            // const b_type = t.tir.types.get(@intFromEnum(b));

            // TODO: Undo
            if (a_type == .ptr_typ and a_type.ptr_typ.cap == .stackref_typ) {
                return true;
            }
            if (a_type == .ptr_typ and a_type.ptr_typ.cap == .stackref_typ and type_ref_eq(s, a_type.ptr_typ.deref_type, b, true)) {
                return true;
            }

            // if (a_type == .ptr_typ and b_type == .ptr) {
            //     if (a_type.ptr.deref_type == b_type.ptr.deref_type) {
            //         if (a_type.ptr.cap == b_type.ptr.cap) {
            //             return true;
            //         } else if (allow_cap_coercion and a_type.ptr.cap == .own_typ and b_type.ptr.cap == .ref_typ) {
            //             return true;
            //         } else {
            //             return false;
            //         }
            //     }
            // }
        }
        print("Types are not equal: \n", .{});
        print_type(&s.tir, a) catch unreachable;
        print("\n", .{});
        print_type(&s.tir, b) catch unreachable;
        print("\n", .{});
        return false;
    }
}

//Compile-time known values.
// pub const Value = union(enum) {
//     pub const Index = u32;
//     const List = std.MultiArrayList(Value);
//     unknown_int: i64,
//     u64: u64,
//     u32: u32,
//     u16: u16,
//     u8: u8,
//     i64: i64,
//     i32: i32,
//     i16: i16,
//     i8: i8,
//     null_val,
//     boolean: bool,
// };

// Typed intermediate representation
pub const TirInst = union(enum) {
    pub const Index = u32;
    const RefStart = 4294967040;
    pub const IndexRef = enum(Index) {
        true_val = RefStart,
        false_val,
        null_val,
        _,

        pub fn is_ref(ref: *const IndexRef) bool {
            if (@intFromEnum(ref.*) < RefStart) {
                return true;
            } else {
                return false;
            }
        }

        pub fn format(ref: *const IndexRef, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            if (ref.is_ref()) {
                try std.fmt.format(writer, "%{d}", .{@intFromEnum(ref.*)});
            } else {
                try std.fmt.format(writer, "inst_ref.{s}", .{@tagName(ref.*)});
            }
        }
    };
    pub const List = std.MultiArrayList(TirInst);

    pub const BinOp = struct {
        lhs: IndexRef,
        rhs: IndexRef,
    };

    pub const Alloca = struct {
        alloc_type: Value.IndexRef,
        ret_type: Value.IndexRef,
    };

    const ExtraSlice = packed struct {
        start: Tir.ExtraIndex,
        end: Tir.ExtraIndex,
    };
    pub const FnDef = struct {
        name: Air.StringIndex,
        params: ExtraSlice,
        ret_type: Value.IndexRef,
        blk: Index,
    };

    fn_def: FnDef,

    arg: struct {
        typ_ref: Value.IndexRef,
        name: Air.StringIndex,
    },
    block: struct {
        start: Index,
        end: Index,
    },
    br: Index,
    // br_cond: struct { cond: Index, blk: Index },
    br_either: struct {
        cond: Index,
        then_blk: Index,
        else_blk: Index,
    },
    constant_val: Value.Index,
    constant_type: Value.IndexRef,

    constant_index: struct {
        index: Value.Index,
        elem_type: Value.IndexRef,
        target: IndexRef,
    },
    index: struct { index: Index, elem_type: Value.IndexRef, target: IndexRef },
    zero_array: Value.IndexRef,
    memalloc: struct {
        expr: IndexRef,
        ptr_type: Value.IndexRef,
    },
    memfree: struct {
        ptr: IndexRef,
        expr_type: Value.IndexRef,
    },
    print: struct {
        val: IndexRef,
    },
    alloca: Alloca,
    load: struct {
        ptr: IndexRef,
        type: Value.IndexRef,
    },
    store: struct {
        val: IndexRef,
        val_type: Value.IndexRef,
        ptr: IndexRef,
    },
    address_of: struct {
        target: IndexRef,
        cap: IndexRef,
        ptr_type: Value.IndexRef,
    },
    update_enum_ptr_with_val: struct {
        enum_ptr: IndexRef,
        enum_type: Value.IndexRef,
        new_tag: u32,
        new_tag_val: IndexRef,
    },
    update_enum_ptr_with_ptr: struct {
        enum_ptr: IndexRef,
        enum_type: Value.IndexRef,
        new_tag: u32,
        new_tag_ptr: IndexRef,
    },
    get_element_ptr: struct {
        aggregate_ptr: IndexRef,
        aggregate_type: Value.IndexRef,
        indeces_start: Tir.ExtraIndex,
        indeces_end: Tir.ExtraIndex,
        ret_type: Value.IndexRef,
    },
    match: struct {
        enum_ptr: IndexRef,
        // Cases are in order of their tags i.e. order of appearance in definition
        // Each case in extra is a blk inst. ref
        cases_start: Tir.ExtraIndex,
        cases_end: Tir.ExtraIndex,
    },
    // Takes in an enum pointer and a tag. Allocates enough space for data type of the tag,
    // copies over the data from the enum_ptr and then returns a ptr to the newly allocated data type.
    enum_project: struct {
        enum_ptr: Index,
        enum_type: Value.IndexRef,
        tag: u32,
        ret_type: Value.IndexRef,
    },
    // ret : struct {
    //     val : IndexRef,

    // },
    ret_void,
    ret: struct {
        val: Value.IndexRef,
        ret_type: Value.IndexRef,
    },

    move: Index,
    // cap_reduce: struct {
    //     expr: Index,
    //     ret_type: Type.IndexRef,
    // },

    add_i8: BinOp,
    add_i16: BinOp,
    add_i32: BinOp,
    add_i64: BinOp,

    add_u8: BinOp,
    add_u16: BinOp,
    add_u32: BinOp,
    add_u64: BinOp,

    lt_i8: BinOp,
    lt_i16: BinOp,
    lt_i32: BinOp,
    lt_i64: BinOp,
    lt_u8: BinOp,
    lt_u16: BinOp,
    lt_u32: BinOp,
    lt_u64: BinOp,
};

//
const TirSpecificError = error{
    Unimplemented,
    MissingMapping,
    ExpectedVal,
    TypeOfType,
    NoType,
    ExpectedBoolean,
    AdditionTypeError,
    ComparisionTypeError,
    IntTooLarge,
    IntCoercionIntoNonInt,
    CannotCoerceKnownToUnknown,
    IndexIntoNonArrayType,
    ArrayTypeMustHaveKnownSize,
    PtrIsNotPtrType,
    ExpectedAggregrateType,
    StoringWrongType,
    StoringOwnTypeWithoutMove,
    MemfreeOnInvalidType,
    MismatchedEnumFieldType,
    MismatchedTypes,
    DerefOnPrimitive,
    DerefOnInvalidType,
    InvalidTagName,
    UndefinedVar,
};

pub const TirError = TirSpecificError || Allocator.Error;

pub const Tir = struct {
    instructions: TirInst.List,
    extra: std.ArrayList(u32),

    // compile-time known values
    values: Value.List,
    allocator: Allocator,

    air: *Air,

    pub const ExtraIndex = u32;
    pub fn deinit(t: *Tir) void {
        t.instructions.deinit(t.allocator);
        t.extra.deinit();
        t.values.deinit(t.allocator);
    }
};

const TirState = struct {
    air: *Air,
    tir: Tir,

    // State needed during generation of TIR from AIR.
    air_tir_inst_map: AirTirInstMap,
    air_tir_type_map: AirTirValMap,
    fn_def_map: FnDefMap,
    scratch: std.ArrayList(u32),

    const FnDefInfo = struct { tir_inst: TirInst.Index, air_inst: AirInst.Index };
    pub const AirTirInstMap = std.AutoHashMap(AirInst.IndexRef, TirInst.IndexRef);
    pub const AirTirValMap = std.AutoHashMap(AirInst.IndexRef, Value.IndexRef);
    pub const FnDefMap = std.AutoHashMap(Air.StringIndex, FnDefInfo);

    fn append_inst(t: *TirState, inst: TirInst) Allocator.Error!TirInst.IndexRef {
        const index = t.tir.instructions.len;

        try t.tir.instructions.append(t.tir.allocator, inst);
        return @enumFromInt(index);
    }

    fn append_val(t: *TirState, val: Value) Allocator.Error!Value.Index {
        const index: Value.Index = @intCast(t.tir.values.len);
        try t.tir.values.append(t.tir.allocator, val);
        return index;
    }

    fn append_val_ref(t: *TirState, val: Value) Allocator.Error!Value.IndexRef {
        const index: Value.Index = @intCast(t.tir.values.len);
        try t.tir.values.append(t.tir.allocator, val);
        return @enumFromInt(index);
    }

    fn get_inst_mapping(t: *TirState, air_index: AirInst.IndexRef) !TirInst.IndexRef {
        const tir_ref: TirInst.IndexRef = t.air_tir_inst_map.get(air_index) orelse {
            print("Missing instruction mapping for {}\n", .{air_index});
            return error.MissingMapping;
        };
        return tir_ref;
    }

    fn get_type_mapping(t: *TirState, air_index: AirInst.IndexRef) !Value.IndexRef {
        const tir_ref: Value.IndexRef = t.air_tir_type_map.get(air_index) orelse {
            print("Missing instruction mapping for {}\n", .{air_index});
            return error.MissingMapping;
        };
        return tir_ref;
    }

    fn get_fn_def_mapping(t: *TirState, name: Air.StringIndex) !FnDefInfo {
        const fn_info = t.fn_def_map.get(name) orelse {
            print("Missing fn def. mapping for {}\n", .{name});
            return error.MissingMapping;
        };
        return fn_info;
    }

    fn set_inst_mapping(t: *TirState, air_index: AirInst.Index, tir_index: TirInst.IndexRef) !void {
        try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_index);
    }

    fn set_type_mapping(t: *TirState, air_index: AirInst.Index, tir_type: Value.IndexRef) !void {
        try t.air_tir_type_map.put(@enumFromInt(air_index), tir_type);
    }

    fn append_constant_val(t: *TirState, val: Value, air_index: AirInst.Index) !void {
        const val_index = try t.append_val(val);
        const tir_inst = TirInst{ .constant_val = val_index };
        const tir_index = try t.append_inst(tir_inst);
        try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_index);
    }

    // fn append_constant_type(t: *TirState, val: Value, air_index: AirInst.Index) !void {
    //     const val_index = try t.append_val(val);
    //     const tir_inst = TirInst{ .constant_val = val_index };
    //     const tir_index = try t.append_inst(tir_inst);
    //     try t.air_tir_inst_map.put(air_ref, tir_index);
    // }

    fn deinit(t: *TirState) void {
        t.scratch.deinit();

        t.air_tir_inst_map.deinit();
        t.air_tir_type_map.deinit();
        t.fn_def_map.deinit();
    }
};

fn print_type(t: *Tir, type_ref: Value.IndexRef) !void {
    switch (type_ref) {
        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
            print("{s}", .{@tagName(type_ref)});
        },
        _ => {
            const type_index = @intFromEnum(type_ref);

            if (type_index >= t.values.len) {
                print("TYPE NOT AVAILABLE", .{});
                return;
            }
            const typ = t.values.get(type_index);
            switch (typ) {
                .ptr_typ => |ptr| {
                    print("&.", .{});
                    try print_type(t, ptr.cap);
                    print(" ", .{});
                    try print_type(t, ptr.deref_type);
                },
                .array_typ => |array| {
                    print("[{}]", .{array.size});
                    try print_type(t, array.element_type);
                },
                .struct_typ, .enum_typ => |tir_container| {
                    const is_struct = if (typ == .struct_typ) true else false;
                    if (is_struct) {
                        print("struct{{", .{});
                    } else {
                        print("enum{{", .{});
                    }

                    var cur_field_index = tir_container.fields_start;
                    while (cur_field_index < tir_container.fields_end) : (cur_field_index += 1) {
                        const cur_field = t.values.get(cur_field_index);
                        switch (cur_field) {
                            .struct_field_typ, .enum_field_typ => |field| {
                                const field_name = t.air.get_string(field.var_name);
                                const field_type = field.field_type;
                                print("{s} : ", .{field_name});
                                try print_type(t, field_type);
                                print(", ", .{});
                            },
                            .mut_struct_field_typ => |field| {
                                const field_name = t.air.get_string(field.var_name);
                                const field_type = field.field_type;
                                print("mut {s} : ", .{field_name});
                                try print_type(t, field_type);
                                print(", ", .{});
                            },
                            else => {
                                print("{} {}\n", .{ cur_field, cur_field_index });
                                unreachable;
                            },
                        }
                    }
                    print("}}", .{});
                },
                else => {
                    print("print type not impl for {}", .{typ});
                    // return error.Unimplemented;
                },
            }
        },
    }
}

fn print_val(t: *Tir, val_ref: Value.IndexRef) void {
    switch (val_ref) {
        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
            print("{s}", .{@tagName(val_ref)});
        },
        _ => {
            const val_index = @intFromEnum(val_ref);

            if (val_index >= t.values.len) {
                print("VALUE NOT AVAILABLE", .{});
                return;
            }
            const val = t.values.get(val_index);
            switch (val) {
                .null_val => print("null val", .{}),
                .boolean_val => |boolean| print("bool, {}", .{boolean}),
                .unknown_int_val => |int| print("unknown_int, {}", .{int}),
                .i8_val => |int| print("i8, {}", .{int}),
                .i16_val => |int| print("i16, {}", .{int}),
                .i32_val => |int| print("i32, {}", .{int}),
                .i64_val => |int| print("i16, {}", .{int}),
                .u8_val => |int| print("u8, {}", .{int}),
                .u16_val => |int| print("u16, {}", .{int}),
                .u32_val => |int| print("u32, {}", .{int}),
                .u64_val => |int| print("u64, {}", .{int}),

                .ptr_typ => |ptr| {
                    print("&.", .{});
                    try print_type(t, ptr.cap);
                    print(" ", .{});
                    try print_type(t, ptr.deref_type);
                },
                .array_typ => |array| {
                    print("[{}]", .{array.size});
                    try print_type(t, array.element_type);
                },
                .struct_typ, .enum_typ => |tir_container| {
                    const is_struct = if (val == .struct_typ) true else false;
                    if (is_struct) {
                        print("struct{{", .{});
                    } else {
                        print("enum{{", .{});
                    }

                    var cur_field_index = tir_container.fields_start;
                    while (cur_field_index < tir_container.fields_end) : (cur_field_index += 1) {
                        const cur_field = t.values.get(cur_field_index);
                        switch (cur_field) {
                            .struct_field_typ, .enum_field_typ => |field| {
                                const field_name = t.air.get_string(field.var_name);
                                const field_type = field.field_type;
                                print("{s} : ", .{field_name});
                                try print_type(t, field_type);
                                print(", ", .{});
                            },
                            .mut_struct_field_typ => |field| {
                                const field_name = t.air.get_string(field.var_name);
                                const field_type = field.field_type;
                                print("mut {s} : ", .{field_name});
                                try print_type(t, field_type);
                                print(", ", .{});
                            },
                            else => {
                                print("{} {}\n", .{ cur_field, cur_field_index });
                                unreachable;
                            },
                        }
                    }
                    print("}}", .{});
                },
                else => {
                    print("print type not impl for {}", .{val});
                    // return error.Unimplemented;
                },
            }
        },
    }
}

pub fn print_tir(t: *Tir, start: u32, stop: u32, indent: u32) !void {
    // print("---------- TIR -------------\n", .{});
    if (start >= stop) {
        return;
    }

    var index: u32 = start;
    while (index < stop and index < t.instructions.len) {
        for (0..indent) |_| {
            print("    ", .{});
        }

        print("%{} = ", .{index});
        const inst = t.instructions.get(index);
        switch (inst) {
            // .int => |int| {
            //     print("int({})", .{int});
            // },
            .constant_type => |type_index| {
                print("constant_type(", .{});
                try print_type(t, type_index);
                print(")", .{});
            },
            .constant_val => |val_index| {
                print("constant_val(", .{});
                print_val(t, @enumFromInt(val_index));
                print(")", .{});
            },
            .add_i8 => |add| {
                print("add i8 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_i16 => |add| {
                print("add i16 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_i32 => |add| {
                print("add i32 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_i64 => |add| {
                print("add i64 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_u8 => |add| {
                print("add u8 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_u16 => |add| {
                print("add u16 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_u32 => |add| {
                print("add u32 (%{}, %{})", .{ add.lhs, add.rhs });
            },
            .add_u64 => |add| {
                print("add u64 (%{}, %{})", .{ add.lhs, add.rhs });
            },

            .lt_i8, .lt_i16, .lt_i32, .lt_i64, .lt_u8, .lt_u16, .lt_u32, .lt_u64 => |lt| {
                print("{s}, {} < {}", .{ @tagName(inst), lt.lhs, lt.rhs });
            },

            .alloca => |alloca| {
                print("alloca ", .{});
                try print_type(t, alloca.alloc_type);
                print(", returns type ", .{});
                try print_type(t, alloca.ret_type);
            },
            .memalloc => |memalloc| {
                print("@memalloc %{} returns type ", .{memalloc.expr});
                try print_type(t, memalloc.ptr_type);
            },
            .memfree => |memfree| {
                print("@memfree %{} returns type ", .{memfree.ptr});
                try print_type(t, memfree.expr_type);
            },
            .print => |p| {
                print("@print %{} ", .{p.val});
            },
            .address_of => |address_of| {
                print("address_of %{} with cap %{} returns type ", .{ address_of.target, address_of.cap });
                try print_type(t, address_of.ptr_type);
            },
            .get_element_ptr => |get_elem_ptr| {
                print("get_element_ptr ", .{});
                try print_type(t, get_elem_ptr.aggregate_type);
                print(", from {} by ", .{get_elem_ptr.aggregate_ptr});

                const slice = t.extra.items[get_elem_ptr.indeces_start..get_elem_ptr.indeces_end];
                for (slice) |s| {
                    print("{}, ", .{s});
                }
            },
            .update_enum_ptr_with_val => |update_enum| {
                // update_enum.new_tag_val
                print("update_enum_ptr_with_val ", .{});
                try print_type(t, update_enum.enum_type);
                print(", {}, tagnum {}, {}", .{ update_enum.enum_ptr, update_enum.new_tag, update_enum.new_tag_val });
            },
            .update_enum_ptr_with_ptr => |update_enum| {
                // update_enum.new_tag_val
                print("update_enum_ptr_with_ptr ", .{});
                try print_type(t, update_enum.enum_type);
                print(", {}, tagnum {}, {}", .{ update_enum.enum_ptr, update_enum.new_tag, update_enum.new_tag_ptr });
            },
            .fn_def => |fn_def| {
                print("fn def {s} (", .{t.air.get_string(fn_def.name)});
                const param_extras = t.extra.items[fn_def.params.start..fn_def.params.end];
                for (param_extras) |param_inst| {
                    print("%{}, ", .{param_inst});
                }
                print(") -> ", .{});
                try print_type(t, fn_def.ret_type);
                print(" at blk %{}", .{fn_def.blk});
            },
            .match => |match| {
                print("match on %{} : ", .{match.enum_ptr});
                var case_index = match.cases_start;
                while (case_index < match.cases_end) : (case_index += 1) {
                    const blk = t.extra.items[case_index];
                    print("{} -> %{}, ", .{ case_index - match.cases_start, blk });
                }
            },
            .enum_project => |project| {
                print("enum_project ", .{});
                try print_type(t, project.enum_type);
                print(", %{}, tagnum {}, ret_type ", .{ project.enum_ptr, project.tag });
                try print_type(t, project.ret_type);
            },
            .zero_array => |zero_array_type| {
                print("zero_array of ", .{});
                try print_type(t, zero_array_type);
            },
            .index => |indexing| {
                print("indexing {} with {} yielding type ", .{ indexing.target, indexing.index });
                try print_type(t, indexing.elem_type);
            },
            .constant_index => |indexing| {
                print("indexing {} with {} yielding type ", .{ indexing.target, indexing.index });
                try print_type(t, indexing.elem_type);
            },
            .load => |load| {
                print("load ", .{});
                try print_type(t, load.type);
                print(", from {}", .{load.ptr});
            },
            .store => |store| {
                print("store ", .{});
                try print_type(t, store.val_type);
                print(" from {}, to ptr at {}", .{ store.val, store.ptr });
            },
            .arg => |arg| {
                print("arg(%{}, {s})", .{ arg.typ_ref, t.air.get_string(arg.name) });
            },
            .move => |move| {
                print("move %{}", .{move});
            },
            .ret_void => {
                print("ret void", .{});
            },
            .ret => |ret| {
                print("ret {} of type", .{ret.val});
                try print_type(t, ret.ret_type);
            },
            .br => |br| {
                print("br %{}", .{br});
            },
            .br_either => |br| {
                print("br %{}, then %{} else %{}", .{ br.cond, br.then_blk, br.else_blk });
            },
            .block => |blk| {
                print("blk %{} to %{} {{\n", .{ blk.start, blk.end });
                if (blk.start < blk.end) {
                    try print_tir(t, blk.start, blk.end + 1, indent + 1);
                    index = blk.end;
                    for (0..indent) |_| {
                        print("    ", .{});
                    }
                }

                print("}}", .{});
            },
            // else => {
            //     print("TIR printing for {} is unimplemented \n", .{inst});
            //     return error.Unimplemented;
            // },
        }
        print(";\n", .{});
        index += 1;
    }
}

fn get_val(s: *TirState, tir_ref: TirInst.IndexRef) !?Value {
    switch (tir_ref) {
        .null_val => return .null_val,
        .true_val => return Value{ .boolean_val = true },
        .false_val => return Value{ .boolean_val = false },
        _ => {
            const tir_index: TirInst.Index = @intFromEnum(tir_ref);
            const tir_inst = s.tir.instructions.get(tir_index);
            switch (tir_inst) {
                .constant_val => |val| return s.tir.values.get(val),
                else => return null,
            }
        },
    }
}

fn get_val_index(s: *TirState, tir_ref: TirInst.IndexRef) !?Value.Index {
    switch (tir_ref) {
        .true_val, .false_val, .null_val => return null,
        _ => {
            const tir_index: TirInst.Index = @intFromEnum(tir_ref);
            const tir_inst = s.tir.instructions.get(tir_index);
            switch (tir_inst) {
                .constant_val => |val| return val,
                else => return null,
            }
        },
    }
}

fn get_enum_field(s: *TirState, enum_agg: Value.TirAggregrate, field_name: Air.StringIndex) !struct { Value.Field, u32 } {
    var enum_field_index = enum_agg.fields_start;
    while (enum_field_index < enum_agg.fields_end) : (enum_field_index += 1) {
        const cur_field = s.tir.values.get(enum_field_index);
        switch (cur_field) {
            .enum_field_typ => |enum_field| {
                if (enum_field.var_name == field_name) {
                    return .{ enum_field, enum_field_index - enum_agg.fields_start };
                }
            },
            else => unreachable,
        }
    }
    return error.InvalidTagName;
}

fn gen_copy_equiv_type(s: *TirState, type_ref: Value.IndexRef) !Value.IndexRef {
    switch (type_ref) {
        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
            return type_ref;
        },
        _ => {
            const tir_type = s.tir.values.get(@intFromEnum(type_ref));
            switch (tir_type) {
                .array_typ => return error.Unimplemented,
                .struct_typ, .enum_typ => |tir_container| {

                    // Reserve space for the field types first
                    const new_fields_start: Value.Index = @intCast(s.tir.values.len);
                    var cur_field_index = tir_container.fields_start;
                    while (cur_field_index < tir_container.fields_end) : (cur_field_index += 1) {
                        _ = try s.append_val(undefined);
                    }

                    cur_field_index = tir_container.fields_start;
                    while (cur_field_index < tir_container.fields_end) : (cur_field_index += 1) {
                        const old_field_type = s.tir.values.get(cur_field_index);
                        var new_field: Value = undefined;
                        switch (old_field_type) {
                            .struct_field_typ => |field| {
                                const new_field_type = try gen_copy_equiv_type(s, field.field_type);
                                new_field = .{ .struct_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
                            },
                            .mut_struct_field_typ => |field| {
                                const new_field_type = try gen_copy_equiv_type(s, field.field_type);
                                new_field = .{ .mut_struct_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
                            },
                            .enum_field_typ => |field| {
                                const new_field_type = try gen_copy_equiv_type(s, field.field_type);
                                new_field = .{ .enum_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
                            },
                            else => unreachable,
                        }
                        s.tir.values.set(new_fields_start + cur_field_index - tir_container.fields_start, new_field);
                    }

                    const new_fields_len: Value.Index = @intCast(tir_container.fields_end - tir_container.fields_start);
                    const new_container_type = if (tir_type == .struct_typ)
                        Value{ .struct_typ = .{ .fields_start = new_fields_start, .fields_end = new_fields_start + new_fields_len } }
                    else
                        Value{ .enum_typ = .{ .fields_start = new_fields_start, .fields_end = new_fields_start + new_fields_len } };
                    const new_container_type_ref = try s.append_val_ref(new_container_type);
                    try print_type(&s.tir, new_container_type_ref);
                    return new_container_type_ref;
                    // old
                    // const struct_fields = s.tir.extra.items[tir_struct.fields_start..struct_typ.fields_end];

                    // // Reserve space for the field types first
                    // const new_struct_fields_start: Type.Index = @intCast(s.tir.types.len);
                    // for (struct_fields) |_| {
                    //     _ = try s.append_type(undefined);
                    // }

                    // print("{}\n", .{s.tir.types.len});
                    // for (struct_fields, 0..) |s_field, i| {
                    //     const old_field_type = s.tir.types.get(s_field);
                    //     var new_field: Type = undefined;
                    //     switch (old_field_type) {
                    //         .struct_field_typ => |field| {
                    //             const new_field_type = try gen_copy_equiv_type(s, field.field_type);
                    //             new_field = .{ .struct_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
                    //         },
                    //         .mut_struct_field_typ => |field| {
                    //             const new_field_type = try gen_copy_equiv_type(s, field.field_type);
                    //             new_field = .{ .mut_struct_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
                    //         },
                    //         .enum_field_typ => |field| {
                    //             const new_field_type = try gen_copy_equiv_type(s, field.field_type);
                    //             new_field = .{ .enum_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
                    //         },
                    //         else => unreachable,
                    //     }
                    //     s.tir.types.set(new_struct_fields_start + i, new_field);
                    //     // const field_type_ref: Type.IndexRef = @enumFromInt(s_field);
                    //     // const copy_equiv_field = try gen_copy_equiv_type(s, field_type_ref);
                    //     // try s.scratch.append(@intFromEnum(copy_equiv_field));
                    // }

                    // const struct_fields_len: Type.Index = @intCast(struct_fields.len);
                    // const new_struct_type = Type{ .struct_typ = .{ .fields_start = new_struct_fields_start, .fields_end = new_struct_fields_start + struct_fields_len } };

                    // const new_struct_type_ref = try s.append_type(new_struct_type);
                    // try print_type(s, new_struct_type_ref);
                    // return new_struct_type_ref;
                },
                .ptr_typ => |ptr| {
                    const new_deref_type = try gen_copy_equiv_type(s, ptr.deref_type);
                    const new_ptr_type = try s.append_val_ref(.{ .ptr_typ = .{ .deref_type = new_deref_type, .cap = .ref_typ } });
                    return new_ptr_type;
                },
                else => unreachable,
            }
        },
    }
}

fn get_aggregate_type(s: *TirState, type_ref: Value.IndexRef, get_enum: bool) !Value.TirAggregrate {
    switch (type_ref) {
        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
            print("Expected aggregrate type, got {}", .{type_ref});
            return error.ExpectedAggregrateType;
        },
        _ => {
            const tir_type = s.tir.values.get(@intFromEnum(type_ref));
            switch (tir_type) {
                .array_typ => return error.Unimplemented,
                .struct_typ => |tir_struct| {
                    if (get_enum == false) {
                        return tir_struct;
                    } else {
                        return error.ExpectedAggregrateType;
                    }
                },
                .enum_typ => |tir_enum| {
                    if (get_enum == true) {
                        return tir_enum;
                    } else {
                        return error.ExpectedAggregrateType;
                    }
                },
                else => {
                    print("Expected aggregrate type, got ", .{});
                    try print_type(&s.tir, type_ref);
                    return error.ExpectedAggregrateType;
                },
            }
        },
    }
}

fn get_val_type(s: *TirState, val_index: Value.Index) !Value.IndexRef {
    const value = s.tir.values.get(val_index);
    switch (value) {
        .unknown_int_val => return .unknown_int_typ,
        .i8_val => return .i8_typ,
        .i16_val => return .i16_typ,
        .i32_val => return .i32_typ,
        .i64_val => return .i64_typ,
        .u8_val => return .u8_typ,
        .u16_val => return .u16_typ,
        .u32_val => return .u32_typ,
        .u64_val => return .u64_typ,
        else => return error.Unimplemented,
    }
}

// Get the type of the resulting value of a TIR instruction reference
fn get_tir_inst_ret_type(s: *TirState, inst_ref: TirInst.IndexRef) !Value.IndexRef {
    switch (inst_ref) {
        .true_val, .false_val => return .boolean_typ,
        .null_val => return .void_typ,
        _ => {
            // The type of index refers to an instruction.
            // By matching on the instruction, figure out what it's type is.
            const inst_index: TirInst.Index = @intFromEnum(inst_ref);
            const inst = s.tir.instructions.get(inst_index);

            switch (inst) {
                .lt_i8, .lt_i16, .lt_i32, .lt_i64, .lt_u8, .lt_u16, .lt_u32, .lt_u64 => return .boolean_typ,
                .block, .ret_void, .ret, .print, .br, .br_either, .match => return error.NoType,
                .fn_def => return error.Unimplemented,
                .enum_project => |project| {
                    // const enum_type = try get_aggregate_type(s, project.enum_type, true);
                    return project.ret_type;

                    // const cur_field_index = enum_type.fields_start + project.tag;
                    // const cur_field = s.tir.types.get(cur_field_index);
                    // const field_type = .cur_fieldenum_field_typ.field_type;
                    // const type_index = try s.append_type(Type{ .ptr_typ = field_type });
                    // return @enumFromInt(type_index);
                },
                .constant_type => |type_ref| return type_ref,
                .constant_val => |val_index| {
                    return get_val_type(s, val_index);
                },
                .alloca => |alloca| {
                    return alloca.ret_type;
                    // const type_index = try s.append_type(Type{ .ptr_typ = type_ref });
                    // return @enumFromInt(type_index);
                },
                .memalloc => |memalloc| {
                    return memalloc.ptr_type;
                },
                .memfree => |memfree| {
                    return memfree.expr_type;
                },
                .zero_array => |type_ref| return type_ref,
                .constant_index => |index| return index.elem_type,
                .index => |index| return index.elem_type,
                .store => |_| {
                    return error.Unimplemented;
                    // TODO: Change stores to store their ptr type rather than their val type
                    // const val_type = try get_tir_inst_ret_type(s, store.val);
                    // const type_index = try s.append_type(Type{ .ptr_typ = val_type });
                    // return @enumFromInt(type_index);
                },
                .load => |load| {
                    return load.type;
                },
                .address_of => |address_of| {
                    return address_of.ptr_type;
                },
                .update_enum_ptr_with_ptr => |update_enum| return get_tir_inst_ret_type(s, update_enum.enum_ptr),
                .update_enum_ptr_with_val => |update_enum| return get_tir_inst_ret_type(s, update_enum.enum_ptr),
                .get_element_ptr => |get_elem_ptr| {
                    return get_elem_ptr.ret_type;
                },
                .move => |move| {
                    const val_type = try get_tir_inst_ret_type(s, @enumFromInt(move));
                    return val_type;
                    // const type_index = try t.append_type(Type{ .ptr_typ = val_type });
                    // return @enumFromInt(type_index);
                },
                // .cap_reduce => |cap_reduce| {
                //     return cap_reduce.ret_type;
                // },
                .add_i8 => return .i8_typ,
                .add_i16 => return .i16_typ,
                .add_i32 => return .i32_typ,
                .add_i64 => return .i64_typ,
                .add_u8 => return .u8_typ,
                .add_u16 => return .u16_typ,
                .add_u32 => return .u32_typ,
                .add_u64 => return .u64_typ,
                .arg => |arg| return arg.typ_ref,
                // .add_i8 => tir.air_tir_map.put(@intFromEnum(air_index), .i8_typ),
                // .add_i8 => tir.air_tir_map.put(@intFromEnum(air_index), .i8_typ),
                // .add_i8 => tir.air_tir_map.put(@intFromEnum(air_index), .i8_typ),
                // else => return error.Unimplemented,
            }
        },
    }
}

fn coerce_unknown_int(unknown_int: i64, target_type: Value.IndexRef) !Value {
    switch (target_type) {
        .i8_typ => {
            if (unknown_int < (1 << 7) and unknown_int >= -(1 << 7)) {
                return Value{ .i8_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .i16_typ => {
            if (unknown_int < (1 << 15) and unknown_int >= -(1 << 15)) {
                return Value{ .i16_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .i32_typ => {
            if (unknown_int < (1 << 31) and unknown_int >= -(1 << 31)) {
                return Value{ .i32_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .i64_typ => {
            if (unknown_int < (1 << 63) and unknown_int >= -(1 << 63)) {
                return Value{ .i64_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .u8_typ => {
            if (unknown_int < (1 << 8)) {
                return Value{ .u8_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .u16_typ => {
            if (unknown_int < (1 << 16)) {
                return Value{ .u16_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .u32_typ => {
            if (unknown_int < (1 << 32)) {
                return Value{ .u32_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .u64_typ => {
            if (unknown_int < (1 << 64)) {
                return Value{ .u64_val = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        else => return error.IntCoercionIntoNonInt,
    }
}

fn tir_gen_blk(s: *TirState, air_blk_index: AirInst.Index, is_top_level: bool) !TirInst.Index {
    const tir_existing_blk = s.air_tir_inst_map.get(@enumFromInt(air_blk_index));
    if (tir_existing_blk) |existing_blk| {
        return @intFromEnum(existing_blk);
    }

    const tir_blk_index = try s.append_inst(undefined);
    var tir_inst = TirInst{ .block = .{ .start = @intCast(s.tir.instructions.len), .end = undefined } };
    try s.air_tir_inst_map.put(@enumFromInt(air_blk_index), tir_blk_index);
    // print("Block begins at {}\n", .{tir_inst.block.start});

    const air_blk = s.air.instructions.get(air_blk_index);

    // print("\nGENERATING BLOCK FROM AIR {d} TO {d}\n", .{ @intFromEnum(air_blk.block.start), @intFromEnum(air_blk.block.end) });

    tir_inst.block.end = try tir_gen_bb(s, @intFromEnum(air_blk.block.start) + 1, true);
    if (is_top_level) {
        tir_inst.block.end = @intCast(s.tir.instructions.len - 1);
    }

    // print("Block ends at {}\n", .{tir_inst.block.end});

    s.tir.instructions.set(@intFromEnum(tir_blk_index), tir_inst);
    return @intFromEnum(tir_blk_index);
}

fn tir_gen_bb(s: *TirState, air_bb_start: AirInst.Index, ret_on_fn_def: bool) TirError!TirInst.Index {
    var air_index: u32 = air_bb_start;
    const air_instructions = s.air.instructions.slice();
    // print("Generating instructions from {}\n", .{air_bb_start});
    while (air_index < s.air.instructions.len) : (air_index += 1) {
        const air_ref: AirInst.IndexRef = @enumFromInt(air_index);
        // print("\n{} Generating TIR for AIR instruction: ", .{s.tir.instructions.len});
        // try air_mod.print_air(s.air, air_index, air_index + 1, 0);
        // print("TIR so far:\n", .{});
        // try print_tir(&s.tir, 0, @intCast(s.tir.instructions.len), 0);

        const air_inst = air_instructions.get(air_index);
        switch (air_inst) {
            .block => |block| {
                // _ = block;
                // return tir_gen_blk(t, air_index);
                // return @intCast(t.instructions.len - 1);
                // return error.Unimplemented;
                air_index = @intFromEnum(block.end) - 1;
            },
            .fn_def => |fn_def_extra| {
                // print("ADDING FN DEF\n", .{});
                const fn_def = s.air.get_extra_struct(AirInst.FnDef, fn_def_extra);
                const tir_ret_type = try s.get_type_mapping(fn_def.ret_type);

                const fn_def_inst_index = try s.append_inst(TirInst{ .fn_def = .{ .name = fn_def.name, .params = undefined, .ret_type = tir_ret_type, .blk = 0 } });
                try s.air_tir_inst_map.put(air_ref, fn_def_inst_index);
                const fn_def_info = TirState.FnDefInfo{ .tir_inst = @intFromEnum(fn_def_inst_index), .air_inst = air_index };
                try s.fn_def_map.put(fn_def.name, fn_def_info);
                const tir_blk = try tir_gen_blk(s, fn_def.blk, false);

                const param_indeces = s.air.extra.items[fn_def.params.start..fn_def.params.end];
                const tir_params_start = s.tir.extra.items.len;
                for (param_indeces) |param_index| {
                    const tir_param = try s.get_inst_mapping(@enumFromInt(param_index));
                    try s.tir.extra.append(@intFromEnum(tir_param));
                }
                const tir_params = TirInst.ExtraSlice{ .start = @intCast(tir_params_start), .end = @intCast(s.tir.extra.items.len) };

                const tir_fn_def = TirInst{ .fn_def = .{ .name = fn_def.name, .params = tir_params, .ret_type = tir_ret_type, .blk = tir_blk } };
                s.tir.instructions.set(@intFromEnum(fn_def_inst_index), tir_fn_def);
                if (ret_on_fn_def) {
                    return tir_blk;
                }
            },
            .fn_call => |fn_call| {
                const fn_def_info = try s.get_fn_def_mapping(fn_call.name);
                // const fn_def_tir = s.tir.instructions.get(.fn_def_infoinst_typ).fn_def;
                const fn_def_air = s.air.get_extra_struct(AirInst.FnDef, s.air.instructions.get(fn_def_info.air_inst).fn_def);

                const air_param_indeces = s.air.extra.items[fn_def_air.params.start..fn_def_air.params.end];
                // const param_indeces = s.tir.extra.items[fn_def_tir.params.start..fn_def_tir.params.end];
                const arg_indeces = s.air.extra.items[fn_call.args_start..fn_call.args_end];

                const is_comptime = true;
                var extra: u32 = 0;
                // while (extra < arg_indeces.len) : (extra += 1) {
                // const a_index = arg_indeces[extra];
                // const p_index = param_indeces[extra];

                // const arg = try s.get_type_mapping(@enumFromInt(a_index));
                // const arg_val = try get_val(s, arg);
                // if (arg_val == null) {
                //     is_comptime = false;
                // }

                // const param = try s.
                // }

                if (is_comptime) {
                    extra = 0;
                    // Remove the previous mapping
                    const fn_def_block = s.air.instructions.get(fn_def_air.blk).block;
                    const fn_def_end: AirInst.Index = @intFromEnum(fn_def_block.end);
                    var fn_def_index = fn_def_info.air_inst;
                    while (fn_def_index < fn_def_end) : (fn_def_index += 1) {
                        // print("Removing mappin for {}\n", .{fn_def_index});
                        _ = s.air_tir_inst_map.remove(@enumFromInt(fn_def_index));
                        _ = s.air_tir_type_map.remove(@enumFromInt(fn_def_index));
                    }

                    // Rebind args to comptime-values
                    while (extra < arg_indeces.len) : (extra += 1) {
                        const a_index = arg_indeces[extra];

                        const arg = try s.get_type_mapping(@enumFromInt(a_index));
                        // print("Remapping {} to {}\n", .{ air_param_indeces[extra], arg });
                        try s.air_tir_type_map.put(@enumFromInt(air_param_indeces[extra]), arg);
                    }

                    _ = s.air_tir_inst_map.remove(@enumFromInt(fn_def_air.blk));

                    const blk_index = try tir_gen_bb(s, fn_def_info.air_inst, true);
                    // try print_tir(&s.tir, air_index, @intCast(s.tir.instructions.len), 0);

                    // print("blk index {}\n", .{blk_index});
                    const blk = s.tir.instructions.get(blk_index).block;
                    const ret_inst = s.tir.instructions.get(blk.end).ret;
                    try s.air_tir_type_map.put(air_ref, ret_inst.val);
                    // if (ret_inst.is_ref().val)) {
                    //     // const air_fn_def_blk = s.air.instructions.get(fn_def_air.blk).block;
                    //     // const air_ret_inst: AirInst.Index = @intFromEnum(air_fn_def_blk.end) - 1;
                    //     // print("Air ret inst: {}\n", .{air_ret_inst});
                    //     // // const ret_val_inst = s.tir.instructions.get(@intFromEnum(ret_inst.val));
                    //     // const ret_val_type = s.air_tir_type_map.get(@enumFromInt(air_ret_inst)).?;
                    // } else {
                    //     return error.Unimplemented;
                    // }
                    // if (ret_inst.is_ref().val)) {
                    //     const ret_val_inst = s.tir.instructions.get(@intFromEnum(ret_inst.val));
                    //     try s.air_tir_type_map.put(@enumFromInt(air_param_indeces[extra]), ret_val_inst.constant_type);
                    // } else {
                    //     return error.Unimplemented;
                    // }
                } else {
                    return error.Unimplemented;
                }
            },
            .struct_def, .enum_def => |air_container_def| {
                const type_info = @typeInfo(AirInst.DeclInfo);
                const tir_container_type_index = try s.append_val_ref(undefined);
                try s.air_tir_type_map.put(air_ref, tir_container_type_index);

                const decl_info_field_count: Air.ExtraIndex = @intCast(type_info.Struct.fields.len);
                var extra = air_container_def.start;
                const tir_fields_start: Value.Index = @intCast(s.tir.values.len);
                while (extra < air_container_def.end) {
                    const field_info = s.air.get_extra_struct(AirInst.DeclInfo, extra);

                    const tir_field_type = try s.get_type_mapping(field_info.type_inst);
                    // orelse {
                    // // mapping might be missing for recursive data structures
                    // if (field_info.type_inst.is_ref()){
                    //     const inst =
                    //     if (maybe_ptr )
                    // } else {
                    //     return error.MissingMapping;
                    // }
                    // };
                    var tir_field: Value = undefined;

                    if (air_inst == .struct_def) {
                        if (field_info.mutable) {
                            tir_field = Value{ .mut_struct_field_typ = .{ .var_name = field_info.var_name, .field_type = tir_field_type } };
                        } else {
                            tir_field = Value{ .struct_field_typ = .{ .var_name = field_info.var_name, .field_type = tir_field_type } };
                        }
                    } else {
                        tir_field = Value{ .enum_field_typ = .{ .var_name = field_info.var_name, .field_type = tir_field_type } };
                    }

                    _ = try s.append_val(tir_field);

                    extra += decl_info_field_count;
                }
                const tir_fields_end: Value.Index = @intCast(s.tir.values.len);

                var tir_container: Value = undefined;
                if (air_inst == .struct_def) {
                    tir_container = Value{ .struct_typ = .{ .fields_start = tir_fields_start, .fields_end = tir_fields_end } };
                } else {
                    tir_container = Value{ .enum_typ = .{ .fields_start = tir_fields_start, .fields_end = tir_fields_end } };
                }
                s.tir.values.set(@intFromEnum(tir_container_type_index), tir_container);
            },
            .update_enum_ptr => |air_update_enum| {
                const tir_enum_ptr_inst = try s.get_inst_mapping(air_update_enum.ptr);
                const tir_enum_ptr_type = try get_tir_inst_ret_type(s, tir_enum_ptr_inst);

                const tir_enum_type = s.tir.values.get(@intFromEnum(tir_enum_ptr_type)).ptr_typ.deref_type;
                const tir_enum = try get_aggregate_type(s, tir_enum_type, true);

                // TODO: Use get_enum_field
                var tag: u32 = undefined;
                var tag_field_type: Value.IndexRef = undefined;
                var enum_field_index = tir_enum.fields_start;
                while (enum_field_index < tir_enum.fields_end) : (enum_field_index += 1) {
                    const cur_field = s.tir.values.get(enum_field_index);
                    switch (cur_field) {
                        .enum_field_typ,
                        => |enum_field| {
                            if (enum_field.var_name == air_update_enum.new_tag) {
                                tag = enum_field_index - tir_enum.fields_start;
                                tag_field_type = enum_field.field_type;
                                break;
                            }
                        },
                        else => unreachable,
                    }
                }

                const tir_tag_contents = try s.get_inst_mapping(air_update_enum.tag_contents);
                // TODO: This will remove booleans
                const tir_tag_contents_val = get_val_index(s, tir_tag_contents) catch null;
                if (tir_tag_contents_val) |val_index| {
                    const val_type = try get_val_type(s, val_index);
                    if (type_ref_eq(s, val_type, tag_field_type, true) == false) {
                        return error.MismatchedEnumFieldType;
                    }

                    const inst = TirInst{ .update_enum_ptr_with_val = .{ .enum_ptr = tir_enum_ptr_inst, .enum_type = tir_enum_type, .new_tag = tag, .new_tag_val = tir_tag_contents } };
                    const inst_ref = try s.append_inst(inst);
                    try s.air_tir_inst_map.put(air_ref, inst_ref);
                } else {
                    const contents_type_ref = try get_tir_inst_ret_type(s, tir_tag_contents);
                    switch (contents_type_ref) {
                        .typ_typ => return error.Unimplemented,
                        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
                            if (type_ref_eq(s, contents_type_ref, tag_field_type, false) == false) {
                                return error.MismatchedEnumFieldType;
                            }

                            const inst = TirInst{ .update_enum_ptr_with_val = .{ .enum_ptr = tir_enum_ptr_inst, .enum_type = tir_enum_type, .new_tag = tag, .new_tag_val = tir_tag_contents } };
                            const inst_ref = try s.append_inst(inst);
                            try s.air_tir_inst_map.put(air_ref, inst_ref);
                        },
                        _ => {
                            const contents_type = s.tir.values.get(@intFromEnum(contents_type_ref));
                            if (contents_type != .ptr_typ) {
                                unreachable;
                            }
                            if (type_ref_eq(s, contents_type_ref, tag_field_type, false) == false and type_ref_eq_stackref_coerce(s, contents_type_ref, tag_field_type) == false) {
                                return error.MismatchedEnumFieldType;
                            }

                            const inst = TirInst{ .update_enum_ptr_with_ptr = .{ .enum_ptr = tir_enum_ptr_inst, .enum_type = tir_enum_type, .new_tag = tag, .new_tag_ptr = tir_tag_contents } };
                            const inst_ref = try s.append_inst(inst);
                            try s.air_tir_inst_map.put(air_ref, inst_ref);
                            // One level of indirection via pointer is a stack struct or enum
                            // switch (contents_type.ptr) {
                            //     .typ_typ => return error.Unimplemented,
                            //     .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ => return error.Unimplemented,
                            //     _ => {
                            //         const pointed_to_typ = t.types.get(@intFromEnum(contents_type));
                            //     },
                            // }

                        },
                    }
                }
            },

            .get_element_ptr => |air_extra| {
                const air_get_elem = s.air.get_extra_struct(AirInst.GetElementPtr, air_extra);
                const tir_aggregate_inst = try s.get_inst_mapping(air_get_elem.aggregate_ptr);

                // print("Tir aggregrate inst {}\n", .{tir_aggregate_inst});
                const tir_aggregate_ptr_ref = try get_tir_inst_ret_type(s, tir_aggregate_inst);
                const tir_aggregrate_ptr_type = s.tir.values.get(@intFromEnum(tir_aggregate_ptr_ref));

                if (tir_aggregrate_ptr_type != .ptr_typ) {
                    print("Expected pointer to aggregrate, got {} from {}\n", .{ tir_aggregrate_ptr_type, tir_aggregate_inst });
                    return error.Unimplemented;
                }

                const tir_aggregate_type = tir_aggregrate_ptr_type.ptr_typ.deref_type;
                const tir_struct = try get_aggregate_type(s, tir_aggregate_type, false);
                const access_field_indeces_start: Tir.ExtraIndex = @intCast(s.tir.extra.items.len);

                var cur_access_field = air_get_elem.fields.start;
                var field_type: ?Value.IndexRef = null;
                while (cur_access_field < air_get_elem.fields.end) : (cur_access_field += 1) {
                    const access_field_name: Air.StringIndex = s.air.extra.items[cur_access_field];
                    // const field_index

                    // The numerical index of the field access. If the the field is the first in
                    // in the aggregate, this would 0, second would be 1 etc.
                    var access_field_index: ?u32 = null;

                    // TODO, change tir_struct for multiple field accesses.
                    var struct_field_index = tir_struct.fields_start;
                    while (struct_field_index < tir_struct.fields_end) : (struct_field_index += 1) {
                        const cur_field = s.tir.values.get(struct_field_index);
                        switch (cur_field) {
                            .struct_field_typ, .mut_struct_field_typ => |struct_field| {
                                if (struct_field.var_name == access_field_name) {
                                    access_field_index = struct_field_index - tir_struct.fields_start;
                                    field_type = struct_field.field_type;
                                    break;
                                }
                            },
                            else => unreachable,
                        }
                    }

                    if (access_field_index) |a_index| {
                        // print(" ACCESS FIELD INDEX {} {}\n", .{ a_index, access_field_indeces_start });
                        try s.tir.extra.append(a_index);
                    } else {
                        return error.UndefinedVar;
                    }
                }
                const access_field_indeces_end: Tir.ExtraIndex = @intCast(s.tir.extra.items.len);
                const ptr_to_field_type = try s.append_val(Value{ .ptr_typ = .{ .deref_type = field_type.?, .cap = .ref_typ } });

                const tir_get_elem = TirInst{ .get_element_ptr = .{ .aggregate_ptr = tir_aggregate_inst, .aggregate_type = tir_aggregate_type, .indeces_start = access_field_indeces_start, .indeces_end = access_field_indeces_end, .ret_type = @enumFromInt(ptr_to_field_type) } };
                const tir_get_elem_inst = try s.append_inst(tir_get_elem);
                try s.air_tir_inst_map.put(air_ref, tir_get_elem_inst);
            },
            .match => |air_match| {
                const tir_enum_ptr_inst = try s.get_inst_mapping(@enumFromInt(air_match.enum_ptr));
                const tir_enum_ptr_type = try get_tir_inst_ret_type(s, tir_enum_ptr_inst);

                const tir_enum_type = s.tir.values.get(@intFromEnum(tir_enum_ptr_type)).ptr_typ.deref_type;
                const tir_enum = try get_aggregate_type(s, tir_enum_type, true);
                const num_cases: u32 = @intCast(tir_enum.fields_end - tir_enum.fields_start);

                const cases_field_count: Air.ExtraIndex = @intCast(@typeInfo(AirInst.MatchCase).Struct.fields.len);

                const tir_match_inst_index = try s.append_inst(undefined);
                try s.air_tir_inst_map.put(air_ref, tir_match_inst_index);

                const cases_start: u32 = @intCast(s.tir.extra.items.len);
                try s.tir.extra.resize(s.tir.extra.items.len + num_cases + 1);
                var case_index = air_match.cases_start;
                while (case_index < air_match.cases_end) : (case_index += cases_field_count) {
                    const case = s.air.get_extra_struct(AirInst.MatchCase, case_index);
                    // print("tag case begin\n", .{});
                    const case_blk = try tir_gen_blk(s, case.blk, false);

                    const enum_field_tuple = try get_enum_field(s, tir_enum, case.tag);
                    // const enum_field = enum_field_tuple[0];
                    const tag_num = enum_field_tuple[1];

                    // print("tag case end {}\n", .{tag_num});
                    s.tir.extra.items[cases_start + tag_num] = case_blk;
                }
                const tir_match = TirInst{ .match = .{ .enum_ptr = tir_enum_ptr_inst, .cases_start = cases_start, .cases_end = cases_start + num_cases } };
                s.tir.instructions.set(@intFromEnum(tir_match_inst_index), tir_match);
                return @intFromEnum(tir_match_inst_index);
            },
            .enum_project => |air_project| {
                const tir_enum_ptr_inst = try s.get_inst_mapping(@enumFromInt(air_project.enum_ptr));
                const tir_enum_ptr_type = try get_tir_inst_ret_type(s, tir_enum_ptr_inst);

                const tir_enum_type = s.tir.values.get(@intFromEnum(tir_enum_ptr_type)).ptr_typ.deref_type;
                const tir_enum = try get_aggregate_type(s, tir_enum_type, true);
                const enum_field_tuple = try get_enum_field(s, tir_enum, air_project.tag);

                const tag = enum_field_tuple[1];

                // Figure out the return type of the project instruction.
                // It is a pointer to the data structure of the tag projecting into.
                const cur_field_index = tir_enum.fields_start + tag;
                const cur_field = s.tir.values.get(cur_field_index);
                const field_type = cur_field.enum_field_typ.field_type;
                const type_index = try s.append_val(Value{ .ptr_typ = .{ .deref_type = field_type, .cap = .ref_typ } });

                // Finally, append the instruction.
                const tir_project = TirInst{ .enum_project = .{ .enum_ptr = @intFromEnum(tir_enum_ptr_inst), .enum_type = tir_enum_type, .tag = enum_field_tuple[1], .ret_type = @enumFromInt(type_index) } };
                const tir_project_inst = try s.append_inst(tir_project);
                try s.air_tir_inst_map.put(air_ref, tir_project_inst);
            },
            .ret_empty => {
                // TODO: Typecheck
                const tir_ret_inst = try s.append_inst(.ret_void);
                try s.air_tir_inst_map.put(air_ref, tir_ret_inst);
                return @intFromEnum(tir_ret_inst);
            },
            .ret => |ret| {
                // const tir_ret_val = try s.get_inst_mapping(ret);
                const tir_ret_val = try s.get_type_mapping(ret);
                const tir_ret_inst = try s.append_inst(TirInst{ .ret = .{ .val = tir_ret_val, .ret_type = .typ_typ } });
                try s.air_tir_inst_map.put(air_ref, tir_ret_inst);
                return @intFromEnum(tir_ret_inst);
            },
            .br => |br| {
                const air_dst: AirInst.Index = @intFromEnum(br);

                if (s.air_tir_inst_map.get(@enumFromInt(air_dst))) |tir_dst| {

                    // const tir_mapped_inst = t.instructions.get(@intFromEnum(tir_dst));
                    // switch (tir_mapped_inst) {
                    //     .block => |blk| {

                    //     }
                    // }

                    const tir_br_index = try s.append_inst(.{ .br = @intFromEnum(tir_dst) });
                    try s.air_tir_inst_map.put(air_ref, tir_br_index);
                    return @intFromEnum(tir_br_index);
                } else {
                    const tir_br_index = try s.append_inst(undefined);
                    try s.air_tir_inst_map.put(
                        air_ref,
                        tir_br_index,
                    );

                    _ = try tir_gen_blk(s, air_dst, false);

                    const br_dest = try s.get_inst_mapping(br);
                    s.tir.instructions.set(@intFromEnum(tir_br_index), .{ .br = @intFromEnum(br_dest) });
                    return @intFromEnum(tir_br_index);
                }
            },
            // .br_either => |br_either| {
            //     const br = t.air.get_extra_struct(AirInst.BrEither, br_either);
            //     const tir_cond = try t.get_inst_mapping(br.cond);
            //     const tir_cond_val = try get_val(t, tir_cond);

            //     const then_blk = t.air.instructions.get(@intFromEnum(br.then_blk));
            //     const else_blk = t.air.instructions.get(@intFromEnum(br.else_blk));
            //     if (tir_cond_val) |val| {
            //         switch (val) {
            //             .boolean => |boolean| {
            //                 if (boolean) {
            //                     try tir_gen_instructions(t, @intFromEnum(then_blk.block.start), @intFromEnum(then_blk.block.end));
            //                 }
            //                 const else_end: u32 = @intFromEnum(else_blk.block.end);
            //                 air_index = else_end;
            //             },
            //             else => return error.ExpectedBoolean,
            //         }
            //     } else {

            //         // Compile-time evaluation not possible.
            //         const cond_typ = try get_tir_inst_ret_type(t, tir_cond);
            //         if (cond_typ != .boolean_typ) {
            //             return error.ExpectedBoolean;
            //         }
            //         var tir_br_inst = TirInst{ .br = .{ .cond = @intFromEnum(tir_cond), .then_blk = undefined, .else_blk = undefined } };
            //         const inst_index = try t.append_inst(tir_br_inst);

            //         tir_br_inst.br.then_blk = try tir_gen_blk(t, @intFromEnum(then_blk.block.start), @intFromEnum(then_blk.block.end));
            //         tir_br_inst.br.else_blk = try tir_gen_blk(t, @intFromEnum(then_blk.block.start), @intFromEnum(then_blk.block.end));
            //         t.instructions.set(@intFromEnum(inst_index), tir_br_inst);
            //     }
            // },
            .br_either => |br_either| {
                const br = s.air.get_extra_struct(AirInst.BrEither, br_either);
                const tir_cond = try s.get_inst_mapping(br.cond);
                const tir_cond_val = try get_val(s, tir_cond);

                const then_blk = s.air.instructions.get(@intFromEnum(br.then_blk));
                const else_blk = s.air.instructions.get(@intFromEnum(br.else_blk));
                if (tir_cond_val) |val| {
                    // print("Inling br either \n", .{});
                    switch (val) {
                        .boolean_val => |boolean| {
                            if (boolean) {
                                return tir_gen_bb(s, @intFromEnum(then_blk.block.start) + 1, true);
                                // tir_blk_index = try tir_gen_blk(t, @intFromEnum(br.then_blk));
                            } else {
                                return tir_gen_bb(s, @intFromEnum(else_blk.block.start) + 1, true);
                                // tir_blk_index = try tir_gen_blk(t, @intFromEnum(br.else_blk));
                            }
                            // const tir_blk_end = t.instructions.get(tir_blk_index).block.end;
                        },
                        else => return error.ExpectedBoolean,
                    }
                } else {
                    // print("Inling br either not possible\n", .{});
                    // Compile-time evaluation not possible.
                    const cond_typ = try get_tir_inst_ret_type(s, tir_cond);
                    if (cond_typ != .boolean_typ) {
                        return error.ExpectedBoolean;
                    }
                    var tir_br_inst = TirInst{ .br_either = .{ .cond = @intFromEnum(tir_cond), .then_blk = undefined, .else_blk = undefined } };
                    const inst_index = try s.append_inst(tir_br_inst);

                    try s.air_tir_inst_map.put(air_ref, inst_index);
                    tir_br_inst.br_either.then_blk = try tir_gen_blk(s, @intFromEnum(br.then_blk), false);
                    tir_br_inst.br_either.else_blk = try tir_gen_blk(s, @intFromEnum(br.else_blk), false);
                    s.tir.instructions.set(@intFromEnum(inst_index), tir_br_inst);
                    return @intFromEnum(inst_index);
                }
            },
            .lt => |lt| {
                var lhs_tir_ref: TirInst.IndexRef = try s.get_inst_mapping(lt.lhs);
                const lhs_val = try get_val(s, lhs_tir_ref);
                var rhs_tir_ref: TirInst.IndexRef = try s.get_inst_mapping(lt.rhs);
                var lhs_typ = try get_tir_inst_ret_type(s, lhs_tir_ref);
                var rhs_typ = try get_tir_inst_ret_type(s, rhs_tir_ref);

                const rhs_val = try get_val(s, rhs_tir_ref);
                if (lhs_val) |l_val| {
                    if (rhs_val) |r_val| {
                        var c_l_val = l_val;
                        var c_r_val = r_val;
                        if (l_val == .unknown_int_val and r_val != .unknown_int_val) {
                            c_l_val = try coerce_unknown_int(l_val.unknown_int_val, rhs_typ);
                        } else if (l_val != .unknown_int_val and r_val == .unknown_int_val) {
                            c_r_val = try coerce_unknown_int(r_val.unknown_int_val, lhs_typ);
                        }

                        if (c_l_val == .unknown_int_val and c_r_val == .unknown_int_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.unknown_int_val < c_r_val.unknown_int_val }, air_index);
                        } else if (c_l_val == .i8_val and c_r_val == .i8_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.i8_val < c_r_val.i8_val }, air_index);
                        } else if (c_l_val == .i16_val and c_r_val == .i16_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.i16_val < c_r_val.i16_val }, air_index);
                        } else if (c_l_val == .i32_val and c_r_val == .i32_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.i32_val < c_r_val.i32_val }, air_index);
                        } else if (c_l_val == .i64_val and c_r_val == .i64_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.i64_val < c_r_val.i64_val }, air_index);
                        } else if (c_l_val == .u8_val and c_r_val == .u8_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.u8_val < c_r_val.u8_val }, air_index);
                        } else if (c_l_val == .u16_val and c_r_val == .u16_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.u16_val < c_r_val.u16_val }, air_index);
                        } else if (c_l_val == .u32_val and c_r_val == .u32_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.u32_val < c_r_val.u32_val }, air_index);
                        } else if (c_l_val == .u64_val and c_r_val == .u64_val) {
                            try s.append_constant_val(Value{ .boolean_val = c_l_val.u64_val < c_r_val.u64_val }, air_index);
                        } else {
                            return error.Unimplemented;
                        }
                        continue;
                    }
                }

                // Compile-time evaluation not possible.
                if (lhs_val) |l_val| {
                    if (l_val == .unknown_int_val) {
                        const coerced_val = try coerce_unknown_int(l_val.unknown_int_val, rhs_typ);
                        const val_index = try s.append_val(coerced_val);
                        lhs_tir_ref = try s.append_inst(.{ .constant_val = val_index });
                        lhs_typ = rhs_typ;
                    }
                }
                if (rhs_val) |r_val| {
                    if (r_val == .unknown_int_val) {
                        const coerced_val = try coerce_unknown_int(r_val.unknown_int_val, lhs_typ);
                        const val_index = try s.append_val(coerced_val);
                        rhs_tir_ref = try s.append_inst(.{ .constant_val = val_index });
                        rhs_typ = lhs_typ;
                    }
                }

                var res_inst: TirInst.IndexRef = undefined;
                if (lhs_typ == .i8_typ and rhs_typ == .i8_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_i8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .i16_typ and rhs_typ == .i16_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_i16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .i32_typ and rhs_typ == .i32_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_i32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .i64_typ and rhs_typ == .i64_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_i64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u8_typ and rhs_typ == .u8_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_u8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u16_typ and rhs_typ == .u16_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_u16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u32_typ and rhs_typ == .u32_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_u32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u64_typ and rhs_typ == .u64_typ) {
                    res_inst = try s.append_inst(TirInst{ .lt_u64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else {
                    return error.ComparisionTypeError;
                }
                try s.air_tir_inst_map.put(air_ref, res_inst);
            },

            .add => |air_add| {
                var lhs_tir_ref: TirInst.IndexRef = s.air_tir_inst_map.get(air_add.lhs) orelse return error.MissingMapping;
                const lhs_val = try get_val(s, lhs_tir_ref);
                var rhs_tir_ref: TirInst.IndexRef = s.air_tir_inst_map.get(air_add.rhs) orelse return error.MissingMapping;
                const rhs_val = try get_val(s, rhs_tir_ref);
                if (lhs_val) |l_val| {
                    if (rhs_val) |r_val| {
                        if (l_val == .unknown_int_val and r_val == .unknown_int_val) {
                            try s.append_constant_val(Value{ .unknown_int_val = l_val.unknown_int_val + r_val.unknown_int_val }, air_index);
                            continue;
                        } else {
                            return error.Unimplemented;
                        }
                    }
                }

                // Compile-time evaluation not possible.
                var lhs_typ = try get_tir_inst_ret_type(s, lhs_tir_ref);
                var rhs_typ = try get_tir_inst_ret_type(s, rhs_tir_ref);

                if (lhs_val) |l_val| {
                    if (l_val == .unknown_int_val) {
                        const coerced_val = try coerce_unknown_int(l_val.unknown_int_val, rhs_typ);
                        const val_index = try s.append_val(coerced_val);
                        lhs_tir_ref = try s.append_inst(.{ .constant_val = val_index });
                        lhs_typ = rhs_typ;
                    }
                }
                if (rhs_val) |r_val| {
                    if (r_val == .unknown_int_val) {
                        const coerced_val = try coerce_unknown_int(r_val.unknown_int_val, lhs_typ);
                        const val_index = try s.append_val(coerced_val);
                        rhs_tir_ref = try s.append_inst(.{ .constant_val = val_index });
                        rhs_typ = lhs_typ;
                    }
                }

                var res_inst: TirInst.IndexRef = undefined;
                // TODO: Just perform an direct comparison

                if (lhs_typ == .i8_typ and rhs_typ == .i8_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_i8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .i16_typ and rhs_typ == .i16_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_i16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .i32_typ and rhs_typ == .i32_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_i32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .i64_typ and rhs_typ == .i64_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_i64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u8_typ and rhs_typ == .u8_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_u8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u16_typ and rhs_typ == .u16_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_u16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u32_typ and rhs_typ == .u32_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_u32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .u64_typ and rhs_typ == .u64_typ) {
                    res_inst = try s.append_inst(TirInst{ .add_u64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else {
                    print("Addition types are mismatched {} and {}\n", .{ lhs_typ, rhs_typ });
                    print("TIR Lhs is from{} and rhs is from {}\n", .{ lhs_tir_ref, rhs_tir_ref });
                    print("AIR Lhs is from{} and rhs is from {}\n", .{ air_add.lhs, air_add.rhs });
                    return error.AdditionTypeError;
                }
                try s.air_tir_inst_map.put(air_ref, res_inst);
            },
            .type_of, .type_of_deref => |air_type_inst| {
                const maybe_mapped_type: ?Value.IndexRef = s.get_type_mapping(air_type_inst) catch null;
                if (maybe_mapped_type) |_| {
                    try s.air_tir_type_map.put(air_ref, .typ_typ);
                } else {
                    const tir_expr_inst = try s.get_inst_mapping(air_type_inst);
                    const tir_expr_type = try get_tir_inst_ret_type(s, tir_expr_inst);

                    if (air_inst == .type_of) {
                        try s.air_tir_type_map.put(air_ref, tir_expr_type);
                    } else {
                        const ptr_type = s.tir.values.get(@intFromEnum(tir_expr_type)).ptr_typ.deref_type;
                        try s.air_tir_type_map.put(air_ref, ptr_type);
                    }
                }
            },
            .type_as => |type_as| {
                const tir_type_ref = try s.get_type_mapping(type_as.type);

                // An expression might evaluate to a type
                // print("Expr is {}\n", .{type_as.expr});
                const maybe_expr_type = s.get_type_mapping(type_as.expr) catch null;
                // print("Type mapping {any}\n", .{maybe_expr_type});
                if (maybe_expr_type) |expr_type| {
                    // print("Made it here\n", .{});
                    switch (expr_type) {
                        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
                            if (tir_type_ref == .typ_typ) {
                                try s.air_tir_type_map.put(air_ref, expr_type);
                                continue;
                            }
                        },
                        .typ_typ => return error.TypeOfType,
                        _ => {
                            if (tir_type_ref == .typ_typ) {
                                try s.air_tir_type_map.put(air_ref, expr_type);
                                const expr_type_inst = try s.append_inst(.{ .constant_type = expr_type });
                                try s.air_tir_inst_map.put(air_ref, expr_type_inst);
                                continue;
                            }
                        },
                    }
                }
                // Otherwise, the expression is a comptime value or runtime value
                const tir_expr_ref = try s.get_inst_mapping(type_as.expr);
                const maybe_val = try get_val(s, tir_expr_ref);
                if (maybe_val) |val| {
                    switch (tir_type_ref) {
                        .i8_typ,
                        .i16_typ,
                        .i32_typ,
                        .i64_typ,
                        .u8_typ,
                        .u16_typ,
                        .u32_typ,
                        .u64_typ,
                        => {
                            if (val == .unknown_int_val) {
                                const coerced_val = try coerce_unknown_int(val.unknown_int_val, tir_type_ref);
                                try s.append_constant_val(coerced_val, air_index);
                            } // const coerced_val = try coerce_unknown_int(val.unknown_int, tir_type_ref);
                            // try s.append_constant_val(val, air_index);
                        },
                        .unknown_int_typ => {
                            if (val == .unknown_int_val) {
                                try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                            }
                        },
                        .typ_typ => {
                            try s.air_tir_type_map.put(air_ref, tir_type_ref);
                        },
                        else => {
                            // print("Val : {}\n", .{val});

                            print("Type as for ", .{});
                            try print_type(&s.tir, tir_type_ref);
                            print(" not impl.\n", .{});
                            return error.Unimplemented;
                        },
                    }
                } else {
                    const expr_type_ref = try get_tir_inst_ret_type(s, tir_expr_ref);
                    // const expr_inst = s.tir.instructions.get(@intFromEnum(tir_expr_ref));

                    if (type_ref_eq(s, expr_type_ref, tir_type_ref, false)) {
                        try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                    } else if (type_ref_eq_stackref_coerce(s, expr_type_ref, tir_type_ref)) {
                        try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                    } else if (type_ref_eq_stackref_coerce(s, tir_type_ref, expr_type_ref)) {
                        try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                    } else {
                        return error.MismatchedTypes;
                    }
                    // } else if (type_ref_eq(s, expr_type_ref, tir_type_ref, true)) {
                    //     // Expression type is own pointer, type as wants ref pointer.
                    //     // Emit cap. reduce instruction
                    //     const expr_type = s.tir.types.get(@intFromEnum(expr_type_ref));
                    //     const reduced_cap_type = try s.append_type(.{ .ptr_typ = .{ .deref_type = expr_type.ptr.deref_type, .cap = .ref_typ } });
                    //     const cap_reduce_inst = try s.append_inst(TirInst{ .cap_reduce = .{ .expr = @intFromEnum(tir_expr_ref), .ret_type = reduced_cap_type } });
                    //     try s.air_tir_inst_map.put(air_ref, cap_reduce_inst);
                    //     // try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                    // } else if (type_ref_eq_stackref_coerce(s, expr_type_ref, tir_type_ref)) {
                    //     // return error.Unimplemented;
                    //     try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                    // } else {
                    //     // print("{}\n", .{expr_inst});
                    //     print("Type coercion not possible: \n", .{});
                    //     print_type(s, expr_type_ref) catch unreachable;
                    //     print(" from {}\n", .{tir_expr_ref});
                    //     print_type(s, tir_type_ref) catch unreachable;
                    //     print("\n", .{});
                    //     return error.MismatchedTypes;
                    // }
                }
            },
            .deref => |air_deref| {
                const tir_deref_target = try s.get_inst_mapping(air_deref);
                const target_type_ref = try get_tir_inst_ret_type(s, tir_deref_target);
                if (target_type_ref.is_ref() == false) {
                    return error.DerefOnPrimitive;
                } else {
                    const target_type = s.tir.values.get(@intFromEnum(target_type_ref));
                    if (target_type == .ptr_typ and target_type.ptr_typ.cap != .stackref_typ) {
                        // const tir_load = try s.append_inst(TirInst{ .load = .{ .ptr_typ = tir_deref_target, .type = target_type.ptr.deref_type } });
                        try s.air_tir_inst_map.put(air_ref, tir_deref_target);
                    } else {
                        return error.DerefOnInvalidType;
                    }
                }
            },
            .alloca => |air_alloc| {
                const tir_alloc_type = try s.get_type_mapping(air_alloc.type);
                const ret_type_index = try s.append_val_ref(Value{ .ptr_typ = .{ .deref_type = tir_alloc_type, .cap = .stackref_typ } });

                const tir_alloca = try s.append_inst(.{ .alloca = .{ .alloc_type = tir_alloc_type, .ret_type = ret_type_index } });
                try s.air_tir_inst_map.put(air_ref, tir_alloca);
            },
            .memalloc => |air_memalloc| {
                const tir_expr = try s.get_inst_mapping(air_memalloc.expr);
                const tir_expr_type_ref = try get_tir_inst_ret_type(s, tir_expr);

                const tir_expr_type = s.tir.values.get(@intFromEnum(tir_expr_type_ref));
                if (tir_expr_type == .ptr_typ and tir_expr_type.ptr_typ.cap == .stackref_typ) {
                    // const ret_type = try t.append_type(Type{ .ptr_typ = .{.deref_type = tir_expr_type_ref, .cap = } });
                    const ptr_type_ref = try s.append_val_ref(Value{ .ptr_typ = .{ .deref_type = tir_expr_type.ptr_typ.deref_type, .cap = .own_typ } });
                    const tir_memalloc = try s.append_inst(.{ .memalloc = .{ .expr = tir_expr, .ptr_type = ptr_type_ref } });
                    try s.air_tir_inst_map.put(air_ref, tir_memalloc);
                } else {
                    return error.Unimplemented;
                }
            },
            .memfree => |air_memfree| {
                const tir_ptr = try s.get_inst_mapping(air_memfree.expr);
                const tir_ptr_type_ref = try get_tir_inst_ret_type(s, tir_ptr);

                const tir_ptr_type = s.tir.values.get(@intFromEnum(tir_ptr_type_ref));
                if (tir_ptr_type == .ptr_typ and tir_ptr_type.ptr_typ.cap == .own_typ) {
                    const expr_type = tir_ptr_type.ptr_typ.deref_type;
                    const ptr_type_ref = try s.append_val_ref(Value{ .ptr_typ = .{ .deref_type = expr_type, .cap = .stackref_typ } });
                    const tir_memfree = try s.append_inst(.{ .memfree = .{ .ptr = tir_ptr, .expr_type = ptr_type_ref } });
                    try s.air_tir_inst_map.put(air_ref, tir_memfree);
                } else {
                    return error.MemfreeOnInvalidType;
                }
            },
            .print => |p| {
                const tir_expr = try s.get_inst_mapping(p.expr);
                const tir_print = try s.append_inst(.{ .print = .{ .val = tir_expr } });
                try s.air_tir_inst_map.put(air_ref, tir_print);
            },
            .address_of => |air_address_of| {
                // We only allow address of types
                const tir_target_type = if (air_address_of.target == .address_of_self)
                    .opaque_typ
                else
                    try s.get_type_mapping(air_address_of.target);

                const tir_cap_type = try s.get_type_mapping(air_address_of.cap);
                const tir_address_type = try s.append_val_ref(Value{ .ptr_typ = .{ .deref_type = tir_target_type, .cap = tir_cap_type } });
                try s.air_tir_type_map.put(air_ref, tir_address_type);
            },
            .zero_array => |air_zero_array| {
                const tir_array_type = try s.get_type_mapping(air_zero_array);
                const tir_array = try s.append_inst(.{ .zero_array = tir_array_type });
                try s.air_tir_inst_map.put(air_ref, tir_array);
            },
            .load => |air_load| {
                const tir_ptr = try s.get_inst_mapping(air_load.ptr);
                const tir_ptr_type = try get_tir_inst_ret_type(s, tir_ptr);
                // print("Loading from inst {} with type {}\n", .{ tir_ptr, tir_ptr_type });
                switch (tir_ptr_type) {
                    .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
                        return error.PtrIsNotPtrType;
                    },
                    _ => {
                        const tir_type = s.tir.values.get(@intFromEnum(tir_ptr_type));
                        switch (tir_type) {
                            .ptr_typ => |ptr| {
                                var tir_load_type: Value.IndexRef = undefined;
                                if (air_load.cap == .own) {
                                    tir_load_type = ptr.deref_type;
                                } else {
                                    const copy_equiv_ref = try gen_copy_equiv_type(s, tir_ptr_type);
                                    // print("Copy equiv {}", .{copy_equiv_ref});
                                    const copy_equiv = s.tir.values.get(@intFromEnum(copy_equiv_ref));
                                    // print("Copy equiv {}", .{copy_equiv});
                                    tir_load_type = copy_equiv.ptr_typ.deref_type;
                                }

                                const tir_load = try s.append_inst(TirInst{ .load = .{ .ptr = tir_ptr, .type = tir_load_type } });
                                try s.air_tir_inst_map.put(air_ref, tir_load);
                            },
                            else => return error.PtrIsNotPtrType,
                        }
                    },
                }

                // const tir_type = try t.get_type_mapping(air_load.type);

            },
            .store => |air_store| {
                const tir_val_inst = try s.get_inst_mapping(air_store.val);
                const tir_val_type_ref = try get_tir_inst_ret_type(s, tir_val_inst);

                const tir_ptr_inst = try s.get_inst_mapping(air_store.ptr);
                const tir_ptr_type = try get_tir_inst_ret_type(s, tir_ptr_inst);

                const tir_pointed_to_type = s.tir.values.get(@intFromEnum(tir_ptr_type)).ptr_typ.deref_type;

                if (type_ref_eq(s, tir_val_type_ref, tir_pointed_to_type, false) == false and type_ref_eq_stackref_coerce(s, tir_val_type_ref, tir_pointed_to_type) == false) {
                    print("{} {}\n", .{ tir_val_inst, tir_ptr_inst });
                    return error.StoringWrongType;
                }

                // if (tir_val_type_ref.is_ref()) {
                //     const tir_val_type = s.tir.types.get(@intFromEnum(tir_val_type_ref));
                //     if (tir_val_type == .ptr_typ and tir_val_type.ptr.cap == .own_typ) {
                //         // Ensure that the value instruction is a move when reassign owning pointers.
                //         const move_inst = s.tir.instructions.get(@intFromEnum(tir_val_inst));
                //         if (move_inst != .move) {
                //             print("{}\n", .{move_inst});
                //             return error.StoringOwnTypeWithoutMove;
                //         } else {
                //             // Ensure that the target is an owning type.
                //             // TODO: This may be need to be more complicated.
                //             if (type_ref_eq(s, tir_val_type_ref, tir_pointed_to_type, false) == false) {
                //                 print("{} COMPARED WITH {}\n", .{ tir_val_inst, tir_ptr_inst });
                //                 return error.StoringWrongType;
                //             }
                //         }

                //         // if (tir_pointed_to_type.is_ref()) {
                //         //     const tir_ptr_deref_type = s.tir.types.get(@intFromEnum(tir_pointed_to_type));
                //         //     if (tir_ptr_deref_type == .ptr_typ and tir_ptr)
                //         // }
                //     }
                // }

                const tir_store = try s.append_inst(TirInst{ .store = .{ .val = tir_val_inst, .val_type = tir_val_type_ref, .ptr = tir_ptr_inst } });
                try s.air_tir_inst_map.put(air_ref, tir_store);
            },
            .move => |air_move| {
                const tir_target = try s.get_inst_mapping(air_move);
                const tir_move = try s.append_inst(TirInst{ .move = @intFromEnum(tir_target) });
                try s.air_tir_inst_map.put(air_ref, tir_move);
            },
            .arg => |air_arg| {
                const tir_type_ref = try s.get_type_mapping(air_arg.type);
                const tir_arg = TirInst{ .arg = .{ .name = air_arg.name, .typ_ref = tir_type_ref } };
                const inst_index = try s.append_inst(tir_arg);
                if (tir_type_ref == .typ_typ) {
                    if (s.air_tir_type_map.get(air_ref)) |arg_type| {
                        // print("AT AIR ARG {} ADDING ARG TYPE MAP TO {}\n", .{ air_index, arg_type });
                        try s.air_tir_type_map.put(air_ref, arg_type);
                    } else {
                        try s.air_tir_type_map.put(air_ref, .typ_typ);
                    }
                    // const arg_type = try s.get_type_mapping(air_ref);
                }
                try s.air_tir_inst_map.put(air_ref, inst_index);
            },
            .indexing => |indexing| {
                const tir_target_inst: ?TirInst.IndexRef = s.get_inst_mapping(indexing.target) catch null;
                const tir_target_type: ?Value.IndexRef = s.get_type_mapping(indexing.target) catch null;

                if (tir_target_inst) |inst| {
                    // We are indexing into a value
                    const target_inst = try get_tir_inst_ret_type(s, inst);
                    // print("Target inst: {}\n", .{target_inst});
                    const target_type = s.tir.values.get(@intFromEnum(target_inst));
                    if (target_type != .array_typ) {
                        return error.IndexIntoNonArrayType;
                    }
                    const element_type = target_type.array_typ.element_type;

                    // Check if the index is comptime available
                    const tir_index_expr = try s.get_inst_mapping(indexing.index);
                    const maybe_val = try get_val(s, tir_index_expr);
                    if (maybe_val) |val| {
                        const val_index = try s.append_val(val);
                        const const_index = try s.append_inst(.{ .constant_index = .{ .index = val_index, .target = inst, .elem_type = element_type } });
                        try s.air_tir_inst_map.put(air_ref, const_index);
                    } else {
                        const runtime_index = try s.append_inst(.{ .index = .{ .index = @intFromEnum(tir_index_expr), .target = inst, .elem_type = element_type } });
                        try s.air_tir_inst_map.put(air_ref, runtime_index);
                    }
                } else if (tir_target_type) |typ| {
                    // We are indexing into a type i.e. creating an array type.
                    // Index must be comptime available.

                    // Check if the index is comptime available
                    const tir_index_expr = try s.get_inst_mapping(indexing.index);
                    const maybe_val = try get_val(s, tir_index_expr);
                    if (maybe_val) |val| {
                        // const val_index = try t.append_val(val);
                        // const const_index = try t.append_inst(.{ .constant_index = .{ .index = val_index, .target = tir_target_inst, .elem_type = element_type } });
                        // try t.air_tir_inst_map.put(air_ref, const_index);
                        if (val != .unknown_int_val) {
                            return error.Unimplemented;
                        }

                        const array_type = Value{ .array_typ = .{ .size = @intCast(val.unknown_int_val), .element_type = typ } };
                        const type_index = try s.append_val_ref(array_type);
                        const type_inst = try s.append_inst(.{ .constant_type = type_index });
                        try s.air_tir_inst_map.put(air_ref, type_inst);
                        try s.air_tir_type_map.put(air_ref, type_index);
                    } else {
                        return error.ArrayTypeMustHaveKnownSize;
                    }
                }
            },
            .int => |int| {
                const val = Value{ .unknown_int_val = @intCast(int) };
                try s.append_constant_val(val, air_index);
            },
            else => return error.Unimplemented,
        }
    }
    return @intCast(s.tir.instructions.len - 1);
}

pub fn tir_gen(air: *Air, allocator: Allocator) !Tir {
    var s = TirState{
        .scratch = std.ArrayList(u32).init(allocator),

        .tir = Tir{
            .instructions = TirInst.List{},
            .extra = std.ArrayList(u32).init(allocator),

            .values = Value.List{},
            .air = air,

            .allocator = allocator,
        },
        .air = air,
        .air_tir_inst_map = TirState.AirTirInstMap.init(allocator),
        .air_tir_type_map = TirState.AirTirValMap.init(allocator),
        .fn_def_map = TirState.FnDefMap.init(allocator),
    };
    defer s.deinit();
    try s.air_tir_inst_map.put(AirInst.IndexRef.true_lit, TirInst.IndexRef.true_val);
    try s.air_tir_inst_map.put(AirInst.IndexRef.false_lit, TirInst.IndexRef.false_val);
    try s.air_tir_inst_map.put(AirInst.IndexRef.null_lit, TirInst.IndexRef.null_val);

    try s.air_tir_type_map.put(AirInst.IndexRef.bool, Value.IndexRef.boolean_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.u8, Value.IndexRef.u8_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.u16, Value.IndexRef.u16_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.u32, Value.IndexRef.u32_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.u64, Value.IndexRef.u64_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.i8, Value.IndexRef.i8_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.i16, Value.IndexRef.i16_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.i32, Value.IndexRef.i32_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.i64, Value.IndexRef.i64_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.void, Value.IndexRef.void_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.own, Value.IndexRef.own_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.ref, Value.IndexRef.ref_typ);
    try s.air_tir_type_map.put(AirInst.IndexRef.type, Value.IndexRef.typ_typ);
    // try s.air_tir_type_map.put(AirInst.IndexRef.address_of_self, Type..IndexRefopaque_typ);

    const topmost_air = s.air.instructions.get(0);
    std.debug.assert(topmost_air == .block);

    var topmost_block = TirInst{ .block = .{ .start = 1, .end = undefined } };
    _ = try s.append_inst(topmost_block);
    _ = try tir_gen_bb(&s, 1, false);
    topmost_block.block.end = @intCast(s.tir.instructions.len - 1);
    s.tir.instructions.set(0, topmost_block);

    print("\n=========== GENERATED TIR ===========\n", .{});
    try print_tir(&s.tir, 0, @intCast(s.tir.instructions.len), 0);
    print("\n===========               ===========\n", .{});
    return s.tir;
}
