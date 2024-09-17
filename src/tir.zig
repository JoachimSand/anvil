const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const anvil_mal = @import("lib/anvil_multiarraylist.zig");
const MultiArrayList = anvil_mal.MultiArrayList;

const air_mod = @import("air.zig");
const Air = air_mod.Air;
const AirState = air_mod.AirState;
const AirInst = air_mod.AirInst;

const Hash = std.hash.Wyhash;

// Represents a compile-time known type
pub const Value = union(enum) {
    pub const Index = u32;
    const List = MultiArrayList(Value);
    const Slice = List.Slice.All;

    const RefStart = 4294967040;
    pub const IndexRef = enum(Index) {
        true_val = RefStart,
        false_val,
        null_val,
        boolean_typ,
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

        pub fn is_index(ref: *const Value.IndexRef) bool {
            if (@intFromEnum(ref.*) < Value.RefStart) {
                return true;
            } else {
                return false;
            }
        }

        pub fn get_index(ref: *const IndexRef) Index {
            return @intFromEnum(ref.*);
        }

        pub fn format(ref: *const IndexRef, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            if (ref.is_index()) {
                try std.fmt.format(writer, "%val_index.{d}", .{@intFromEnum(ref.*)});
            } else {
                try std.fmt.format(writer, "{s}", .{@tagName(ref.*)});
            }
        }
    };

    const Field = struct { var_name: Air.StringIndex, field_type: Value.IndexRef };

    const TirContainer = struct {
        fields_count: Index,

        // Whether this is a copy-equivalent version of another container
        // definition. If not, trailing the fields of this container
        // is a copy-equivalent definition.
        is_copy_equiv: bool,
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

    // recursive_ptr: struct { deref_type: IndexRef, cap: IndexRef },
    ptr_typ: struct { deref_type: IndexRef, cap: IndexRef },
    struct_typ: TirContainer,
    field_typ: Field,
    mut_field_typ: Field,
    enum_typ: TirContainer,

    array_typ: struct {
        size: u32,
        element_type: Value.IndexRef,
    },
};

// own_coerce: If set to true, if a has an ownership ptr type and b has a ref ptr type,
// accept equality
// stackref_coerce: If set to true, type equality is accepted for types whose only difference
// is that one is a &.stackref reference to the other.
fn types_are_equal(s: *TirState, a: Value.IndexRef, b: Value.IndexRef, own_coerce: bool, stackref_coerce: bool) bool {
    if (a == b) {
        return true;
    } else {
        // if (a == .opaque_typ or b == .opaque_typ) {
        //     return true;
        // }
        if (a.is_index() and b.is_index()) {
            const a_type = s.tir.values.get(a.get_index());
            const b_type = s.tir.values.get(b.get_index());

            if (a_type == .ptr_typ and b_type == .ptr_typ) {
                // if (a_type.ptr_typ.deref_type == .opaque_typ or b_type.ptr_typ.deref_type == .opaque_typ) {
                //     return true;
                // }
                // Ownership coercion
                if (own_coerce and a_type.ptr_typ.cap == .own_typ and b_type.ptr_typ.cap == .ref_typ and a_type.ptr_typ.deref_type == b_type.ptr_typ.deref_type) {
                    return types_are_equal(s, a_type.ptr_typ.deref_type, b, false, false);
                } else {
                    return false;
                }
            }
            // else if (a_type == .enum_typ and b_type == .enum_typ) {
            //     return true;
            // }
        }

        // Stackref coercion
        if (stackref_coerce) {
            if (a.is_index()) {
                const a_type = s.tir.values.get(a.get_index());
                if (a_type == .ptr_typ and a_type.ptr_typ.cap == .stackref_typ) {
                    return types_are_equal(s, a_type.ptr_typ.deref_type, b, own_coerce, false);
                }
            }

            if (b.is_index()) {
                const b_type = s.tir.values.get(b.get_index());
                if (b_type == .ptr_typ and b_type.ptr_typ.cap == .stackref_typ) {
                    return types_are_equal(s, a, b_type.ptr_typ.deref_type, own_coerce, false);
                }
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
        if (a.is_index() and b.is_index()) {
            const a_type = s.tir.values.get(@intFromEnum(a));
            // const b_type = t.tir.types.get(@intFromEnum(b));

            // TODO: Undo
            if (a_type == .ptr_typ and a_type.ptr_typ.cap == .stackref_typ) {
                return true;
            }
            if (a_type == .ptr_typ and a_type.ptr_typ.cap == .stackref_typ and types_are_equal(s, a_type.ptr_typ.deref_type, b, true)) {
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
    pub const List = MultiArrayList(TirInst);
    const Slice = List.Slice.All;

    pub const BinOp = struct {
        lhs: Index,
        rhs: Index,
    };
    pub const FieldAccess = struct {
        container_ptr: Index,
        field_id: u32,
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
        // Null if not available.
        constant_val: Value.IndexRef,
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
    constant_val: Value.IndexRef,
    constant_type: Value.IndexRef,

    constant_index: struct {
        index: Value.Index,
        elem_type: Value.IndexRef,
        target: Index,
    },
    index: struct { index: Index, elem_type: Value.IndexRef, target: Index },
    zero_array: Value.IndexRef,
    memalloc: struct {
        expr: Index,
        ptr_type: Value.IndexRef,
    },
    memfree: struct {
        ptr: Index,
        expr_type: Value.IndexRef,
    },
    print: struct {
        val: Index,
    },
    alloca: struct {
        alloc_type: Value.IndexRef,
        ret_type: Value.IndexRef,
    },
    load: struct {
        ptr: Index,
        type: Value.IndexRef,
    },

    // Store value into memory pointed to by ptr field.
    // 'ptr' argument must be pointer to some type T.
    // 'val' argument may either be an instruction containing T
    // or a pointer to a T.
    store: struct {
        val: Index,
        ptr: Index,
    },
    // TODO: Move to extra
    field_ptr: FieldAccess,
    field_val: FieldAccess,
    // address_of: struct {
    //     target: IndexRef,
    //     cap: IndexRef,
    //     ptr_type: Value.IndexRef,
    // },
    update_enum_ptr_with_val: struct {
        enum_ptr: Index,
        enum_type: Value.IndexRef,
        new_tag: u32,
        new_tag_val: Index,
    },
    update_enum_ptr_with_ptr: struct {
        enum_ptr: Index,
        enum_type: Value.IndexRef,
        new_tag: u32,
        new_tag_ptr: Index,
    },
    // get_element_ptr: struct {
    //     aggregate_ptr: IndexRef,
    //     aggregate_type: Value.IndexRef,
    //     indeces_start: Tir.ExtraIndex,
    //     indeces_end: Tir.ExtraIndex,
    //     ret_type: Value.IndexRef,
    // },
    match: struct {
        enum_ptr: Index,
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
    ExpectedType,
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
    TypeMustBeComptime,
    CapMustBeComptime,
    DerefOnPrimitive,
    DerefOnInvalidType,
    InvalidTagName,
    InvalidFieldName,
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

    pub fn get_val_fields(t: *Tir, container_index: Value.Index, count: Value.Index) Value.Slice {
        return t.values.slice().all_items_slice(container_index + 1, container_index + 1 + count);
    }

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
    // TODO: Consider deprecating this in favour of only using constant_vals
    // air_tir_val_map: AirTirValMap,

    value_intern_map: std.AutoHashMap(u64, Value.Index),

    fn_def_map: FnDefMap,
    scratch: std.ArrayList(u32),
    // Maps from container types to their respective field maps,
    // which in turn map from field names to field ids.
    field_maps: std.AutoHashMap(Value.IndexRef, FieldMap),

    pub const AirTirInstMap = std.AutoHashMap(AirInst.Index, TirInst.Index);
    // pub const AirTirValMap = std.AutoHashMap(AirInst.IndexRef, Value.IndexRef);
    // pub const ValMap = std.AutoHashMap(u64, Value.Index);

    const FnDefInfo = struct { tir_inst: TirInst.Index, air_inst: AirInst.Index };
    pub const FnDefMap = std.AutoHashMap(Air.StringIndex, FnDefInfo);

    pub const FieldId = u32;
    pub const FieldMap = std.AutoHashMap(Air.StringIndex, FieldId);
    pub const FieldMaps = std.AutoHashMap(Value.IndexRef, FieldMap);

    fn append_inst(t: *TirState, inst: TirInst) Allocator.Error!TirInst.Index {
        const index = t.tir.instructions.len;
        try t.tir.instructions.append(t.tir.allocator, inst);
        return @intCast(index);
    }

    fn append_inst_mapping(t: *TirState, air_index: AirInst.Index, inst: TirInst) Allocator.Error!TirInst.Index {
        const index_ref: TirInst.Index = @intCast(t.tir.instructions.len);

        try t.tir.instructions.append(t.tir.allocator, inst);
        try t.set_inst_mapping(air_index, index_ref);
        return index_ref;
    }

    fn intern_val(s: *TirState, val: Value) !Value.IndexRef {
        switch (val) {
            .unknown_int_val, .u64_val, .u32_val, .u16_val, .u8_val, .i64_val, .i32_val, .i16_val, .i8_val => return error.Unimplemented,

            inline .ptr_typ, .field_typ, .mut_field_typ => |v| {
                const hash = Hash.hash(0, std.mem.asBytes(&v));
                if (s.value_intern_map.get(hash)) |index| {
                    return @enumFromInt(index);
                } else {
                    const index = try s.append_val(val);
                    try s.value_intern_map.put(hash, @intFromEnum(index));
                    return index;
                }
            },
            else => return error.Unimplemented,
        }
    }

    // Must not be used for values that are potentially types.
    fn append_val(t: *TirState, val: Value) Allocator.Error!Value.IndexRef {
        const index: Value.Index = @intCast(t.tir.values.len);
        try t.tir.values.append(t.tir.allocator, val);
        return @enumFromInt(index);
    }

    fn get_inst_mapping(t: *TirState, air_index: AirInst.Index) !TirInst.Index {
        const tir_ref: TirInst.Index = t.air_tir_inst_map.get(air_index) orelse {
            print("Missing instruction mapping for {}\n", .{air_index});
            return error.MissingMapping;
        };
        return tir_ref;
    }

    // fn get_val_mapping(t: *TirState, air_index: AirInst.IndexRef) !Value.IndexRef {
    //     const tir_ref: Value.IndexRef = t.air_tir_val_map.get(air_index) orelse {
    //         print("Missing instruction mapping for {}\n", .{air_index});
    //         return error.MissingMapping;
    //     };
    //     return tir_ref;
    // }

    fn get_fn_def_mapping(t: *TirState, name: Air.StringIndex) !FnDefInfo {
        const fn_info = t.fn_def_map.get(name) orelse {
            print("Missing fn def. mapping for {}\n", .{name});
            return error.MissingMapping;
        };
        return fn_info;
    }

    fn set_inst_mapping(t: *TirState, air_index: AirInst.Index, tir_index: TirInst.Index) !void {
        try t.air_tir_inst_map.put(air_index, tir_index);
    }

    // fn set_val_mapping(t: *TirState, air_index: AirInst.Index, tir_val: Value.IndexRef) !void {
    //     try t.air_tir_val_map.put(@enumFromInt(air_index), tir_val);
    // }

    fn append_constant_val(t: *TirState, val: Value, air_index: AirInst.Index, intern: bool) !void {
        const val_index = if (intern) try t.intern_val(val) else try t.append_val(val);
        _ = try t.append_inst_mapping(air_index, TirInst{ .constant_val = val_index });
    }

    fn append_constant_val_ref(t: *TirState, val_ref: Value.IndexRef, air_index: AirInst.Index) !void {
        const tir_inst = TirInst{ .constant_val = val_ref };
        const tir_index = try t.append_inst(tir_inst);
        try t.air_tir_inst_map.put(air_index, tir_index);
    }
    // TODO: Move many of these to Tir rather than TirState.
    // Retrieve the comptime-known value of a Tir instruction.
    fn get_val(s: *TirState, tir_index: TirInst.Index) ?Value.IndexRef {
        const tir_inst = s.tir.instructions.get(tir_index);
        switch (tir_inst) {
            .constant_val => |val| return val,
            else => return null,
        }
    }

    fn get_val_noref(s: *TirState, tir_index: TirInst.Index) ?Value {
        const tir_inst = s.tir.instructions.get(tir_index);
        switch (tir_inst) {
            .constant_val => |val_ref| {
                if (val_ref.is_index()) {
                    return s.tir.values.get(val_ref.get_index());
                } else {
                    return null;
                }
            },
            else => return null,
        }
    }

    fn get_val_air(s: *TirState, air_ref: AirInst.IndexRef) ?Value.IndexRef {
        return switch (air_ref) {
            .true_lit => .true_val,
            .false_lit => .false_val,
            .null_lit => .null_val,
            .bool => .boolean_typ,
            .u8 => .u8_typ,
            .u16 => .u16_typ,
            .u32 => .u32_typ,
            .u64 => .u64_typ,
            .i8 => .i8_typ,
            .i16 => .i16_typ,
            .i32 => .i32_typ,
            .i64 => .i64_typ,
            .void => .void_typ,
            .own => .own_typ,
            .ref => .ref_typ,
            .type => .typ_typ,
            .address_of_self => return undefined,
            _ => {
                const tir_ref = s.get_inst_mapping(@intFromEnum(air_ref)) catch return null;
                return s.get_val(tir_ref);
            },
        };
    }

    fn get_field_id(t: *TirState, container: Value.IndexRef, field_name: Air.StringIndex) !FieldId {
        const field_map: TirState.FieldMap = t.field_maps.get(container).?;
        return field_map.get(field_name) orelse return error.InvalidFieldName;
    }

    fn get_field_val_ref(s: *TirState, container: Value.IndexRef, field_name: Air.StringIndex) !Value.IndexRef {
        return @enumFromInt(container.get_index() + 1 + try s.get_field_id(container, field_name));
    }

    fn get_field_val(s: *TirState, container: Value.IndexRef, field_name: Air.StringIndex) !Value {
        const val_ref = try s.get_field_val_ref(container, field_name);
        return s.tir.values.get(val_ref.get_index());
    }

    fn get_field_type(s: *TirState, container: Value.IndexRef, field_name: Air.StringIndex) !Value.IndexRef {
        const field_val = try s.get_field_val(container, field_name);
        switch (field_val) {
            .field_typ, .mut_field_typ => |field| return field.field_type,
            else => unreachable,
        }
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
        // t.air_tir_val_map.deinit();
        t.value_intern_map.deinit();
        t.fn_def_map.deinit();

        var field_map_it = t.field_maps.valueIterator();
        while (field_map_it.next()) |map| {
            map.deinit();
        }
        t.field_maps.deinit();
    }
};

fn print_type(t: *Tir, type_ref: Value.IndexRef) !void {
    switch (type_ref) {
        .true_val, .false_val, .null_val, .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
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

                    const fields = t.get_val_fields(type_index, tir_container.fields_count);
                    for (fields.data, fields.tags) |field_bare, tag| {
                        var field: Value.Field = undefined;
                        if (tag == .field_typ) {
                            field = field_bare.field_typ;
                        } else {
                            field = field_bare.field_typ;
                        }
                        const field_name = t.air.get_string(field.var_name);
                        if (tag == .mut_field_typ) {
                            print("mut ", .{});
                        }
                        print("{s}: {}, ", .{ field_name, field.field_type });

                        // try print_type(t, field.field_type);
                        // print(", ", .{});
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
        .true_val, .false_val, .null_val, .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
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

                    const fields = t.get_val_fields(val_index, tir_container.fields_count);
                    for (fields.data, fields.tags) |field_bare, tag| {
                        var field: Value.Field = undefined;
                        if (tag == .field_typ) {
                            field = field_bare.field_typ;
                        } else {
                            field = field_bare.field_typ;
                        }
                        const field_name = t.air.get_string(field.var_name);
                        switch (tag) {
                            .field_typ => {
                                print("{s} : ", .{field_name});
                            },
                            .mut_field_typ => {
                                print("mut {s} : ", .{field_name});
                            },
                            else => unreachable,
                        }

                        try print_type(t, field.field_type);
                        print(", ", .{});
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
                print_val(t, val_index);
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
                // print(", returns type ", .{});
                // try print_type(t, alloca.ret_type);
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
            // .address_of => |address_of| {
            //     print("address_of %{} with cap %{} returns type ", .{ address_of.target, address_of.cap });
            //     try print_type(t, address_of.ptr_type);
            // },
            // .get_element_ptr => |get_elem_ptr| {
            //     print("get_element_ptr ", .{});
            //     try print_type(t, get_elem_ptr.aggregate_type);
            //     print(", from {} by ", .{get_elem_ptr.aggregate_ptr});

            //     const slice = t.extra.items[get_elem_ptr.indeces_start..get_elem_ptr.indeces_end];
            //     for (slice) |s| {
            //         print("{}, ", .{s});
            //     }
            // },
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
            .field_ptr => |field_access| {
                print("field_ptr {} {}, ret_type: ", .{ field_access.container_ptr, field_access.field_id });
                try print_type(t, field_access.ret_type);
            },
            .field_val => |field_access| {
                print("field_ptr {} {}, ret_type", .{ field_access.container_ptr, field_access.field_id });
                try print_type(t, field_access.ret_type);
            },
            .fn_def => |fn_def| {
                print("fn def {s} (", .{t.air.get_string(fn_def.name)});

                // const param_extras = t.extra.items[fn_def.params.start..fn_def.params.end];
                // for (param_extras) |param_inst| {
                //     print("%{}, ", .{param_inst});
                // }
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
                // print("store ", .{});
                // try print_type(t, store.val_type);
                print("store {} to ptr at {}", .{ store.val, store.ptr });
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

fn get_val_index(s: *TirState, tir_ref: TirInst.Index) !?Value.Index {
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

fn get_enum_field(s: *TirState, enum_agg: Value.TirContainer, field_name: Air.StringIndex) !struct { Value.Field, u32 } {
    var enum_field_index = enum_agg.fields_start;
    while (enum_field_index < enum_agg.fields_end) : (enum_field_index += 1) {
        const cur_field = s.tir.values.get(enum_field_index);
        switch (cur_field) {
            .field_typ => |enum_field| {
                if (enum_field.var_name == field_name) {
                    return .{ enum_field, enum_field_index - enum_agg.fields_start };
                }
            },
            else => unreachable,
        }
    }
    return error.InvalidTagName;
}

fn get_copy_equiv_type(s: *TirState, type_ref: Value.IndexRef) !Value.IndexRef {
    switch (type_ref) {
        .true_val, .false_val, .null_val => return error.ExpectedType,
        .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .typ_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
            return type_ref;
        },
        _ => {
            const tir_type = s.tir.values.get(@intFromEnum(type_ref));
            switch (tir_type) {
                .array_typ => return error.Unimplemented,
                .struct_typ, .enum_typ => |tir_container| {
                    if (tir_container.is_copy_equiv) {
                        const type_index: Value.Index = @intFromEnum(type_ref);
                        return @enumFromInt(type_index + 1 + tir_container.fields_count);
                    } else {
                        return type_ref;
                    }
                },
                .ptr_typ => |ptr| {
                    const new_deref_type = try get_copy_equiv_type(s, ptr.deref_type);
                    const new_ptr_type = try s.intern_val(.{ .ptr_typ = .{ .deref_type = new_deref_type, .cap = .ref_typ } });
                    return new_ptr_type;
                },
                else => return error.Unimplemented,
            }

            // return error.Unimplemented;
            // const tir_type = s.tir.values.get(@intFromEnum(type_ref));
            // switch (tir_type) {
            //     .array_typ => return error.Unimplemented,
            //     .struct_typ, .enum_typ => |tir_container| {

            //         // Reserve space for the field types first
            //         const new_fields_start: Value.Index = @intCast(s.tir.values.len);
            //         var cur_field_index = tir_container.fields_start;
            //         while (cur_field_index < tir_container.fields_end) : (cur_field_index += 1) {
            //             _ = try s.append_val(undefined);
            //         }

            //         cur_field_index = tir_container.fields_start;
            //         while (cur_field_index < tir_container.fields_end) : (cur_field_index += 1) {
            //             const old_field_type = s.tir.values.get(cur_field_index);
            //             var new_field: Value = undefined;
            //             switch (old_field_type) {
            //                 .field_typ => |field| {
            //                     const new_field_type = try gen_copy_equiv_type(s, field.field_type);
            //                     new_field = .{ .field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
            //                 },
            //                 .mut_field_typ => |field| {
            //                     const new_field_type = try gen_copy_equiv_type(s, field.field_type);
            //                     new_field = .{ .mut_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
            //                 },
            //                 else => unreachable,
            //             }
            //             s.tir.values.set(new_fields_start + cur_field_index - tir_container.fields_start, new_field);
            //         }

            //         const new_fields_len: Value.Index = @intCast(tir_container.fields_end - tir_container.fields_start);
            //         const new_container_type = if (tir_type == .struct_typ)
            //             Value{ .struct_typ = .{ .fields_start = new_fields_start, .fields_end = new_fields_start + new_fields_len } }
            //         else
            //             Value{ .enum_typ = .{ .fields_start = new_fields_start, .fields_end = new_fields_start + new_fields_len } };
            //         const new_container_type_ref = try s.append_val_ref(new_container_type);
            //         try print_type(&s.tir, new_container_type_ref);
            //         return new_container_type_ref;

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
            //         .field_typ => |field| {
            //             const new_field_type = try gen_copy_equiv_type(s, field.field_type);
            //             new_field = .{ .field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
            //         },
            //         .mut_field_typ => |field| {
            //             const new_field_type = try gen_copy_equiv_type(s, field.field_type);
            //             new_field = .{ .mut_field_typ = .{ .var_name = field.var_name, .field_type = new_field_type } };
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
            // },
            // .ptr_typ => |ptr| {
            //     const new_deref_type = try gen_copy_equiv_type(s, ptr.deref_type);
            //     const new_ptr_type = try s.intern_val(.{ .ptr_typ = .{ .deref_type = new_deref_type, .cap = .ref_typ } });
            //     return new_ptr_type;
            // },
            // else => unreachable,
            // }
        },
    }
}

fn get_aggregate_type(s: *TirState, type_ref: Value.IndexRef, get_enum: bool) !Value.TirContainer {
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

fn get_val_type(s: *TirState, val_ref: Value.IndexRef) !Value.IndexRef {
    switch (val_ref) {
        .true_val, .false_val => return .boolean_typ,
        .null_val => return error.Unimplemented,
        .boolean_typ, .unknown_int_typ, .u8_typ, .u16_typ, .u32_typ, .u64_typ, .i8_typ, .i16_typ, .i32_typ, .i64_typ => return .typ_typ,
        .void_typ, .opaque_typ => return .typ_typ,
        .typ_typ => return error.TypeOfType,
        // TODO: Add .typ_cap?
        .own_typ, .ref_typ, .stackref_typ => return error.Unimplemented,
        _ => {
            const value = s.tir.values.get(@intFromEnum(val_ref));
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
                .ptr_typ, .struct_typ, .enum_typ, .field_typ, .mut_field_typ, .array_typ => return .typ_typ,
            }
        },
    }
}

// Get the type of the resulting value of a TIR instruction reference
// TODO: This could be cached.
fn get_tir_inst_ret_type(s: *TirState, inst_index: TirInst.Index) !Value.IndexRef {
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
        .field_ptr, .field_val => |f| {
            return f.ret_type;
        },
        // .address_of => |address_of| {
        //     return address_of.ptr_type;
        // },
        .update_enum_ptr_with_ptr => |update_enum| return get_tir_inst_ret_type(s, update_enum.enum_ptr),
        .update_enum_ptr_with_val => |update_enum| return get_tir_inst_ret_type(s, update_enum.enum_ptr),
        // .get_element_ptr => |get_elem_ptr| {
        //     return get_elem_ptr.ret_type;
        // },
        .move => |move| {
            const val_type = try get_tir_inst_ret_type(s, move);
            return val_type;
        },
        .add_i8 => return .i8_typ,
        .add_i16 => return .i16_typ,
        .add_i32 => return .i32_typ,
        .add_i64 => return .i64_typ,
        .add_u8 => return .u8_typ,
        .add_u16 => return .u16_typ,
        .add_u32 => return .u32_typ,
        .add_u64 => return .u64_typ,
        .arg => |arg| return arg.typ_ref,
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
        else => {
            print("Attempt to coerce comp-time int into {}\n", .{target_type});
            return error.IntCoercionIntoNonInt;
        },
    }
}

fn tir_gen_blk(s: *TirState, air_blk_index: AirInst.Index, is_top_level: bool) !TirInst.Index {
    const tir_existing_blk = s.air_tir_inst_map.get(air_blk_index);
    if (tir_existing_blk) |existing_blk| {
        return existing_blk;
    }

    const tir_blk_index = try s.append_inst(undefined);
    var tir_inst = TirInst{ .block = .{ .start = @intCast(s.tir.instructions.len), .end = undefined } };
    try s.air_tir_inst_map.put(air_blk_index, tir_blk_index);
    // print("Block begins at {}\n", .{tir_inst.block.start});

    const air_blk = s.air.instructions.get(air_blk_index);

    // print("\nGENERATING BLOCK FROM AIR {d} TO {d}\n", .{ @intFromEnum(air_blk.block.start), @intFromEnum(air_blk.block.end) });

    tir_inst.block.end = try tir_gen_bb(s, @intFromEnum(air_blk.block.start) + 1, true);
    if (is_top_level) {
        tir_inst.block.end = @intCast(s.tir.instructions.len - 1);
    }

    // print("Block ends at {}\n", .{tir_inst.block.end});

    s.tir.instructions.set(tir_blk_index, tir_inst);
    return tir_blk_index;
}

fn tir_gen_container_def(s: *TirState, air_container: AirInst.ContainerDef, air_index: AirInst.Index, is_copy_equiv: bool) !TirInst.Index {
    const tir_container_type_index = try s.append_val(undefined);
    const tir_inst = try s.append_inst(.{ .constant_val = tir_container_type_index });

    var field_map = TirState.FieldMap.init(s.tir.allocator);
    const air_fields = s.air.get_fields(air_index, air_container.field_count);
    for (air_fields.data, air_fields.tags, 0..) |air_field, air_tag, i| {
        // Extract the field type.
        const air_fdef = if (air_tag == .field_def) air_field.field_def else air_field.mut_field_def;
        var tir_field_type = s.get_val_air(air_fdef.type_inst) orelse return error.TypeMustBeComptime;

        // Create a mapping between a field name and ID for subsequential easier lookups.
        try field_map.put(air_fdef.var_name, @intCast(i));

        // If requested, convert the field type to a copy-type (i.e. no owned references).
        if (is_copy_equiv) {
            tir_field_type = try get_copy_equiv_type(s, tir_field_type);
        }

        const tir_field: Value = if (air_tag == .field_def)
            .{ .field_typ = .{ .var_name = air_fdef.var_name, .field_type = tir_field_type } }
        else
            .{ .mut_field_typ = .{ .var_name = air_fdef.var_name, .field_type = tir_field_type } };
        _ = try s.append_val(tir_field);
    }

    try s.field_maps.put(tir_container_type_index, field_map);

    // Create the container type.
    const tir_container: Value = if (s.air.instructions.get(air_index) == .struct_def)
        .{ .struct_typ = .{ .fields_count = air_container.field_count, .is_copy_equiv = is_copy_equiv } }
    else
        .{ .enum_typ = .{ .fields_count = air_container.field_count, .is_copy_equiv = is_copy_equiv } };
    s.tir.values.set(@intFromEnum(tir_container_type_index), tir_container);
    return tir_inst;
}

fn tir_gen_bb(s: *TirState, air_bb_start: AirInst.Index, ret_on_fn_def: bool) TirError!TirInst.Index {
    var air_index: u32 = air_bb_start;
    const air_instructions = s.air.instructions.slice();
    // print("Generating instructions from {}\n", .{air_bb_start});
    while (air_index < s.air.instructions.len) : (air_index += 1) {
        // const air_index: AirInst.IndexRef = @enumFromInt(air_index);
        print("\n{} Generating TIR for AIR instruction: ", .{s.tir.instructions.len});
        _ = try air_mod.print_air(s.air, air_index, air_index + 1, 0);

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
                const tir_ret_type = s.get_val_air(fn_def.ret_type) orelse return error.TypeMustBeComptime;

                const fn_def_inst_index = try s.append_inst(TirInst{ .fn_def = .{ .name = fn_def.name, .params = undefined, .ret_type = tir_ret_type, .blk = undefined } });
                try s.air_tir_inst_map.put(air_index, fn_def_inst_index);
                const fn_def_info = TirState.FnDefInfo{ .tir_inst = fn_def_inst_index, .air_inst = air_index };
                try s.fn_def_map.put(fn_def.name, fn_def_info);
                const tir_blk = try tir_gen_blk(s, fn_def.blk, false);

                const param_indeces = s.air.extra.items[fn_def.params.start..fn_def.params.end];
                const tir_params_start = s.tir.extra.items.len;
                for (param_indeces) |param_index| {
                    const tir_param = try s.get_inst_mapping(param_index);
                    try s.tir.extra.append(tir_param);
                }
                const tir_params = TirInst.ExtraSlice{ .start = @intCast(tir_params_start), .end = @intCast(s.tir.extra.items.len) };

                const tir_fn_def = TirInst{ .fn_def = .{ .name = fn_def.name, .params = tir_params, .ret_type = tir_ret_type, .blk = tir_blk } };
                s.tir.instructions.set(fn_def_inst_index, tir_fn_def);
                if (ret_on_fn_def) {
                    return tir_blk;
                }
            },
            // .fn_call => |call_extra| {
            //     const fn_call = s.air.get_extra_struct(AirInst.FnCall, call_extra);
            //     const fn_def_info = try s.get_fn_def_mapping(fn_call.name);
            //     // const fn_def_tir = s.tir.instructions.get(.fn_def_infoinst_typ).fn_def;
            //     const fn_def_air = s.air.get_extra_struct(AirInst.FnDef, s.air.instructions.get(fn_def_info.air_inst).fn_def);

            //     const air_param_indeces = s.air.extra.items[fn_def_air.params.start..fn_def_air.params.end];
            //     // const param_indeces = s.tir.extra.items[fn_def_tir.params.start..fn_def_tir.params.end];
            //     const arg_indeces = s.air.extra.items[fn_call.args.start..fn_call.args.end];

            //     const is_comptime = true;
            //     var extra: u32 = 0;
            //     // while (extra < arg_indeces.len) : (extra += 1) {
            //     // const a_index = arg_indeces[extra];
            //     // const p_index = param_indeces[extra];

            //     // const arg = try s.get_type_mapping(@enumFromInt(a_index));
            //     // const arg_val = try get_val(s, arg);
            //     // if (arg_val == null) {
            //     //     is_comptime = false;
            //     // }

            //     // const param = try s.
            //     // }

            //     if (is_comptime) {
            //         extra = 0;
            //         // Remove the previous mapping
            //         const fn_def_block = s.air.instructions.get(fn_def_air.blk).block;
            //         const fn_def_end: AirInst.Index = @intFromEnum(fn_def_block.end);
            //         var fn_def_index = fn_def_info.air_inst;
            //         while (fn_def_index < fn_def_end) : (fn_def_index += 1) {
            //             // print("Removing mappin for {}\n", .{fn_def_index});
            //             _ = s.air_tir_inst_map.remove(@enumFromInt(fn_def_index));
            //             // _ = s.air_tir_val_map.remove(@enumFromInt(fn_def_index));
            //         }

            //         // Rebind args to comptime-values
            //         while (extra < arg_indeces.len) : (extra += 1) {
            //             const a_index = arg_indeces[extra];

            //             const arg = try s.get_val_mapping(@enumFromInt(a_index));
            //             // print("Remapping {} to {}\n", .{ air_param_indeces[extra], arg });
            //             try s.air_tir_val_map.put(@enumFromInt(air_param_indeces[extra]), arg);
            //         }

            //         _ = s.air_tir_inst_map.remove(@enumFromInt(fn_def_air.blk));

            //         const blk_index = try tir_gen_bb(s, fn_def_info.air_inst, true);
            //         // try print_tir(&s.tir, air_index, @intCast(s.tir.instructions.len), 0);

            //         // print("blk index {}\n", .{blk_index});
            //         const blk = s.tir.instructions.get(blk_index).block;
            //         const ret_inst = s.tir.instructions.get(blk.end).ret;
            //         try s.air_tir_val_map.put(air_ref, ret_inst.val);
            //         // if (ret_inst.is_ref().val)) {
            //         //     // const air_fn_def_blk = s.air.instructions.get(fn_def_air.blk).block;
            //         //     // const air_ret_inst: AirInst.Index = @intFromEnum(air_fn_def_blk.end) - 1;
            //         //     // print("Air ret inst: {}\n", .{air_ret_inst});
            //         //     // // const ret_val_inst = s.tir.instructions.get(@intFromEnum(ret_inst.val));
            //         //     // const ret_val_type = s.air_tir_type_map.get(@enumFromInt(air_ret_inst)).?;
            //         // } else {
            //         //     return error.Unimplemented;
            //         // }
            //         // if (ret_inst.is_ref().val)) {
            //         //     const ret_val_inst = s.tir.instructions.get(@intFromEnum(ret_inst.val));
            //         //     try s.air_tir_type_map.put(@enumFromInt(air_param_indeces[extra]), ret_val_inst.constant_type);
            //         // } else {
            //         //     return error.Unimplemented;
            //         // }
            //     } else {
            //         return error.Unimplemented;
            //     }
            // },
            .struct_def, .enum_def => |air_container_def| {
                const def = try tir_gen_container_def(s, air_container_def, air_index, false);
                _ = try tir_gen_container_def(s, air_container_def, air_index, true);

                try s.air_tir_inst_map.put(air_index, def);

                // Skip the field_defs we have already processed.
                air_index = air_index + air_container_def.field_count;
            },
            .get_field_type => |cft| {
                // const container_typ = try get
                const container_typ = s.get_val_air(cft.container_type) orelse return error.TypeMustBeComptime;
                if (container_typ.is_index() == false) {
                    print("Container ref: {}", .{container_typ});
                    return error.MismatchedTypes;
                }

                const val = s.tir.values.get(container_typ.get_index());
                if (val == .struct_typ or val == .enum_typ) {
                    const field_type = try s.get_field_type(container_typ, cft.field_name);
                    _ = try s.append_inst_mapping(air_index, .{ .constant_val = field_type });
                    print("GET FIELD TYPE RESULTING IN {}\n", .{field_type});
                } else {
                    return error.MismatchedTypes;
                }
            },
            .container_init => |c_init| {
                const container_typ = s.get_val_air(c_init.container_type) orelse return error.TypeMustBeComptime;
                const ret_type = try s.intern_val(.{ .ptr_typ = .{ .deref_type = container_typ, .cap = .stackref_typ } });
                const tir_alloc = try s.append_inst_mapping(air_index, .{ .alloca = .{ .alloc_type = container_typ, .ret_type = ret_type } });

                const container_typ_val = s.tir.values.get(container_typ.get_index());
                switch (container_typ_val) {
                    .struct_typ => |s_typ| {
                        if (s_typ.fields_count != c_init.field_init_count) {
                            // TODO: Implement default-values.
                            return error.Unimplemented;
                        }

                        const field_inits = s.air.get_fields(air_index, s_typ.fields_count);
                        for (field_inits.data) |air_bare| {
                            // Determine the field ID of the field being initialised.
                            const field_init = air_bare.field_init;
                            const field_name = air_instructions.get(field_init.field_type.get_index()).get_field_type.field_name;
                            const field_id = try s.get_field_id(container_typ, field_name);

                            // Determine the type of the field and type of the value being assigned to it.
                            // Ensure these types match.
                            const field_typ = try s.get_field_type(container_typ, field_name);
                            const expr = if (field_init.expr.is_index() == false) return error.Unimplemented else try s.get_inst_mapping(field_init.expr.get_index());
                            const expr_typ = try get_tir_inst_ret_type(s, expr);
                            if (types_are_equal(s, field_typ, expr_typ, false, true) == false) {
                                return error.MismatchedTypes;
                            }

                            // Append instructions to store the initialiser expression. Map the air field_init instruction to the
                            // value of the expression assigned to it.
                            const field_ret_type = try s.intern_val(Value{ .ptr_typ = .{ .deref_type = field_typ, .cap = .ref_typ } });
                            const field_ptr = try s.append_inst(.{ .field_ptr = .{ .container_ptr = tir_alloc, .field_id = field_id, .ret_type = field_ret_type } });
                            _ = try s.append_inst(.{ .store = .{ .val = expr, .ptr = field_ptr } });
                            try s.set_inst_mapping(air_index + field_id + 1, expr);
                        }
                        // Skip the field_inits we have already processed.
                        air_index = air_index + s_typ.fields_count;
                    },
                    .enum_typ => |_| {
                        return error.Unimplemented;
                    },
                    else => unreachable,
                }
            },
            .get_field_ptr => |get_field| {
                if (get_field.container.is_index() == false) {
                    return error.MismatchedTypes;
                }
                const container_expr = try s.get_inst_mapping(get_field.container.get_index());

                // get_field_ptr accepts both a value of a container and a ptr to a
                // container as an argument. Handle both cases.

                const container_expr_typeref = try get_tir_inst_ret_type(s, container_expr);
                const container_expr_type = s.tir.values.get(container_expr_typeref.get_index());

                const container_typ: Value.IndexRef =
                    switch (container_expr_type) {
                    .ptr_typ => |ptr| ptr.deref_type,
                    .struct_typ => |_| container_expr_typeref,
                    .enum_typ => return error.Unimplemented,
                    else => {
                        print("{}", .{container_expr_type});
                        return error.MismatchedTypes;
                    },
                };

                const field_id = try s.get_field_id(container_typ, get_field.field_name);

                // TODO: This is similar to the code written for fields in container_init.
                const field_typ = try s.get_field_type(container_typ, get_field.field_name);
                const field_ret_type = try s.intern_val(Value{ .ptr_typ = .{ .deref_type = field_typ, .cap = .ref_typ } });
                _ = try s.append_inst_mapping(air_index, .{ .field_ptr = .{ .container_ptr = container_expr, .field_id = field_id, .ret_type = field_ret_type } });
            },

            // .update_enum_ptr => |air_update_enum| {
            //     const tir_enum_ptr_inst = try s.get_inst_mapping(air_update_enum.ptr);
            //     const tir_enum_ptr_type = try get_tir_inst_ret_type(s, tir_enum_ptr_inst);

            //     const tir_enum_type = s.tir.values.get(@intFromEnum(tir_enum_ptr_type)).ptr_typ.deref_type;
            //     const tir_enum = try get_aggregate_type(s, tir_enum_type, true);

            //     // TODO: Use get_enum_field
            //     var tag: u32 = undefined;
            //     var tag_field_type: Value.IndexRef = undefined;
            //     var enum_field_index = tir_enum.fields_start;
            //     while (enum_field_index < tir_enum.fields_end) : (enum_field_index += 1) {
            //         const cur_field = s.tir.values.get(enum_field_index);
            //         switch (cur_field) {
            //             .enum_field_typ,
            //             => |enum_field| {
            //                 if (enum_field.var_name == air_update_enum.new_tag) {
            //                     tag = enum_field_index - tir_enum.fields_start;
            //                     tag_field_type = enum_field.field_type;
            //                     break;
            //                 }
            //             },
            //             else => unreachable,
            //         }
            //     }

            //     const tir_tag_contents = try s.get_inst_mapping(air_update_enum.tag_contents);
            //     // TODO: This will remove booleans
            //     const tir_tag_contents_val = get_val_index(s, tir_tag_contents) catch null;
            //     if (tir_tag_contents_val) |val_index| {
            //         const val_type = try get_val_type(s, val_index);
            //         if (type_ref_eq(s, val_type, tag_field_type, true) == false) {
            //             return error.MismatchedEnumFieldType;
            //         }

            //         const inst = TirInst{ .update_enum_ptr_with_val = .{ .enum_ptr = tir_enum_ptr_inst, .enum_type = tir_enum_type, .new_tag = tag, .new_tag_val = tir_tag_contents } };
            //         const inst_ref = try s.append_inst(inst);
            //         try s.air_tir_inst_map.put(air_ref, inst_ref);
            //     } else {
            //         const contents_type_ref = try get_tir_inst_ret_type(s, tir_tag_contents);
            //         switch (contents_type_ref) {
            //             .typ_typ => return error.Unimplemented,
            //             .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ => {
            //                 if (type_ref_eq(s, contents_type_ref, tag_field_type, false) == false) {
            //                     return error.MismatchedEnumFieldType;
            //                 }

            //                 const inst = TirInst{ .update_enum_ptr_with_val = .{ .enum_ptr = tir_enum_ptr_inst, .enum_type = tir_enum_type, .new_tag = tag, .new_tag_val = tir_tag_contents } };
            //                 const inst_ref = try s.append_inst(inst);
            //                 try s.air_tir_inst_map.put(air_ref, inst_ref);
            //             },
            //             _ => {
            //                 const contents_type = s.tir.values.get(@intFromEnum(contents_type_ref));
            //                 if (contents_type != .ptr_typ) {
            //                     unreachable;
            //                 }
            //                 if (type_ref_eq(s, contents_type_ref, tag_field_type, false) == false) {
            //                     return error.MismatchedEnumFieldType;
            //                 }

            //                 const inst = TirInst{ .update_enum_ptr_with_ptr = .{ .enum_ptr = tir_enum_ptr_inst, .enum_type = tir_enum_type, .new_tag = tag, .new_tag_ptr = tir_tag_contents } };
            //                 const inst_ref = try s.append_inst(inst);
            //                 try s.air_tir_inst_map.put(air_ref, inst_ref);
            //                 // One level of indirection via pointer is a stack struct or enum
            //                 // switch (contents_type.ptr) {
            //                 //     .typ_typ => return error.Unimplemented,
            //                 //     .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ => return error.Unimplemented,
            //                 //     _ => {
            //                 //         const pointed_to_typ = t.types.get(@intFromEnum(contents_type));
            //                 //     },
            //                 // }

            //             },
            //         }
            //     }
            // },

            // .get_element_ptr => |air_extra| {
            //     const air_get_elem = s.air.get_extra_struct(AirInst.GetElementPtr, air_extra);
            //     const tir_aggregate_inst = try s.get_inst_mapping(air_get_elem.aggregate_ptr);

            //     // print("Tir aggregrate inst {}\n", .{tir_aggregate_inst});
            //     const tir_aggregate_ptr_ref = try get_tir_inst_ret_type(s, tir_aggregate_inst);
            //     const tir_aggregrate_ptr_type = s.tir.values.get(@intFromEnum(tir_aggregate_ptr_ref));

            //     if (tir_aggregrate_ptr_type != .ptr_typ) {
            //         print("Expected pointer to aggregrate, got {} from {}\n", .{ tir_aggregrate_ptr_type, tir_aggregate_inst });
            //         return error.Unimplemented;
            //     }

            //     const tir_aggregate_type = tir_aggregrate_ptr_type.ptr_typ.deref_type;
            //     const tir_struct = try get_aggregate_type(s, tir_aggregate_type, false);
            //     const access_field_indeces_start: Tir.ExtraIndex = @intCast(s.tir.extra.items.len);

            //     var cur_access_field = air_get_elem.fields.start;
            //     var field_type: ?Value.IndexRef = null;
            //     while (cur_access_field < air_get_elem.fields.end) : (cur_access_field += 1) {
            //         const access_field_name: Air.StringIndex = s.air.extra.items[cur_access_field];
            //         // const field_index

            //         // The numerical index of the field access. If the the field is the first in
            //         // in the aggregate, this would 0, second would be 1 etc.
            //         var access_field_index: ?u32 = null;

            //         // TODO, change tir_struct for multiple field accesses.
            //         var struct_field_index = tir_struct.fields_start;
            //         while (struct_field_index < tir_struct.fields_end) : (struct_field_index += 1) {
            //             const cur_field = s.tir.values.get(struct_field_index);
            //             switch (cur_field) {
            //                 .field_typ, .mut_field_typ => |struct_field| {
            //                     if (struct_field.var_name == access_field_name) {
            //                         access_field_index = struct_field_index - tir_struct.fields_start;
            //                         field_type = struct_field.field_type;
            //                         break;
            //                     }
            //                 },
            //                 else => unreachable,
            //             }
            //         }

            //         if (access_field_index) |a_index| {
            //             // print(" ACCESS FIELD INDEX {} {}\n", .{ a_index, access_field_indeces_start });
            //             try s.tir.extra.append(a_index);
            //         } else {
            //             return error.UndefinedVar;
            //         }
            //     }
            //     const access_field_indeces_end: Tir.ExtraIndex = @intCast(s.tir.extra.items.len);
            //     const ptr_to_field_type = try s.append_val(Value{ .ptr_typ = .{ .deref_type = field_type.?, .cap = .ref_typ } });

            //     const tir_get_elem = TirInst{ .get_element_ptr = .{ .aggregate_ptr = tir_aggregate_inst, .aggregate_type = tir_aggregate_type, .indeces_start = access_field_indeces_start, .indeces_end = access_field_indeces_end, .ret_type = @enumFromInt(ptr_to_field_type) } };
            //     const tir_get_elem_inst = try s.append_inst(tir_get_elem);
            //     try s.air_tir_inst_map.put(air_ref, tir_get_elem_inst);
            // },
            // .match => |air_match| {
            //     const tir_enum_ptr_inst = try s.get_inst_mapping(@enumFromInt(air_match.enum_ptr));
            //     const tir_enum_ptr_type = try get_tir_inst_ret_type(s, tir_enum_ptr_inst);

            //     const tir_enum_type = s.tir.values.get(@intFromEnum(tir_enum_ptr_type)).ptr_typ.deref_type;
            //     const tir_enum = try get_aggregate_type(s, tir_enum_type, true);
            //     const num_cases: u32 = @intCast(tir_enum.fields_end - tir_enum.fields_start);

            //     const cases_field_count: Air.ExtraIndex = @intCast(@typeInfo(AirInst.MatchCase).Struct.fields.len);

            //     const tir_match_inst_index = try s.append_inst(undefined);
            //     try s.air_tir_inst_map.put(air_ref, tir_match_inst_index);

            //     const cases_start: u32 = @intCast(s.tir.extra.items.len);
            //     try s.tir.extra.resize(s.tir.extra.items.len + num_cases + 1);
            //     var case_index = air_match.cases_start;
            //     while (case_index < air_match.cases_end) : (case_index += cases_field_count) {
            //         const case = s.air.get_extra_struct(AirInst.MatchCase, case_index);
            //         // print("tag case begin\n", .{});
            //         const case_blk = try tir_gen_blk(s, case.blk, false);

            //         const enum_field_tuple = try get_enum_field(s, tir_enum, case.tag);
            //         // const enum_field = enum_field_tuple[0];
            //         const tag_num = enum_field_tuple[1];

            //         // print("tag case end {}\n", .{tag_num});
            //         s.tir.extra.items[cases_start + tag_num] = case_blk;
            //     }
            //     const tir_match = TirInst{ .match = .{ .enum_ptr = tir_enum_ptr_inst, .cases_start = cases_start, .cases_end = cases_start + num_cases } };
            //     s.tir.instructions.set(@intFromEnum(tir_match_inst_index), tir_match);
            //     return @intFromEnum(tir_match_inst_index);
            // },
            // .enum_project => |air_project| {
            //     const tir_enum_ptr_inst = try s.get_inst_mapping(@enumFromInt(air_project.enum_ptr));
            //     const tir_enum_ptr_type = try get_tir_inst_ret_type(s, tir_enum_ptr_inst);

            //     const tir_enum_type = s.tir.values.get(@intFromEnum(tir_enum_ptr_type)).ptr_typ.deref_type;
            //     const tir_enum = try get_aggregate_type(s, tir_enum_type, true);
            //     const enum_field_tuple = try get_enum_field(s, tir_enum, air_project.tag);

            //     const tag = enum_field_tuple[1];

            //     // Figure out the return type of the project instruction.
            //     // It is a pointer to the data structure of the tag projecting into.
            //     const cur_field_index = tir_enum.fields_start + tag;
            //     const cur_field = s.tir.values.get(cur_field_index);
            //     const field_type = cur_field.field_typ.field_type;
            //     const type_index = try s.append_val(Value{ .ptr_typ = .{ .deref_type = field_type, .cap = .ref_typ } });

            //     // Finally, append the instruction.
            //     const tir_project = TirInst{ .enum_project = .{ .enum_ptr = @intFromEnum(tir_enum_ptr_inst), .enum_type = tir_enum_type, .tag = enum_field_tuple[1], .ret_type = @enumFromInt(type_index) } };
            //     const tir_project_inst = try s.append_inst(tir_project);
            //     try s.air_tir_inst_map.put(air_ref, tir_project_inst);
            // },
            .ret_empty => {
                // TODO: Typecheck
                const tir_ret_inst = try s.append_inst(.ret_void);
                try s.air_tir_inst_map.put(air_index, tir_ret_inst);
                return tir_ret_inst;
            },
            // .ret => |ret| {
            //     // const tir_ret_val = try s.get_inst_mapping(ret);
            //     const tir_ret_val = try s.get_val_mapping(ret);
            //     const tir_ret_inst = try s.append_inst(TirInst{ .ret = .{ .val = tir_ret_val, .ret_type = .typ_typ } });
            //     try s.air_tir_inst_map.put(air_ref, tir_ret_inst);
            //     return tir_ret_inst;
            // },
            .br => |br| {
                const air_dst: AirInst.Index = @intFromEnum(br);

                if (s.air_tir_inst_map.get(air_dst)) |tir_dst| {

                    // const tir_mapped_inst = t.instructions.get(@intFromEnum(tir_dst));
                    // switch (tir_mapped_inst) {
                    //     .block => |blk| {

                    //     }
                    // }

                    const tir_br_index = try s.append_inst(.{ .br = tir_dst });
                    try s.air_tir_inst_map.put(air_index, tir_br_index);
                    return tir_br_index;
                } else {
                    const tir_br_index = try s.append_inst(undefined);
                    try s.air_tir_inst_map.put(
                        air_index,
                        tir_br_index,
                    );

                    _ = try tir_gen_blk(s, air_dst, false);

                    const br_dest = try s.get_inst_mapping(@intFromEnum(br));
                    s.tir.instructions.set(tir_br_index, .{ .br = br_dest });
                    return tir_br_index;
                }
            },
            .br_either => |br_either| {
                const br = s.air.get_extra_struct(AirInst.BrEither, br_either);
                const tir_cond_val = s.get_val_air(br.cond);

                const then_blk = s.air.instructions.get(@intFromEnum(br.then_blk));
                const else_blk = s.air.instructions.get(@intFromEnum(br.else_blk));
                if (tir_cond_val) |val| {
                    // print("Inling br either \n", .{});
                    switch (val) {
                        .true_val => return tir_gen_bb(s, @intFromEnum(then_blk.block.start) + 1, true),
                        .false_val => return tir_gen_bb(s, @intFromEnum(else_blk.block.start) + 1, true),
                        else => return error.ExpectedBoolean,
                    }
                } else {
                    // print("Inling br either not possible\n", .{});
                    // Compile-time evaluation not possible.
                    const tir_cond = try s.get_inst_mapping(br.cond.get_index());
                    const cond_typ = try get_tir_inst_ret_type(s, tir_cond);
                    if (cond_typ != .boolean_typ) {
                        return error.ExpectedBoolean;
                    }
                    var tir_br_inst = TirInst{ .br_either = .{ .cond = tir_cond, .then_blk = undefined, .else_blk = undefined } };
                    const inst_index = try s.append_inst(tir_br_inst);

                    try s.air_tir_inst_map.put(air_index, inst_index);
                    tir_br_inst.br_either.then_blk = try tir_gen_blk(s, @intFromEnum(br.then_blk), false);
                    tir_br_inst.br_either.else_blk = try tir_gen_blk(s, @intFromEnum(br.else_blk), false);
                    s.tir.instructions.set(inst_index, tir_br_inst);
                    return inst_index;
                }
            },
            .lt => |lt| {
                // Currently, the only values stored in AirInst.Ref are true, false and null.
                // Less-than can operate on none of these.
                if (lt.lhs.is_index() == false or lt.rhs.is_index() == false) {
                    return error.MismatchedTypes;
                }

                var lhs_tir_ref: TirInst.Index = try s.get_inst_mapping(lt.lhs.get_index());
                const lhs_val = s.get_val_noref(lhs_tir_ref);
                var rhs_tir_ref: TirInst.Index = try s.get_inst_mapping(lt.rhs.get_index());
                var lhs_typ = try get_tir_inst_ret_type(s, lhs_tir_ref);
                var rhs_typ = try get_tir_inst_ret_type(s, rhs_tir_ref);

                const rhs_val = s.get_val_noref(rhs_tir_ref);
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
                            if (c_l_val.unknown_int_val < c_r_val.unknown_int_val) {
                                try s.append_constant_val_ref(.true_val, air_index);
                            } else {
                                try s.append_constant_val_ref(.false_val, air_index);
                            }
                        } else {
                            return error.Unimplemented;
                        }
                        // else if (c_l_val == .i8_val and c_r_val == .i8_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.i8_val < c_r_val.i8_val }, air_index);
                        // } else if (c_l_val == .i16_val and c_r_val == .i16_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.i16_val < c_r_val.i16_val }, air_index);
                        // } else if (c_l_val == .i32_val and c_r_val == .i32_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.i32_val < c_r_val.i32_val }, air_index);
                        // } else if (c_l_val == .i64_val and c_r_val == .i64_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.i64_val < c_r_val.i64_val }, air_index);
                        // } else if (c_l_val == .u8_val and c_r_val == .u8_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.u8_val < c_r_val.u8_val }, air_index);
                        // } else if (c_l_val == .u16_val and c_r_val == .u16_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.u16_val < c_r_val.u16_val }, air_index);
                        // } else if (c_l_val == .u32_val and c_r_val == .u32_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.u32_val < c_r_val.u32_val }, air_index);
                        // } else if (c_l_val == .u64_val and c_r_val == .u64_val) {
                        //     try s.append_constant_val(Value{ .boolean_val = c_l_val.u64_val < c_r_val.u64_val }, air_index);
                        // } else {
                        //     return error.Unimplemented;
                        // }
                        continue;
                    }
                }

                // Compile-time evaluation not possible.
                if (lhs_val) |l_val_ref| {
                    if (l_val_ref == .unknown_int_val) {
                        const coerced_val = try coerce_unknown_int(l_val_ref.unknown_int_val, rhs_typ);
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

                var res_inst: TirInst.Index = undefined;
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
                try s.air_tir_inst_map.put(air_index, res_inst);
            },

            .add => |air_add| {
                // Currently, the only values stored in AirInst.Ref are true, false and null.
                // Add can operate on none of these.
                if (air_add.lhs.is_index() == false or air_add.rhs.is_index() == false) {
                    return error.MismatchedTypes;
                }
                // TODO: Consider storing most comptime results in the value list by default, only emitting constant instructions
                // when the results are needed for a non-comptime result.
                var lhs_tir_ref: TirInst.Index = try s.get_inst_mapping(air_add.lhs.get_index());
                const lhs_val = s.get_val_noref(lhs_tir_ref);
                var rhs_tir_ref: TirInst.Index = try s.get_inst_mapping(air_add.rhs.get_index());
                const rhs_val = s.get_val_noref(rhs_tir_ref);
                if (lhs_val) |l_val| {
                    if (rhs_val) |r_val| {
                        if (l_val == .unknown_int_val and r_val == .unknown_int_val) {
                            try s.append_constant_val(Value{ .unknown_int_val = l_val.unknown_int_val + r_val.unknown_int_val }, air_index, false);
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

                var res_inst: TirInst.Index = undefined;
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
                try s.air_tir_inst_map.put(air_index, res_inst);
            },
            .type_of, .type_of_deref => |air_type_inst| {
                const maybe_mapped_type: ?Value.IndexRef = s.get_val_air(air_type_inst);
                if (maybe_mapped_type) |_| {
                    try s.append_constant_val_ref(.typ_typ, air_index);
                } else {
                    // Safe index conversion since air_type_inst has no TIR comp-time available value.
                    const tir_expr_inst = try s.get_inst_mapping(air_type_inst.get_index());
                    print("{} {}\n\n", .{
                        air_type_inst,
                        tir_expr_inst,
                    });
                    var tir_expr_type = try get_tir_inst_ret_type(s, tir_expr_inst);
                    if (air_inst == .type_of) {
                        // Head .stackref pointers are omitted when performing type-of.
                        if (tir_expr_type.is_index()) {
                            const val = s.tir.values.get(tir_expr_type.get_index());
                            if (val == .ptr_typ and val.ptr_typ.cap == .stackref_typ) {
                                tir_expr_type = val.ptr_typ.deref_type;
                            }
                        }
                        try s.append_constant_val_ref(tir_expr_type, air_index);
                    } else {
                        const ptr_type = s.tir.values.get(@intFromEnum(tir_expr_type)).ptr_typ.deref_type;
                        try s.append_constant_val_ref(ptr_type, air_index);
                    }
                }
            },
            .type_as => |type_as| {
                const tir_type_val = s.get_val_air(type_as.type) orelse return error.TypeMustBeComptime;

                // If the expression is comp-time known, we may be able to convert comptime-ints
                // to their well-typed equivalents.
                const maybe_expr_ref = s.get_val_air(type_as.expr);
                if (maybe_expr_ref) |expr_ref| {
                    if (expr_ref.is_index()) {
                        const expr_val = s.tir.values.get(expr_ref.get_index());
                        if (expr_val == .unknown_int_val) {
                            const coerced_val = try coerce_unknown_int(expr_val.unknown_int_val, tir_type_val);
                            try s.append_constant_val(coerced_val, air_index, false);
                            continue;
                        }
                    }
                }

                // For all other cases, we just check if the type-as passes type-checking
                // since no conversions need to be done.

                // Safe to convert to index, since it has already been determined expr is
                // not comp-time known.
                const tir_expr_ref = try s.get_inst_mapping(type_as.expr.get_index());
                const expr_type_ref = try get_tir_inst_ret_type(s, tir_expr_ref);

                try print_type(&s.tir, tir_type_val);
                try print_type(&s.tir, expr_type_ref);

                if (types_are_equal(s, expr_type_ref, tir_type_val, false, true)) {
                    try s.air_tir_inst_map.put(air_index, tir_expr_ref);
                    // } else if (type_ref_eq_stackref_coerce(s, expr_type_ref, tir_type_ref)) {
                    //     try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                    // } else if (type_ref_eq_stackref_coerce(s, tir_type_ref, expr_type_ref)) {
                    //     try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                } else {
                    return error.MismatchedTypes;
                }

                // const maybe_val = s.get_val(tir_expr_ref);
                // if (maybe_val) |val| {
                //     switch (tir_type_val) {
                //         .i8_typ,
                //         .i16_typ,
                //         .i32_typ,
                //         .i64_typ,
                //         .u8_typ,
                //         .u16_typ,
                //         .u32_typ,
                //         .u64_typ,
                //         => {
                //             if (val == .unknown_int_val) {
                //                 const coerced_val = try coerce_unknown_int(val.unknown_int_val, tir_type_val);
                //                 try s.append_constant_val(coerced_val, air_index);
                //             } // const coerced_val = try coerce_unknown_int(val.unknown_int, tir_type_ref);
                //             // try s.append_constant_val(val, air_index);
                //         },
                //         .unknown_int_typ => {
                //             if (val == .unknown_int_val) {
                //                 try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                //             }
                //         },
                //         .typ_typ => {
                //             try s.air_tir_val_map.put(air_ref, tir_type_val);
                //         },
                //         else => {
                //             print("Val : {}\n", .{val});

                //             print("Type as for ", .{});
                //             try print_type(&s.tir, tir_type_val);
                //             print(" not impl.\n", .{});
                //             return error.Unimplemented;
                //         },
                //     }
                // } else {
                // const expr_type_ref = try get_tir_inst_ret_type(s, tir_expr_ref);
                // // const expr_inst = s.tir.instructions.get(@intFromEnum(tir_expr_ref));

                // if (type_ref_eq(s, expr_type_ref, tir_type_val, false)) {
                //     try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                //     // } else if (type_ref_eq_stackref_coerce(s, expr_type_ref, tir_type_ref)) {
                //     //     try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                //     // } else if (type_ref_eq_stackref_coerce(s, tir_type_ref, expr_type_ref)) {
                //     //     try s.air_tir_inst_map.put(air_ref, tir_expr_ref);
                // } else {
                //     return error.MismatchedTypes;
                // }
                // }
            },
            .deref => |air_deref| {
                const tir_deref_target = try s.get_inst_mapping(air_deref.get_index());
                const target_type_ref = try get_tir_inst_ret_type(s, tir_deref_target);
                if (target_type_ref.is_index() == false) {
                    return error.DerefOnPrimitive;
                } else {
                    const target_type = s.tir.values.get(@intFromEnum(target_type_ref));
                    if (target_type == .ptr_typ and target_type.ptr_typ.cap != .stackref_typ) {
                        // const tir_load = try s.append_inst(TirInst{ .load = .{ .ptr_typ = tir_deref_target, .type = target_type.ptr.deref_type } });
                        try s.air_tir_inst_map.put(air_index, tir_deref_target);
                    } else {
                        return error.DerefOnInvalidType;
                    }
                }
            },
            // TODO: Different handling for alloca mut
            .alloca, .alloca_mut => |air_alloc| {
                const tir_alloc_type = s.get_val_air(air_alloc.type) orelse return error.TypeMustBeComptime;
                const ret_type_index = try s.intern_val(Value{ .ptr_typ = .{ .deref_type = tir_alloc_type, .cap = .stackref_typ } });

                const tir_alloca = try s.append_inst(.{ .alloca = .{ .alloc_type = tir_alloc_type, .ret_type = ret_type_index } });
                try s.air_tir_inst_map.put(air_index, tir_alloca);
            },
            .memalloc => |air_memalloc| {
                const tir_expr = try s.get_inst_mapping(air_memalloc.expr.get_index());
                const tir_expr_type_ref = try get_tir_inst_ret_type(s, tir_expr);

                const tir_expr_type = s.tir.values.get(@intFromEnum(tir_expr_type_ref));
                if (tir_expr_type == .ptr_typ and tir_expr_type.ptr_typ.cap == .stackref_typ) {
                    // const ret_type = try t.append_type(Type{ .ptr_typ = .{.deref_type = tir_expr_type_ref, .cap = } });
                    const ptr_type_ref = try s.intern_val(Value{ .ptr_typ = .{ .deref_type = tir_expr_type.ptr_typ.deref_type, .cap = .own_typ } });
                    const tir_memalloc = try s.append_inst(.{ .memalloc = .{ .expr = tir_expr, .ptr_type = ptr_type_ref } });
                    try s.air_tir_inst_map.put(air_index, tir_memalloc);
                } else {
                    return error.Unimplemented;
                }
            },
            .memfree => |air_memfree| {
                const tir_ptr = try s.get_inst_mapping(air_memfree.expr.get_index());
                const tir_ptr_type_ref = try get_tir_inst_ret_type(s, tir_ptr);

                const tir_ptr_type = s.tir.values.get(@intFromEnum(tir_ptr_type_ref));
                if (tir_ptr_type == .ptr_typ and tir_ptr_type.ptr_typ.cap == .own_typ) {
                    const expr_type = tir_ptr_type.ptr_typ.deref_type;
                    const ptr_type_ref = try s.intern_val(Value{ .ptr_typ = .{ .deref_type = expr_type, .cap = .stackref_typ } });
                    const tir_memfree = try s.append_inst(.{ .memfree = .{ .ptr = tir_ptr, .expr_type = ptr_type_ref } });
                    try s.air_tir_inst_map.put(air_index, tir_memfree);
                } else {
                    return error.MemfreeOnInvalidType;
                }
            },
            .print => |p| {
                const tir_expr = try s.get_inst_mapping(p.expr.get_index());
                const tir_print = try s.append_inst(.{ .print = .{ .val = tir_expr } });
                try s.air_tir_inst_map.put(air_index, tir_print);
            },
            .address_of => |air_address_of| {
                // We only allow address of types
                const tir_target_type = if (air_address_of.target == .address_of_self)
                    .opaque_typ
                else
                    s.get_val_air(air_address_of.target) orelse return error.Unimplemented;

                const tir_cap_type = s.get_val_air(air_address_of.cap) orelse return error.CapMustBeComptime;
                try s.append_constant_val(Value{ .ptr_typ = .{ .deref_type = tir_target_type, .cap = tir_cap_type } }, air_index, true);
            },
            // .zero_array => |air_zero_array| {
            //     const tir_array_type = try s.get_val_mapping(air_zero_array);
            //     const tir_array = try s.append_inst(.{ .zero_array = tir_array_type });
            //     try s.air_tir_inst_map.put(air_ref, tir_array);
            // },
            .get => |air_get| {
                const lval = try s.get_inst_mapping(air_get.src.get_index());
                const lval_type = try get_tir_inst_ret_type(s, lval);
                // print("Loading from inst {} with type {}\n", .{ tir_ptr, tir_ptr_type });
                switch (lval_type) {
                    .boolean_typ, .unknown_int_typ, .u64_typ, .u32_typ, .u16_typ, .u8_typ, .i64_typ, .i32_typ, .i16_typ, .i8_typ, .void_typ, .own_typ, .ref_typ, .stackref_typ, .opaque_typ, .true_val, .false_val, .null_val => {
                        return error.PtrIsNotPtrType;
                    },
                    .typ_typ => {
                        // For types, get is a no-op.
                        try s.set_inst_mapping(air_index, lval);
                    },

                    _ => {
                        const tir_type = s.tir.values.get(@intFromEnum(lval_type));
                        switch (tir_type) {
                            .ptr_typ => |ptr| {
                                var tir_load_type: Value.IndexRef = undefined;
                                if (air_get.cap == .own) {
                                    tir_load_type = ptr.deref_type;
                                } else {
                                    const copy_equiv_ref = try get_copy_equiv_type(s, lval_type);
                                    // print("Copy equiv {}", .{copy_equiv_ref});
                                    const copy_equiv = s.tir.values.get(@intFromEnum(copy_equiv_ref));
                                    // print("Copy equiv {}", .{copy_equiv});
                                    tir_load_type = copy_equiv.ptr_typ.deref_type;
                                }

                                const tir_load = try s.append_inst(TirInst{ .load = .{ .ptr = lval, .type = tir_load_type } });
                                try s.air_tir_inst_map.put(air_index, tir_load);
                            },
                            else => return error.PtrIsNotPtrType,
                        }
                    },
                }

                // const tir_type = try t.get_type_mapping(air_load.type);

            },
            .store => |air_store| {
                // TODO: Comptime stores.
                // if (s.get_val_air(air_store.val)) |_| {
                //     return error.Unimplemented;
                // }

                const tir_val_inst = try s.get_inst_mapping(air_store.val.get_index());
                const tir_val_type_ref = try get_tir_inst_ret_type(s, tir_val_inst);

                // TODO: Could be a dangerous index conversion..
                const tir_ptr_inst = try s.get_inst_mapping(air_store.ptr.get_index());
                const tir_ptr_type = try get_tir_inst_ret_type(s, tir_ptr_inst);

                const tir_pointed_to_type = s.tir.values.get(@intFromEnum(tir_ptr_type)).ptr_typ.deref_type;

                if (types_are_equal(s, tir_val_type_ref, tir_pointed_to_type, false, true) == false) {
                    print("{} {}\n", .{ tir_val_inst, tir_ptr_inst });
                    return error.StoringWrongType;
                }
                // if (type_ref_eq(s, tir_val_type_ref, tir_pointed_to_type, false) == false and type_ref_eq_stackref_coerce(s, tir_val_type_ref, tir_pointed_to_type) == false) {
                //     print("{} {}\n", .{ tir_val_inst, tir_ptr_inst });
                //     return error.StoringWrongType;
                // }

                const tir_store = try s.append_inst(TirInst{ .store = .{ .val = tir_val_inst, .ptr = tir_ptr_inst } });
                try s.air_tir_inst_map.put(air_index, tir_store);
            },
            .move => |air_move| {
                // TODO: Could be a dangerous index conversion..
                const tir_target = try s.get_inst_mapping(air_move.get_index());
                const tir_move = try s.append_inst(TirInst{ .move = tir_target });
                try s.air_tir_inst_map.put(air_index, tir_move);
            },
            // .arg => |air_arg| {
            //     const tir_type_ref = try s.get_val_mapping(air_arg.type);
            //     const tir_arg = TirInst{ .arg = .{ .name = air_arg.name, .typ_ref = tir_type_ref } };
            //     const inst_index = try s.append_inst(tir_arg);
            //     if (tir_type_ref == .typ_typ) {
            //         if (s.air_tir_val_map.get(air_ref)) |arg_type| {
            //             // print("AT AIR ARG {} ADDING ARG TYPE MAP TO {}\n", .{ air_index, arg_type });
            //             try s.air_tir_val_map.put(air_ref, arg_type);
            //         } else {
            //             try s.air_tir_val_map.put(air_ref, .typ_typ);
            //         }
            //         // const arg_type = try s.get_type_mapping(air_ref);
            //     }
            //     try s.air_tir_inst_map.put(air_ref, inst_index);
            // },
            // .indexing => |indexing| {
            //     const tir_target_inst: ?TirInst.Index = s.get_inst_mapping(indexing.target) catch null;
            //     const tir_target_type: ?Value.IndexRef = s.get_val_mapping(indexing.target) catch null;

            //     if (tir_target_inst) |inst| {
            //         // We are indexing into a value
            //         const target_inst = try get_tir_inst_ret_type(s, inst);
            //         // print("Target inst: {}\n", .{target_inst});
            //         const target_type = s.tir.values.get(@intFromEnum(target_inst));
            //         if (target_type != .array_typ) {
            //             return error.IndexIntoNonArrayType;
            //         }
            //         const element_type = target_type.array_typ.element_type;

            //         // Check if the index is comptime available
            //         const tir_index_expr = try s.get_inst_mapping(indexing.index);
            //         const maybe_val = s.get_val(tir_index_expr);
            //         if (maybe_val) |val| {
            //             const val_index = try s.append_val(val);
            //             const const_index = try s.append_inst(.{ .constant_index = .{ .index = val_index, .target = inst, .elem_type = element_type } });
            //             try s.air_tir_inst_map.put(air_ref, const_index);
            //         } else {
            //             const runtime_index = try s.append_inst(.{ .index = .{ .index = @intFromEnum(tir_index_expr), .target = inst, .elem_type = element_type } });
            //             try s.air_tir_inst_map.put(air_ref, runtime_index);
            //         }
            //     } else if (tir_target_type) |typ| {
            //         // We are indexing into a type i.e. creating an array type.
            //         // Index must be comptime available.

            //         // Check if the index is comptime available
            //         const tir_index_expr = try s.get_inst_mapping(indexing.index);
            //         const maybe_val = s.get_val(tir_index_expr);
            //         if (maybe_val) |val| {
            //             // const val_index = try t.append_val(val);
            //             // const const_index = try t.append_inst(.{ .constant_index = .{ .index = val_index, .target = tir_target_inst, .elem_type = element_type } });
            //             // try t.air_tir_inst_map.put(air_ref, const_index);
            //             if (val != .unknown_int_val) {
            //                 return error.Unimplemented;
            //             }

            //             const array_type = Value{ .array_typ = .{ .size = @intCast(val.unknown_int_val), .element_type = typ } };
            //             const type_index = try s.append_val_ref(array_type);
            //             const type_inst = try s.append_inst(.{ .constant_type = type_index });
            //             try s.air_tir_inst_map.put(air_ref, type_inst);
            //             try s.air_tir_val_map.put(air_ref, type_index);
            //         } else {
            //             return error.ArrayTypeMustHaveKnownSize;
            //         }
            //     }
            // },
            .int => |int| {
                const val = Value{ .unknown_int_val = @intCast(int) };
                try s.append_constant_val(val, air_index, false);
            },
            else => {
                print("TIR generation unimplemented for {}\n", .{air_inst});
                return error.Unimplemented;
            },
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
        // .air_tir_val_map = TirState.AirTirValMap.init(allocator),
        .value_intern_map = std.AutoHashMap(u64, Value.Index).init(allocator),

        .fn_def_map = TirState.FnDefMap.init(allocator),
        .field_maps = TirState.FieldMaps.init(allocator),
    };
    defer s.deinit();
    // try s.air_tir_inst_map.put(AirInst.IndexRef.true_lit, TirInst.Index.true_val);
    // try s.air_tir_inst_map.put(AirInst.IndexRef.false_lit, TirInst.Index.false_val);
    // try s.air_tir_inst_map.put(AirInst.IndexRef.null_lit, TirInst.Index.null_val);

    // try s.air_tir_val_map.put(AirInst.IndexRef.bool, Value.IndexRef.boolean_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.u8, Value.IndexRef.u8_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.u16, Value.IndexRef.u16_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.u32, Value.IndexRef.u32_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.u64, Value.IndexRef.u64_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.i8, Value.IndexRef.i8_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.i16, Value.IndexRef.i16_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.i32, Value.IndexRef.i32_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.i64, Value.IndexRef.i64_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.void, Value.IndexRef.void_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.own, Value.IndexRef.own_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.ref, Value.IndexRef.ref_typ);
    // try s.air_tir_val_map.put(AirInst.IndexRef.type, Value.IndexRef.typ_typ);
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
