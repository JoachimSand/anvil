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

// Represents a compile-time known type
const Type = union(enum) {
    const Index = u32;
    const List = std.MultiArrayList(Type);
    const Field = struct { var_name: Air.StringIndex, field_type: Type.Index };

    // TODO: Maybe remove all primitives which are already in TirInst.IndexRef?
    // boolean,
    // tir_u64,
    // tir_u32,
    // tir_u16,
    // tir_u8,
    // tir_i64,
    // tir_i32,
    // tir_i16,
    // tir_i8,
    // typ,
    // We need to either store mapping from field names to typeindexes in
    // TIR or re-use the AIR mappings.
    type_struct: []Field,
    // strct: []Type,
    // array: *Type,
};

//Compile-time known values.
const Value = union(enum) {
    const Index = u32;
    const List = std.MultiArrayList(Value);
    unknown_int: i64,
    u64: u64,
    u32: u32,
    u16: u16,
    u8: u8,
    i64: i64,
    i32: i32,
    i16: i16,
    i8: i8,
    boolean: bool,
};

// Typed intermediate representation
const TirInst = union(enum) {
    const Index = u32;
    const IndexRef = enum(Index) { tir_boolean = 4294967040, tir_true_lit, tir_false_lit, tir_unknown_int, tir_u64, tir_u32, tir_u16, tir_u8, tir_i64, tir_i32, tir_i16, tir_i8, tir_typ, _ };
    const List = std.MultiArrayList(TirInst);

    const BinOp = struct {
        lhs: IndexRef,
        rhs: IndexRef,
    };

    arg: struct {
        typ_ref: IndexRef,
        name: Air.StringIndex,
    },
    block: struct {
        start: Index,
        end: Index,
    },
    br: struct {
        cond: Index,
        then_blk: Index,
        else_blk: Index,
    },
    constant_val: Value.Index,
    constant_type: Type.Index,
    add_i8: BinOp,
    add_i16: BinOp,
    add_i32: BinOp,
    add_i64: BinOp,

    add_u8: BinOp,
    add_u16: BinOp,
    add_u32: BinOp,
    add_u64: BinOp,
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
    IntTooLarge,
    CannotCoerceKnownToUnknown,
};

pub const TirError = TirSpecificError || Allocator.Error;

const TirState = struct {
    const AirTirMap = std.AutoHashMap(AirInst.IndexRef, TirInst.IndexRef);

    instructions: TirInst.List,
    extra: std.ArrayList(TirInst.IndexRef),
    scratch: std.ArrayList(u32),

    air: *Air,
    air_tir_map: AirTirMap,

    // compile-time known values
    values: Value.List,
    types: Type.List,

    allocator: Allocator,

    fn append_inst(t: *TirState, inst: TirInst) Allocator.Error!TirInst.IndexRef {
        const index = t.instructions.len;

        try t.instructions.append(t.allocator, inst);
        return @enumFromInt(index);
    }

    fn append_val(t: *TirState, val: Value) Allocator.Error!Value.Index {
        const index = t.values.len;

        try t.values.append(t.allocator, val);
        return @intCast(index);
    }

    fn append_type(t: *TirState, typ: Type) Allocator.Error!Type.Index {
        const index = t.types.len;

        try t.types.append(t.allocator, typ);
        return @intCast(index);
    }

    fn get_mapping(t: *TirState, air_index: AirInst.IndexRef) !TirInst.IndexRef {
        const tir_ref: TirInst.IndexRef = t.air_tir_map.get(air_index) orelse {
            print("Missing mapping for {}\n", .{air_index});
            return error.MissingMapping;
        };
        return tir_ref;
    }

    fn append_constant_val(t: *TirState, val: Value, air_index: AirInst.Index) !void {
        const val_index = try t.append_val(val);
        const tir_inst = TirInst{ .constant_val = val_index };
        const tir_index = try t.append_inst(tir_inst);
        try t.air_tir_map.put(@enumFromInt(air_index), tir_index);
    }

    fn deinit(t: *TirState) void {
        t.instructions.deinit(t.allocator);
        t.extra.deinit();
        t.scratch.deinit();

        t.air_tir_map.deinit();

        t.values.deinit(t.allocator);
        t.types.deinit(t.allocator);
    }
};

fn print_tir(t: *TirState) !void {
    print("---------- TIR -------------\n", .{});
    var index: u32 = 0;
    while (index < t.instructions.len) {
        // for (0..indent) |_| {
        //     print("    ", .{});
        // }

        print("%{} = ", .{index});
        const inst = t.instructions.get(index);
        switch (inst) {
            // .int => |int| {
            //     print("int({})", .{int});
            // },
            .constant_val => |val_index| {
                const val = t.values.get(val_index);
                print("constant_val(", .{});
                switch (val) {
                    .unknown_int => |int| print("unknown_int, {}", .{int}),
                    .i8 => |int| print("i8, {}", .{int}),
                    else => return error.Unimplemented,
                }
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
            .arg => |arg| {
                print("arg(%{}, %{})", .{ arg.typ_ref, arg.name });
            },
            .br => |br| {
                print("br %{}, then %{} else %{}", .{ br.cond, br.then_blk, br.else_blk });
            },
            .block => |blk| {
                print("blk %{} to %{}", .{ blk.start, blk.end });
            },
            else => return error.Unimplemented,
        }
        print(";\n", .{});
        index += 1;
    }
}

fn get_val(t: *TirState, tir_ref: TirInst.IndexRef) !?Value {
    switch (tir_ref) {
        .tir_boolean, .tir_unknown_int, .tir_u64, .tir_u32, .tir_u16, .tir_u8, .tir_i64, .tir_i32, .tir_i16, .tir_i8, .tir_typ => return error.ExpectedVal,
        .tir_true_lit => return Value{ .boolean = true },
        .tir_false_lit => return Value{ .boolean = false },
        _ => {
            const lhs_tir_index: TirInst.Index = @intFromEnum(tir_ref);
            const lhs_tir_inst = t.instructions.get(lhs_tir_index);
            switch (lhs_tir_inst) {
                .constant_val => |val| return t.values.get(val),
                else => return null,
            }
        },
    }
}

// Get the type of the resulting value of a TIR instruction reference
fn get_tir_inst_ret_type(t: *TirState, inst_ref: TirInst.IndexRef) !TirInst.IndexRef {
    switch (inst_ref) {
        .tir_boolean, .tir_unknown_int, .tir_u64, .tir_u32, .tir_u16, .tir_u8, .tir_i64, .tir_i32, .tir_i16, .tir_i8 => {
            // const tir_type = try tir.append_type(.typ);
            // const tir_type_inst = try tir.append_inst(TirInst{ .constant_type = tir_type });
            return .tir_typ;
        },
        .tir_true_lit, .tir_false_lit => return .tir_boolean,
        .tir_typ => return error.TypeOfType,
        _ => {
            // The type of index refers to an instruction.
            // By matching on the instruction, figure out what it's type is.
            const inst_index: TirInst.Index = @intFromEnum(inst_ref);
            const inst = t.instructions.get(inst_index);

            switch (inst) {
                .block, .br => return error.NoType,
                .constant_type => |type_index| {
                    const tir_type = t.types.get(type_index);
                    _ = tir_type;
                    return error.Unimplemented;
                },
                .constant_val => |val_index| {
                    const value = t.values.get(val_index);
                    switch (value) {
                        .unknown_int => return .tir_unknown_int,
                        .i8 => return .tir_i8,
                        .i16 => return .tir_i16,
                        .i32 => return .tir_i32,
                        .i64 => return .tir_i64,
                        .u8 => return .tir_u8,
                        .u16 => return .tir_u16,
                        .u32 => return .tir_u32,
                        .u64 => return .tir_u64,
                        else => return error.Unimplemented,
                    }
                },
                .add_i8 => return .tir_i8,
                .add_i16 => return .tir_i16,
                .add_i32 => return .tir_i32,
                .add_i64 => return .tir_i64,
                .add_u8 => return .tir_u8,
                .add_u16 => return .tir_u16,
                .add_u32 => return .tir_u32,
                .add_u64 => return .tir_u64,
                .arg => |arg| return arg.typ_ref,
                // .add_i8 => tir.air_tir_map.put(@intFromEnum(air_index), .tir_i8),
                // .add_i8 => tir.air_tir_map.put(@intFromEnum(air_index), .tir_i8),
                // .add_i8 => tir.air_tir_map.put(@intFromEnum(air_index), .tir_i8),
                // else => return error.Unimplemented,
            }
        },
    }
}

fn tir_gen_blk(t: *TirState, air_start: u32, air_end: u32) !TirInst.Index {
    var tir_inst = TirInst{ .block = .{ .start = @intCast(t.instructions.len), .end = undefined } };
    const blk_index = try t.append_inst(tir_inst);

    try tir_gen_instructions(t, air_start, air_end);
    tir_inst.block.end = @intCast(t.instructions.len - 1);
    t.instructions.set(@intFromEnum(blk_index), tir_inst);
    return @intFromEnum(blk_index);
}

fn tir_gen_instructions(t: *TirState, air_start: AirInst.Index, air_end: AirInst.Index) TirError!void {
    var air_index: u32 = air_start;
    const air_instructions = t.air.instructions.slice();
    while (air_index < air_end) : (air_index += 1) {
        print("\nGenerating TIR for AIR instruction {}\n", .{air_index});
        try air_mod.print_air(t.air, air_index, air_index + 1, 0);
        print("TIR so far:", .{});
        try print_tir(t);

        const air_inst = air_instructions.get(air_index);
        switch (air_inst) {
            .block => |block| {
                _ = block;
            },
            .fn_def => |fn_def| {
                _ = fn_def;
            },
            .br => |br_extra| {
                const br = t.air.get_extra_struct(AirInst.Br, br_extra);
                const tir_cond = try t.get_mapping(br.cond);
                const tir_cond_val = try get_val(t, tir_cond);

                const then_blk = t.air.instructions.get(@intFromEnum(br.then_blk));
                const else_blk = t.air.instructions.get(@intFromEnum(br.else_blk));
                if (tir_cond_val) |val| {
                    switch (val) {
                        .boolean => |boolean| {
                            if (boolean) {
                                try tir_gen_instructions(t, @intFromEnum(then_blk.block.start), @intFromEnum(then_blk.block.end));
                            } else {
                                try tir_gen_instructions(t, @intFromEnum(else_blk.block.start), @intFromEnum(else_blk.block.end));
                            }
                            const else_end: u32 = @intFromEnum(else_blk.block.end);
                            air_index = else_end;
                        },
                        else => return error.ExpectedBoolean,
                    }
                } else {

                    // Compile-time evaluation not possible.
                    const cond_typ = try get_tir_inst_ret_type(t, tir_cond);
                    if (cond_typ != .tir_boolean) {
                        return error.ExpectedBoolean;
                    }
                    var tir_br_inst = TirInst{ .br = .{ .cond = @intFromEnum(tir_cond), .then_blk = undefined, .else_blk = undefined } };
                    const inst_index = try t.append_inst(tir_br_inst);

                    tir_br_inst.br.then_blk = try tir_gen_blk(t, @intFromEnum(then_blk.block.start), @intFromEnum(then_blk.block.end));
                    tir_br_inst.br.else_blk = try tir_gen_blk(t, @intFromEnum(then_blk.block.start), @intFromEnum(then_blk.block.end));
                    t.instructions.set(@intFromEnum(inst_index), tir_br_inst);
                }
            },
            .add => |air_add| {
                const lhs_tir_ref: TirInst.IndexRef = t.air_tir_map.get(air_add.lhs) orelse return error.MissingMapping;
                const lhs_val = try get_val(t, lhs_tir_ref);
                const rhs_tir_ref: TirInst.IndexRef = t.air_tir_map.get(air_add.rhs) orelse return error.MissingMapping;
                const rhs_val = try get_val(t, rhs_tir_ref);
                if (lhs_val) |l_val| {
                    if (rhs_val) |r_val| {
                        if (l_val == .unknown_int and r_val == .unknown_int) {
                            try t.append_constant_val(Value{ .unknown_int = l_val.unknown_int + r_val.unknown_int }, air_index);
                            continue;
                        } else {
                            return error.Unimplemented;
                        }
                    }
                }

                // Compile-time evaluation not possible.
                const lhs_typ = try get_tir_inst_ret_type(t, lhs_tir_ref);
                const rhs_typ = try get_tir_inst_ret_type(t, rhs_tir_ref);
                var res_inst: TirInst.IndexRef = undefined;
                if (lhs_typ == .tir_i8 and rhs_typ == .tir_i8) {
                    res_inst = try t.append_inst(TirInst{ .add_i8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_i16 and rhs_typ == .tir_i16) {
                    res_inst = try t.append_inst(TirInst{ .add_i16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_i32 and rhs_typ == .tir_i32) {
                    res_inst = try t.append_inst(TirInst{ .add_i32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_i64 and rhs_typ == .tir_i64) {
                    res_inst = try t.append_inst(TirInst{ .add_i64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u8 and rhs_typ == .tir_u8) {
                    res_inst = try t.append_inst(TirInst{ .add_u8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u16 and rhs_typ == .tir_u16) {
                    res_inst = try t.append_inst(TirInst{ .add_u16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u32 and rhs_typ == .tir_u32) {
                    res_inst = try t.append_inst(TirInst{ .add_u32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u64 and rhs_typ == .tir_u64) {
                    res_inst = try t.append_inst(TirInst{ .add_u64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else {
                    return error.AdditionTypeError;
                }
                try t.air_tir_map.put(@enumFromInt(air_index), res_inst);
            },
            .type_of => |air_type_inst| {
                const tir_expr_inst = try t.get_mapping(air_type_inst);
                const tir_expr_type = try get_tir_inst_ret_type(t, tir_expr_inst);
                try t.air_tir_map.put(@enumFromInt(air_index), tir_expr_type);
            },
            .type_as => |type_as| {
                const tir_expr_ref = try t.get_mapping(type_as.expr);
                const tir_type_ref = try t.get_mapping(type_as.type);
                const maybe_val = try get_val(t, tir_expr_ref);
                switch (tir_type_ref) {
                    .tir_i8 => {
                        if (maybe_val) |val| {
                            if (val == .unknown_int) {
                                if (val.unknown_int < (1 << 7) and val.unknown_int >= -(1 << 7)) {
                                    try t.append_constant_val(Value{ .i8 = @intCast(val.unknown_int) }, air_index);
                                } else {
                                    return error.IntTooLarge;
                                }
                            }
                        }
                    },
                    // _ => {
                    //     // const tir_type_inst = try tir.get_mapping(type_as.type);
                    //     // switch (tir_type_inst) {
                    //     //     .boolean
                    //     // }
                    // },
                    .tir_unknown_int => {
                        if (maybe_val) |val| {
                            if (val == .unknown_int) {
                                try t.air_tir_map.put(@enumFromInt(air_index), tir_expr_ref);
                            } else {
                                return error.CannotCoerceKnownToUnknown;
                            }
                        }
                    },
                    else => {
                        print("Type as for {} not impl.\n", .{tir_type_ref});
                        return error.Unimplemented;
                    },
                }
            },
            .arg => |air_arg| {
                const tir_type_ref = try t.get_mapping(air_arg.type);
                const tir_arg = TirInst{ .arg = .{ .name = air_arg.name, .typ_ref = tir_type_ref } };
                const inst_index = try t.append_inst(tir_arg);
                try t.air_tir_map.put(@enumFromInt(air_index), inst_index);
            },
            .int => |int| {
                const val = Value{ .unknown_int = @intCast(int) };
                try t.append_constant_val(val, air_index);
            },
            else => return error.Unimplemented,
        }
    }
}

pub fn tir_gen(air: *Air, allocator: Allocator) !void {
    var tir = TirState{
        .instructions = TirInst.List{},
        .extra = std.ArrayList(TirInst.IndexRef).init(allocator),
        .scratch = std.ArrayList(u32).init(allocator),

        .air = air,
        .air_tir_map = TirState.AirTirMap.init(allocator),

        .values = Value.List{},
        .types = Type.List{},

        .allocator = allocator,
    };
    defer tir.deinit();

    try tir.air_tir_map.put(AirInst.IndexRef.bool, TirInst.IndexRef.tir_boolean);
    try tir.air_tir_map.put(AirInst.IndexRef.true_lit, TirInst.IndexRef.tir_true_lit);
    try tir.air_tir_map.put(AirInst.IndexRef.false_lit, TirInst.IndexRef.tir_false_lit);
    try tir.air_tir_map.put(AirInst.IndexRef.u8, TirInst.IndexRef.tir_u8);
    try tir.air_tir_map.put(AirInst.IndexRef.u16, TirInst.IndexRef.tir_u16);
    try tir.air_tir_map.put(AirInst.IndexRef.u32, TirInst.IndexRef.tir_u32);
    try tir.air_tir_map.put(AirInst.IndexRef.u64, TirInst.IndexRef.tir_u64);

    try tir.air_tir_map.put(AirInst.IndexRef.i8, TirInst.IndexRef.tir_i8);
    try tir.air_tir_map.put(AirInst.IndexRef.i16, TirInst.IndexRef.tir_i16);
    try tir.air_tir_map.put(AirInst.IndexRef.i32, TirInst.IndexRef.tir_i32);
    try tir.air_tir_map.put(AirInst.IndexRef.i64, TirInst.IndexRef.tir_i64);

    try tir_gen_instructions(&tir, 0, @intCast(tir.air.instructions.len));
    try print_tir(&tir);
}
