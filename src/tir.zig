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
    const IndexRef = enum(Index) { tir_boolean = 4294967040, tir_unknown_int, tir_u64, tir_u32, tir_u16, tir_u8, tir_i64, tir_i32, tir_i16, tir_i8, tir_typ, _ };
    const List = std.MultiArrayList(Type);
    const Field = struct { var_name: Air.StringIndex, field_type: Type.Index };

    ptr: Type.IndexRef,
    type_struct: []Field,
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
    const IndexRef = enum(Index) { tir_true_lit = 4294967040, tir_false_lit, _ };
    const List = std.MultiArrayList(TirInst);

    const BinOp = struct {
        lhs: IndexRef,
        rhs: IndexRef,
    };

    arg: struct {
        typ_ref: Type.IndexRef,
        name: Air.StringIndex,
    },
    block: struct {
        start: Index,
        end: Index,
    },
    br: Index,
    br_cond: struct { cond: Index, blk: Index },
    br_either: struct {
        cond: Index,
        then_blk: Index,
        else_blk: Index,
    },
    constant_val: Value.Index,
    constant_type: Type.IndexRef,
    alloca: Type.IndexRef,
    load: struct {
        ptr: IndexRef,
        type: Type.IndexRef,
    },
    store: struct {
        val: IndexRef,
        val_type: Type.IndexRef,
        ptr: IndexRef,
    },
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
    StoringWrongType,
};

pub const TirError = TirSpecificError || Allocator.Error;

const TirState = struct {
    const AirTirInstMap = std.AutoHashMap(AirInst.IndexRef, TirInst.IndexRef);
    const AirTirTypeMap = std.AutoHashMap(AirInst.IndexRef, Type.IndexRef);

    instructions: TirInst.List,
    extra: std.ArrayList(TirInst.IndexRef),
    scratch: std.ArrayList(u32),

    air: *Air,
    air_tir_inst_map: AirTirInstMap,
    air_tir_type_map: AirTirTypeMap,

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

    fn get_inst_mapping(t: *TirState, air_index: AirInst.IndexRef) !TirInst.IndexRef {
        const tir_ref: TirInst.IndexRef = t.air_tir_inst_map.get(air_index) orelse {
            print("Missing instruction mapping for {}\n", .{air_index});
            return error.MissingMapping;
        };
        return tir_ref;
    }

    fn get_type_mapping(t: *TirState, air_index: AirInst.IndexRef) !Type.IndexRef {
        const tir_ref: Type.IndexRef = t.air_tir_type_map.get(air_index) orelse {
            print("Missing instruction mapping for {}\n", .{air_index});
            return error.MissingMapping;
        };
        return tir_ref;
    }

    fn append_constant_val(t: *TirState, val: Value, air_index: AirInst.Index) !void {
        const val_index = try t.append_val(val);
        const tir_inst = TirInst{ .constant_val = val_index };
        const tir_index = try t.append_inst(tir_inst);
        try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_index);
    }

    fn deinit(t: *TirState) void {
        t.instructions.deinit(t.allocator);
        t.extra.deinit();
        t.scratch.deinit();

        t.air_tir_inst_map.deinit();
        t.air_tir_type_map.deinit();

        t.values.deinit(t.allocator);
        t.types.deinit(t.allocator);
    }
};

fn print_tir(t: *TirState, start: u32, stop: u32, indent: u32) !void {
    // print("---------- TIR -------------\n", .{});
    var index: u32 = start;
    while (index <= stop) {
        for (0..indent) |_| {
            print("    ", .{});
        }

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
                    .boolean => |boolean| print("bool, {}", .{boolean}),
                    .unknown_int => |int| print("unknown_int, {}", .{int}),
                    .i8 => |int| print("i8, {}", .{int}),
                    .i16 => |int| print("i16, {}", .{int}),
                    .i32 => |int| print("i32, {}", .{int}),
                    .i64 => |int| print("i16, {}", .{int}),
                    .u8 => |int| print("u8, {}", .{int}),
                    .u16 => |int| print("u16, {}", .{int}),
                    .u32 => |int| print("u32, {}", .{int}),
                    .u64 => |int| print("u64, {}", .{int}),
                    // else => return error.Unimplemented,
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

            .lt_i8, .lt_i16, .lt_i32, .lt_i64, .lt_u8, .lt_u16, .lt_u32, .lt_u64 => |lt| {
                print(" %{s}, ${} < ${}", .{ @tagName(inst), lt.lhs, lt.rhs });
            },

            .alloca => |alloca_type| {
                print("alloca {}", .{alloca_type});
            },
            .load => |load| {
                print("load {}, {}", .{ load.type, load.ptr });
            },
            .store => |store| {
                print("store {} from {}, {}", .{ store.val_type, store.val, store.ptr });
            },
            .arg => |arg| {
                print("arg(%{}, %{})", .{ arg.typ_ref, arg.name });
            },
            .br => |br| {
                print("br %{}", .{br});
            },
            .br_either => |br| {
                print("br %{}, then %{} else %{}", .{ br.cond, br.then_blk, br.else_blk });
            },
            .block => |blk| {
                print("blk %{} to %{} {{\n", .{ blk.start, blk.end });
                try print_tir(t, blk.start, blk.end, indent + 1);
                index = blk.end;
                for (0..indent) |_| {
                    print("    ", .{});
                }
                print("}}", .{});
            },
            else => return error.Unimplemented,
        }
        print(";\n", .{});
        index += 1;
    }
}

fn get_val(t: *TirState, tir_ref: TirInst.IndexRef) !?Value {
    switch (tir_ref) {
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
fn get_tir_inst_ret_type(t: *TirState, inst_ref: TirInst.IndexRef) !Type.IndexRef {
    switch (inst_ref) {
        .tir_true_lit, .tir_false_lit => return .tir_boolean,

        _ => {
            // The type of index refers to an instruction.
            // By matching on the instruction, figure out what it's type is.
            const inst_index: TirInst.Index = @intFromEnum(inst_ref);
            const inst = t.instructions.get(inst_index);

            switch (inst) {
                .lt_i8, .lt_i16, .lt_i32, .lt_i64, .lt_u8, .lt_u16, .lt_u32, .lt_u64 => return .tir_boolean,
                .block, .br, .br_cond, .br_either => return error.NoType,
                .constant_type => |type_ref| return type_ref,
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
                .alloca => |type_ref| return type_ref,
                .store => |store| {
                    const val_type = try get_tir_inst_ret_type(t, store.val);
                    const type_index = try t.append_type(Type{ .ptr = val_type });
                    return @enumFromInt(type_index);
                },
                .load => |load| {
                    return load.type;
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

fn coerce_unknown_int(unknown_int: i64, target_type: Type.IndexRef) !Value {
    switch (target_type) {
        .tir_i8 => {
            if (unknown_int < (1 << 7) and unknown_int >= -(1 << 7)) {
                return Value{ .i8 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_i16 => {
            if (unknown_int < (1 << 15) and unknown_int >= -(1 << 15)) {
                return Value{ .i16 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_i32 => {
            if (unknown_int < (1 << 31) and unknown_int >= -(1 << 31)) {
                return Value{ .i32 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_i64 => {
            if (unknown_int < (1 << 63) and unknown_int >= -(1 << 63)) {
                return Value{ .i64 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_u8 => {
            if (unknown_int < (1 << 8)) {
                return Value{ .u8 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_u16 => {
            if (unknown_int < (1 << 16)) {
                return Value{ .u16 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_u32 => {
            if (unknown_int < (1 << 32)) {
                return Value{ .u32 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        .tir_u64 => {
            if (unknown_int < (1 << 64)) {
                return Value{ .u64 = @intCast(unknown_int) };
            } else {
                return error.IntTooLarge;
            }
        },
        else => return error.IntCoercionIntoNonInt,
    }
}

fn tir_gen_blk(t: *TirState, air_blk_index: AirInst.Index) !TirInst.Index {
    const tir_existing_blk = t.air_tir_inst_map.get(@enumFromInt(air_blk_index));
    if (tir_existing_blk) |existing_blk| {
        return @intFromEnum(existing_blk);
    }

    const tir_blk_index = try t.append_inst(undefined);
    var tir_inst = TirInst{ .block = .{ .start = @intCast(t.instructions.len), .end = undefined } };
    try t.air_tir_inst_map.put(@enumFromInt(air_blk_index), tir_blk_index);
    print("Block begins at {}\n", .{tir_inst.block.start});

    const air_blk = t.air.instructions.get(air_blk_index);

    print("\nGENERATING BLOCK FROM AIR {d} TO {d}\n", .{ @intFromEnum(air_blk.block.start), @intFromEnum(air_blk.block.end) });

    tir_inst.block.end = try tir_gen_bb(t, @intFromEnum(air_blk.block.start) + 1);

    print("Block ends at {}\n", .{tir_inst.block.end});

    t.instructions.set(@intFromEnum(tir_blk_index), tir_inst);
    return @intFromEnum(tir_blk_index);
}

fn tir_gen_bb(t: *TirState, air_bb_start: AirInst.Index) TirError!TirInst.Index {
    var air_index: u32 = air_bb_start;
    const air_instructions = t.air.instructions.slice();
    print("Generating instructions from {}\n", .{air_bb_start});
    while (air_index < t.air.instructions.len) : (air_index += 1) {
        print("\nGenerating TIR for AIR instruction {}\n", .{air_index});
        try air_mod.print_air(t.air, air_index, air_index + 1, 0);
        // print("TIR so far:", .{});
        // try print_tir(t, 0, @intCast(t.instructions.len), 0);

        const air_inst = air_instructions.get(air_index);
        switch (air_inst) {
            .block => |block| {
                _ = block;
                _ = try tir_gen_blk(t, air_index);
                return @intCast(t.instructions.len - 1);
                // return error.Unimplemented;
            },
            .fn_def => |fn_def_extra| {
                _ = fn_def_extra;
                // const fn_def = t.air.get_extra_struct(AirInst.FnDef, fn_def_extra);
                // _ = try tir_gen_blk(t, @intFromEnum(fn_def.blk));

                return error.Unimplemented;
                // _ = fn_def;
            },
            .br => |br| {
                const air_dst: AirInst.Index = @intFromEnum(br);

                if (t.air_tir_inst_map.get(@enumFromInt(air_dst))) |tir_dst| {

                    // const tir_mapped_inst = t.instructions.get(@intFromEnum(tir_dst));
                    // switch (tir_mapped_inst) {
                    //     .block => |blk| {

                    //     }
                    // }

                    const tir_br_index = try t.append_inst(.{ .br = @intFromEnum(tir_dst) });
                    try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_br_index);
                    return @intFromEnum(tir_br_index);
                } else {
                    const tir_br_index = try t.append_inst(undefined);
                    try t.air_tir_inst_map.put(
                        @enumFromInt(air_index),
                        tir_br_index,
                    );

                    _ = try tir_gen_blk(t, air_dst);

                    const br_dest = try t.get_inst_mapping(br);
                    t.instructions.set(@intFromEnum(tir_br_index), .{ .br = @intFromEnum(br_dest) });
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
            //         if (cond_typ != .tir_boolean) {
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
                const br = t.air.get_extra_struct(AirInst.BrEither, br_either);
                const tir_cond = try t.get_inst_mapping(br.cond);
                const tir_cond_val = try get_val(t, tir_cond);

                const then_blk = t.air.instructions.get(@intFromEnum(br.then_blk));
                const else_blk = t.air.instructions.get(@intFromEnum(br.else_blk));
                if (tir_cond_val) |val| {
                    print("Inling br either \n", .{});
                    switch (val) {
                        .boolean => |boolean| {
                            if (boolean) {
                                return tir_gen_bb(t, @intFromEnum(then_blk.block.start) + 1);
                                // tir_blk_index = try tir_gen_blk(t, @intFromEnum(br.then_blk));
                            } else {
                                return tir_gen_bb(t, @intFromEnum(else_blk.block.start) + 1);
                                // tir_blk_index = try tir_gen_blk(t, @intFromEnum(br.else_blk));
                            }
                            // const tir_blk_end = t.instructions.get(tir_blk_index).block.end;
                        },
                        else => return error.ExpectedBoolean,
                    }
                } else {
                    print("Inling br either not possible\n", .{});
                    // Compile-time evaluation not possible.
                    const cond_typ = try get_tir_inst_ret_type(t, tir_cond);
                    if (cond_typ != .tir_boolean) {
                        return error.ExpectedBoolean;
                    }
                    var tir_br_inst = TirInst{ .br_either = .{ .cond = @intFromEnum(tir_cond), .then_blk = undefined, .else_blk = undefined } };
                    const inst_index = try t.append_inst(tir_br_inst);

                    t.instructions.set(@intFromEnum(inst_index), tir_br_inst);
                    try t.air_tir_inst_map.put(@enumFromInt(air_index), inst_index);
                    tir_br_inst.br_either.then_blk = try tir_gen_blk(t, @intFromEnum(br.then_blk));
                    tir_br_inst.br_either.else_blk = try tir_gen_blk(t, @intFromEnum(br.else_blk));
                    return @intFromEnum(inst_index);
                }
            },
            .lt => |lt| {
                var lhs_tir_ref: TirInst.IndexRef = t.air_tir_inst_map.get(lt.lhs) orelse return error.MissingMapping;
                const lhs_val = try get_val(t, lhs_tir_ref);
                var rhs_tir_ref: TirInst.IndexRef = t.air_tir_inst_map.get(lt.rhs) orelse return error.MissingMapping;
                var lhs_typ = try get_tir_inst_ret_type(t, lhs_tir_ref);
                var rhs_typ = try get_tir_inst_ret_type(t, rhs_tir_ref);

                const rhs_val = try get_val(t, rhs_tir_ref);
                if (lhs_val) |l_val| {
                    if (rhs_val) |r_val| {
                        var c_l_val = l_val;
                        var c_r_val = r_val;
                        if (l_val == .unknown_int and r_val != .unknown_int) {
                            c_l_val = try coerce_unknown_int(l_val.unknown_int, rhs_typ);
                        } else if (l_val != .unknown_int and r_val == .unknown_int) {
                            c_r_val = try coerce_unknown_int(r_val.unknown_int, lhs_typ);
                        }

                        if (c_l_val == .unknown_int and c_r_val == .unknown_int) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.unknown_int < c_r_val.unknown_int }, air_index);
                        } else if (c_l_val == .i8 and c_r_val == .i8) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.i8 < c_r_val.i8 }, air_index);
                        } else if (c_l_val == .i16 and c_r_val == .i16) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.i16 < c_r_val.i16 }, air_index);
                        } else if (c_l_val == .i32 and c_r_val == .i32) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.i32 < c_r_val.i32 }, air_index);
                        } else if (c_l_val == .i64 and c_r_val == .i64) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.i64 < c_r_val.i64 }, air_index);
                        } else if (c_l_val == .u8 and c_r_val == .u8) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.u8 < c_r_val.u8 }, air_index);
                        } else if (c_l_val == .u16 and c_r_val == .u16) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.u16 < c_r_val.u16 }, air_index);
                        } else if (c_l_val == .u32 and c_r_val == .u32) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.u32 < c_r_val.u32 }, air_index);
                        } else if (c_l_val == .u64 and c_r_val == .u64) {
                            try t.append_constant_val(Value{ .boolean = c_l_val.u64 < c_r_val.u64 }, air_index);
                        } else {
                            return error.Unimplemented;
                        }
                        continue;
                    }
                }

                // Compile-time evaluation not possible.
                if (lhs_val) |l_val| {
                    if (l_val == .unknown_int) {
                        const coerced_val = try coerce_unknown_int(l_val.unknown_int, rhs_typ);
                        const val_index = try t.append_val(coerced_val);
                        lhs_tir_ref = try t.append_inst(.{ .constant_val = val_index });
                        lhs_typ = rhs_typ;
                    }
                }
                if (rhs_val) |r_val| {
                    if (r_val == .unknown_int) {
                        const coerced_val = try coerce_unknown_int(r_val.unknown_int, lhs_typ);
                        const val_index = try t.append_val(coerced_val);
                        rhs_tir_ref = try t.append_inst(.{ .constant_val = val_index });
                        rhs_typ = lhs_typ;
                    }
                }

                var res_inst: TirInst.IndexRef = undefined;
                if (lhs_typ == .tir_i8 and rhs_typ == .tir_i8) {
                    res_inst = try t.append_inst(TirInst{ .lt_i8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_i16 and rhs_typ == .tir_i16) {
                    res_inst = try t.append_inst(TirInst{ .lt_i16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_i32 and rhs_typ == .tir_i32) {
                    res_inst = try t.append_inst(TirInst{ .lt_i32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_i64 and rhs_typ == .tir_i64) {
                    res_inst = try t.append_inst(TirInst{ .lt_i64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u8 and rhs_typ == .tir_u8) {
                    res_inst = try t.append_inst(TirInst{ .lt_u8 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u16 and rhs_typ == .tir_u16) {
                    res_inst = try t.append_inst(TirInst{ .lt_u16 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u32 and rhs_typ == .tir_u32) {
                    res_inst = try t.append_inst(TirInst{ .lt_u32 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else if (lhs_typ == .tir_u64 and rhs_typ == .tir_u64) {
                    res_inst = try t.append_inst(TirInst{ .lt_u64 = .{ .lhs = lhs_tir_ref, .rhs = rhs_tir_ref } });
                } else {
                    return error.ComparisionTypeError;
                }
                try t.air_tir_inst_map.put(@enumFromInt(air_index), res_inst);
            },

            .add => |air_add| {
                const lhs_tir_ref: TirInst.IndexRef = t.air_tir_inst_map.get(air_add.lhs) orelse return error.MissingMapping;
                const lhs_val = try get_val(t, lhs_tir_ref);
                const rhs_tir_ref: TirInst.IndexRef = t.air_tir_inst_map.get(air_add.rhs) orelse return error.MissingMapping;
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
                    print("Addition types are mismatched {} and {}\n", .{ lhs_typ, rhs_typ });
                    print("TIR Lhs is from{} and rhs is from {}\n", .{ lhs_tir_ref, rhs_tir_ref });
                    print("AIR Lhs is from{} and rhs is from {}\n", .{ air_add.lhs, air_add.rhs });
                    return error.AdditionTypeError;
                }
                try t.air_tir_inst_map.put(@enumFromInt(air_index), res_inst);
            },
            .type_of => |air_type_inst| {
                const tir_expr_inst = try t.get_inst_mapping(air_type_inst);
                const tir_expr_type = try get_tir_inst_ret_type(t, tir_expr_inst);
                try t.air_tir_type_map.put(@enumFromInt(air_index), tir_expr_type);
            },
            .type_as => |type_as| {
                const tir_expr_ref = try t.get_inst_mapping(type_as.expr);
                const tir_type_ref = try t.get_type_mapping(type_as.type);
                const maybe_val = try get_val(t, tir_expr_ref);
                if (maybe_val) |val| {
                    switch (tir_type_ref) {
                        .tir_i8,
                        .tir_i16,
                        .tir_i32,
                        .tir_i64,
                        .tir_u8,
                        .tir_u16,
                        .tir_u32,
                        .tir_u64,
                        => {
                            if (val == .unknown_int) {
                                const coerced_val = try coerce_unknown_int(val.unknown_int, tir_type_ref);
                                try t.append_constant_val(coerced_val, air_index);
                            }
                        },
                        .tir_unknown_int => {
                            if (val == .unknown_int) {
                                try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_expr_ref);
                            }
                        },
                        else => {
                            print("Type as for {} not impl.\n", .{tir_type_ref});
                            return error.Unimplemented;
                        },
                    }
                } else {
                    switch (tir_type_ref) {
                        .tir_unknown_int => return error.CannotCoerceKnownToUnknown,
                        else => return error.Unimplemented,
                    }
                }
            },
            .alloc => |air_alloc| {
                const tir_alloc_type = try t.get_type_mapping(air_alloc.type);
                const tir_alloca = try t.append_inst(.{ .alloca = tir_alloc_type });
                try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_alloca);
            },
            .load => |air_load| {
                const tir_ptr = try t.get_inst_mapping(air_load.ptr);
                const tir_type = try t.get_type_mapping(air_load.type);

                const tir_load = try t.append_inst(TirInst{ .load = .{ .ptr = tir_ptr, .type = tir_type } });
                try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_load);
            },
            .store => |air_store| {
                const tir_val_inst = try t.get_inst_mapping(air_store.val);
                const tir_val_type = try get_tir_inst_ret_type(t, tir_val_inst);

                const tir_ptr = try t.get_inst_mapping(air_store.ptr);
                const tir_ptr_type = t.instructions.get(@intFromEnum(tir_ptr)).alloca;
                if (tir_val_type != tir_ptr_type) {
                    return error.StoringWrongType;
                }

                const tir_store = try t.append_inst(TirInst{ .store = .{ .val = tir_val_inst, .val_type = tir_val_type, .ptr = tir_ptr } });
                try t.air_tir_inst_map.put(@enumFromInt(air_index), tir_store);
            },
            .arg => |air_arg| {
                const tir_type_ref = try t.get_type_mapping(air_arg.type);
                const tir_arg = TirInst{ .arg = .{ .name = air_arg.name, .typ_ref = tir_type_ref } };
                const inst_index = try t.append_inst(tir_arg);
                try t.air_tir_inst_map.put(@enumFromInt(air_index), inst_index);
            },
            .int => |int| {
                const val = Value{ .unknown_int = @intCast(int) };
                try t.append_constant_val(val, air_index);
            },
            else => return error.Unimplemented,
        }
    }
    return @intCast(t.instructions.len - 1);
}

pub fn tir_gen(air: *Air, allocator: Allocator) !void {
    var tir = TirState{
        .instructions = TirInst.List{},
        .extra = std.ArrayList(TirInst.IndexRef).init(allocator),
        .scratch = std.ArrayList(u32).init(allocator),

        .air = air,
        .air_tir_inst_map = TirState.AirTirInstMap.init(allocator),
        .air_tir_type_map = TirState.AirTirTypeMap.init(allocator),

        .values = Value.List{},
        .types = Type.List{},

        .allocator = allocator,
    };
    defer tir.deinit();
    try tir.air_tir_inst_map.put(AirInst.IndexRef.true_lit, TirInst.IndexRef.tir_true_lit);
    try tir.air_tir_inst_map.put(AirInst.IndexRef.false_lit, TirInst.IndexRef.tir_false_lit);

    try tir.air_tir_type_map.put(AirInst.IndexRef.bool, Type.IndexRef.tir_boolean);
    try tir.air_tir_type_map.put(AirInst.IndexRef.u8, Type.IndexRef.tir_u8);
    try tir.air_tir_type_map.put(AirInst.IndexRef.u16, Type.IndexRef.tir_u16);
    try tir.air_tir_type_map.put(AirInst.IndexRef.u32, Type.IndexRef.tir_u32);
    try tir.air_tir_type_map.put(AirInst.IndexRef.u64, Type.IndexRef.tir_u64);
    try tir.air_tir_type_map.put(AirInst.IndexRef.i8, Type.IndexRef.tir_i8);
    try tir.air_tir_type_map.put(AirInst.IndexRef.i16, Type.IndexRef.tir_i16);
    try tir.air_tir_type_map.put(AirInst.IndexRef.i32, Type.IndexRef.tir_i32);
    try tir.air_tir_type_map.put(AirInst.IndexRef.i64, Type.IndexRef.tir_i64);

    const topmost_air = tir.air.instructions.get(0);
    std.debug.assert(topmost_air == .block);

    // _ = try tir_gen_blk(&tir, 0);
    const end = try tir_gen_bb(&tir, 0);
    try print_tir(&tir, 0, end, 0);
}