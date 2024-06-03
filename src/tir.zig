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
    const Field = struct { var_name: AirState.StringIndex, field_type: Type.Index };

    // TODO: Maybe remove all primitives which are already in TirInst.IndexRef?
    boolean,
    int_u64,
    int_u32,
    int_u16,
    int_u8,
    int_i64,
    int_i32,
    int_i16,
    int_i8,
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
    const IndexRef = enum(Index) {
        boolean = 4294967040,
        int_u64,
        int_u32,
        int_u16,
        int_u8,
        int_i64,
        int_i32,
        int_i16,
        int_i8,
    };
    const List = std.MultiArrayList(TirInst);

    constant_val: Value,
    constant_type: Type,
    add: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
};

//

const TirState = struct {
    const AirTirMap = std.AutoHashMap(AirInst.IndexRef, TirInst.IndexRef);

    instructions: TirInst.List,
    extra: std.ArrayList(TirInst.IndexRef),
    scratch: std.ArrayList(u32),

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
};

pub fn tir_gen(air: Air, allocator: Allocator) !void {
    var tir = TirState{
        .instructions = TirInst.List{},
        .extra = std.ArrayList(TirInst.IndexRef).init(allocator),
        .scratch = std.ArrayList(u32).init(allocator),

        .air_tir_map = TirState.AirTirMap.init(allocator),

        .values = Value.List{},
        .types = Type.List{},

        .allocator = allocator,
    };

    try tir.air_tir_map.put(AirInst.IndexRef.bool, TirInst.IndexRef.boolean);
    try tir.air_tir_map.put(AirInst.IndexRef.u8, TirInst.IndexRef.int_u8);
    try tir.air_tir_map.put(AirInst.IndexRef.u16, TirInst.IndexRef.int_u16);
    try tir.air_tir_map.put(AirInst.IndexRef.u32, TirInst.IndexRef.int_u32);
    try tir.air_tir_map.put(AirInst.IndexRef.u64, TirInst.IndexRef.int_u64);

    var air_index: u32 = 0;
    const air_instructions = air.instructions.slice();
    while (air_index < air.instructions.len) {
        const air_inst = air_instructions.get(air_index);
        switch (air_inst) {
            // .add => |add| {
            //     const lhs_tir_ref: TirInst.IndexRef = try tir.air_tir_map.get(add.lhs);
            //     switch (lhs_tir_ref) {
            //         .boolean |
            //             .int_u64 |
            //             .int_u32 |
            //             .int_u16 |
            //             .int_u8 |
            //             .int_i64 |
            //             .int_i32 |
            //             .int_i16 |
            //             .int_i8 =>
            //             return error.AddOnType,
            //         _ => {
            //             const lhs_tir_index : TirInst.Index = @intFromEnum(lhs_tir_ref);
            //             const lhs_tir_inst = tir.instructions.get(lhs_tir_index);

            //         }
            //     }
            //     // if (add.)
            // },
            .int => |int| {
                const tir_inst = TirInst{ .constant_val = Value{ .unknown_int = @intCast(int) } };
                const tir_index = try tir.append_inst(tir_inst);
                try tir.air_tir_map.put(@enumFromInt(air_index), tir_index);
            },
            else => return error.Unimplemented,
        }
        air_index += 1;
    }
}
