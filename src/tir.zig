const std = @import("std");
const print = std.debug.print;

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

const Type = union(enum) {
    const Index = u32;
    typ,
    boolean,
    int_u64,
    int_u32,
    strct: []Type,
    array: *Type,
};

// Typed intermediate representation
const TirInst = union(enum) {
    const Index = u32;

    constant: struct {
        typ: Type,
        val: Value.Index,
    },
    add: struct {
        lhs: Index,
        rhs: Index,
    },
};

const Value = union(enum) {
    const Index = u32;
    int_u64: u64,
    boolean: bool,
    typ: Type,
};

const TirState = struct {
    instructions: std.MultiArrayList(TirInst),

    // compile-time known values
    values: std.ArrayList(Value),
};
