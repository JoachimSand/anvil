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

// Represents a compile-time known type
const Type = union(enum) {
    const Index = u32;
    boolean,
    int_u64,
    int_u32,
    int_u16,
    int_u8,
    int_i64,
    int_i32,
    int_i16,
    int_i8,
    type_struct: []Type,
    // strct: []Type,
    // array: *Type,
};

//Compile-time known values.
const Value = union(enum) {
    const Index = u32;
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

    constant_val: struct {
        val: Value,
    },
    constant_type: struct {
        ty: Type,
    },
    add: struct {
        lhs: Index,
        rhs: Index,
    },
};

//

const TirState = struct {
    instructions: std.MultiArrayList(TirInst),

    // compile-time known values
    values: std.ArrayList(Value),
};
