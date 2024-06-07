// Module responsible for performing the main memory-safety analysis of the compiler
const std = @import("std");
const print = std.debug.print;

const tir_mod = @import("tir.zig");
const Type = tir_mod.Type;
const Tir = tir_mod.Tir;
const TirInst = tir_mod.TirInst;

pub const SafetyRes = union(enum) { mem_leak: TirInst.Index, safe };

const LinearType = enum {
    val,
    own,
};

pub fn check_safety(t: *const Tir) !SafetyRes {
    const instructions = t.instructions.slice();
    for (0..instructions.len) |inst_index| {
        const inst = instructions.get(inst_index);
        print("{}\n", .{inst});
    }

    return .safe;
}
