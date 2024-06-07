// Module responsible for performing the main memory-safety analysis of the compiler
const std = @import("std");
const print = std.debug.print;
const set = @import("ziglangSet");

const tir_mod = @import("tir.zig");
const Type = tir_mod.Type;
const Tir = tir_mod.Tir;
const TirInst = tir_mod.TirInst;
const type_is_ref = tir_mod.type_is_ref;

pub const SafetyRes = union(enum) { mem_leak: TirInst.Index, safe };

const LinearType = enum {
    val,
    own,
};

const LinearState = struct {
    // Keys denote instructions which own an allocation.
    // Values denote the allocation instruction which they own.
    owners: std.AutoHashMap(TirInst.Index, TirInst.Index),

    fn new_alloc(ls: *LinearState, inst: TirInst.Index) !void {
        print("Adding owner {}\n", .{inst});
        _ = try ls.owners.putNoClobber(inst, inst);
    }

    fn move_owner(ls: *LinearState, from: TirInst.Index, to: TirInst.Index) !void {
        print("Moving owner from {} to {}\n", .{ from, to });
        const alloc_inst = ls.owners.get(from) orelse return error.MovingNonOwner;
        _ = ls.owners.remove(from);
        _ = try ls.owners.putNoClobber(to, alloc_inst);
        // _ = try ls.owners.putNoClobber(inst, inst);
    }

    fn delete_alloc(ls: *LinearState, inst: TirInst.Index) !void {
        print("Removing owner {}\n", .{inst});
        const res = ls.owners.remove(inst);
        if (res == false) {
            return error.RemovingNonOwner;
        }
    }
    // fn remove_owner(ls : *LinearState, inst : TirInst.Index) !void {
    //     const already_owner = try ls.owning_insts.remove(inst)
    //     if (already_owner) {
    //         return error.AlreadyOwner;
    //     }
    // }
};

fn analyse_bb(t: *const Tir, ls: *LinearState, bb_start: TirInst.Index) !void {
    const instructions = t.instructions.slice();
    const end: TirInst.Index = @intCast(t.instructions.len);
    for (bb_start..end) |inst_index| {
        const index: u32 = @intCast(inst_index);
        const inst = instructions.get(inst_index);
        print("{} : ", .{inst_index});
        switch (inst) {
            .memalloc => {
                // TODO: MOVE CONTENTS OF EXPR HERE
                try ls.new_alloc(index);
            },
            .move => |move| {
                try ls.move_owner(move, index);
            },
            .store => |store| {
                if (type_is_ref(store.val_type)) {
                    const storing_type = t.types.get(@intFromEnum(store.val_type));
                    if (storing_type == .ptr and storing_type.ptr.cap == .tir_own) {
                        try ls.move_owner(@intFromEnum(store.val), @intFromEnum(store.ptr));
                        continue;
                    }
                }
                print("\n", .{});
            },
            .load => |load| {
                if (type_is_ref(load.type)) {
                    const load_type = t.types.get(@intFromEnum(load.type));

                    const next_inst = t.instructions.get(index + 1);
                    if (load_type == .ptr and load_type.ptr.cap == .tir_own and next_inst != .cap_reduce) {
                        try ls.move_owner(@intFromEnum(load.ptr), index);
                        continue;
                    }
                }
                print("\n", .{});
            },
            .memfree => |memfree| {
                try ls.delete_alloc(@intFromEnum(memfree.ptr));
            },
            else => {
                print("Ignoring\n", .{});
            },
        }
    }
}

fn analyse_fn(t: *const Tir, fn_def: TirInst.FnDef) !SafetyRes {
    // TODO: Analyse args.
    var state = LinearState{
        .owners = std.AutoHashMap(TirInst.Index, TirInst.Index).init(t.allocator),
    };
    defer state.owners.deinit();
    try analyse_bb(t, &state, fn_def.blk);
    // state.owning_insts.clone();
    return .safe;
}

// At every program point, determine for each pointer variable
// whether it is
// invalid (may point to free'd memory)
// valid, and if so, which allocations it may point to.
// Allocations are here uniquely identified by their program location l.
// Then, whenever a pointer is dereferenced, we may check if it is still valid.

// To this end, we first need to know when each allocation becomes invalid.
// In other words, the "range" of an allocation (denoting the range of program
// points for which it is valid) is required.
// Every allocation must have a closed range i.e. not leak.

pub fn check_safety(t: *const Tir) !SafetyRes {

    // Create a set of u32s called A
    // var A = set.Set(u32).init(t.allocator);
    // defer A.deinit();

    // // Add some data
    // _ = try A.add(5);
    // _ = try A.add(6);
    // _ = try A.add(7);

    const instructions = t.instructions.slice();
    for (0..instructions.len) |inst_index| {
        const inst = instructions.get(inst_index);
        // print("{}\n", .{inst});
        switch (inst) {
            .fn_def => |fn_def| {
                // TODO: Analyse args.
                _ = try analyse_fn(t, fn_def);
            },
            else => {
                // print("Ignoring\n", .{});
            },
        }
    }

    return .safe;
}
