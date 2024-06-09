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

// const FieldId = u32;
// const FieldTerminator = 0xFF;

// fn fields_to_fid(fields: []u8) FieldId {
//     var fid: FieldId = 0xFF_FF_FF_FF;
//     var fid_arr: []u8 = &fid;
//     for (fields, 0..) |field, index| {
//         fid_arr[index] = field;
//     }
// }
const DerefField = 0xFE;
const FieldsInit = .{ 0xFF, 0xFF, 0xFF, 0xFF };

const FieldId = struct {
    field_end: u8 = 0,
    fields: [4]u8 = FieldsInit,
    inst: TirInst.Index,
};

const LinearState = struct {
    // Keys denote instructions which own an allocation.
    // Values denote the allocation instruction which they own.
    owners: std.AutoHashMap(FieldId, TirInst.Index),

    fn new_alloc(ls: *LinearState, inst: TirInst.Index) !void {
        print("Adding owner {}\n", .{inst});
        const field = FieldId{ .inst = inst };
        _ = try ls.owners.putNoClobber(field, inst);
    }

    fn move_owner(ls: *LinearState, from: FieldId, to: FieldId) !void {
        print("Moving owner from {} to {}\n", .{ from, to });
        const alloc_inst = ls.owners.get(from) orelse return error.MovingNonOwner;
        _ = ls.owners.remove(from);
        _ = try ls.owners.putNoClobber(to, alloc_inst);
        // _ = try ls.owners.putNoClobber(inst, inst);
    }

    fn delete_alloc(ls: *LinearState, field: FieldId) !void {
        print("Removing owner {}\n", .{field});
        const res = ls.owners.remove(field);
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

fn move_ownership_all_fields(t: *const Tir, ls: *LinearState, from_field: FieldId, to_field: FieldId, cur_type: Type.IndexRef) !void {
    switch (cur_type) {
        .tir_boolean, .tir_unknown_int, .tir_u64, .tir_u32, .tir_u16, .tir_u8, .tir_i64, .tir_i32, .tir_i16, .tir_i8, .tir_void, .tir_typ, .tir_own, .tir_ref, .tir_stackref => return,
        _ => {
            const typ = t.types.get(@intFromEnum(cur_type));
            print("typ {}\n", .{typ});
            switch (typ) {
                .ptr => {
                    if (typ.ptr.cap == .tir_own) {
                        try ls.move_owner(from_field, to_field);
                        // if (add) {
                        //     return;
                        //     // try ls.new_alloc(FieldId{ .inst = inst, .fields = cur_fields });
                        // } else {
                        //     // try ls.move_owner(, )
                        //     // try ls.delete_alloc(FieldId{ .inst = inst, .fields = cur_fields });
                        // }
                        // return;
                    } else if (typ.ptr.cap == .tir_stackref) {
                        try move_ownership_all_fields(t, ls, from_field, to_field, typ.ptr.deref_type);
                    }
                },
                .tir_struct => |tir_struct| {
                    var cur_field_index = tir_struct.fields_start;
                    while (cur_field_index < tir_struct.fields_end) : (cur_field_index += 1) {
                        const cur_field = t.types.get(cur_field_index);
                        switch (cur_field) {
                            .tir_struct_field, .tir_enum_field, .tir_mut_struct_field => |field_info| {
                                var new_from_field = from_field;
                                new_from_field.fields[new_from_field.field_end] = @intCast(cur_field_index - tir_struct.fields_start);
                                new_from_field.field_end += 1;

                                var new_to_field = to_field;
                                new_to_field.fields[new_to_field.field_end] = @intCast(cur_field_index - tir_struct.fields_start);
                                new_to_field.field_end += 1;
                                try move_ownership_all_fields(t, ls, new_from_field, new_to_field, field_info.field_type);
                            },
                            else => unreachable,
                        }
                    }
                },
                .tir_struct_field, .tir_enum_field, .tir_mut_struct_field => unreachable,
                .tir_array => return error.Unimplemented,
                .tir_enum => return error.Unimplemented,
            }
        },
    }
}

fn analyse_bb(t: *const Tir, ls: *LinearState, bb_start: TirInst.Index) !void {
    const instructions = t.instructions.slice();
    const end: TirInst.Index = @intCast(t.instructions.len);
    for (bb_start..end) |inst_index| {
        const index: u32 = @intCast(inst_index);
        const inst = instructions.get(inst_index);
        print("{} : ", .{inst_index});
        switch (inst) {
            .memalloc => |memalloc| {
                var to_field = FieldId{ .fields = FieldsInit, .field_end = 1, .inst = index };
                to_field.fields[0] = DerefField;

                const from_field = FieldId{ .inst = @intFromEnum(memalloc.expr) };
                const expr_type = t.types.get(@intFromEnum(memalloc.ptr_type)).ptr.deref_type;
                try move_ownership_all_fields(t, ls, from_field, to_field, expr_type);

                try ls.new_alloc(index);
            },
            .memfree => |memfree| {
                // TODO: START HERE
                // var to_field = FieldId{ .fields = FieldsInit, .field_end = 1, .inst = index };

                // const from_field = FieldId{ .inst = @intFromEnum(memfree.ptr) };
                // const expr_type = t.types.get(@intFromEnum(memfree.ptr_type)).ptr.deref_type;
                // try move_ownership_all_fields(t, ls, from_field, to_field, expr_type);

                try ls.delete_alloc(FieldId{ .inst = @intFromEnum(memfree.ptr) });
                // const cur_fields = FieldsInit;
                // try amend_ownership_all_fields(t, ls, index, memfree.expr_type, cur_fields, 0, true);
            },
            .move => |move| {
                try ls.move_owner(FieldId{ .inst = move }, FieldId{ .inst = index });
            },
            .store => |store| {
                if (type_is_ref(store.val_type)) {
                    const storing_type = t.types.get(@intFromEnum(store.val_type));
                    if (storing_type == .ptr and storing_type.ptr.cap == .tir_own) {
                        // Store is storing a value which is an owning pointer.
                        const from = FieldId{ .inst = @intFromEnum(store.val) };

                        // First, we determine the destination of the store by
                        // inspection the instruction from which the dest. ptr came.
                        // Note that we only have 3 possible options here:
                        // alloca (local stack var).
                        // getelementptr (var in local struct/enum)
                        // memfree (var in local struct/enum)

                        const ptr_inst = t.instructions.get(@intFromEnum(store.ptr));
                        switch (ptr_inst) {
                            .get_element_ptr => |gep| {
                                var to = FieldId{ .inst = @intFromEnum(gep.aggregate_ptr) };

                                const field_indeces = t.extra.items[gep.indeces_start..gep.indeces_end];
                                for (field_indeces) |f| {
                                    to.fields[to.field_end] = @intCast(f);
                                    to.field_end += 1;
                                }
                                try ls.move_owner(from, to);
                                continue;
                            },
                            .memfree => |_| {
                                const to = FieldId{ .inst = @intFromEnum(store.ptr) };
                                try ls.move_owner(from, to);
                                continue;
                            },
                            .alloca => {
                                const to = FieldId{ .inst = @intFromEnum(store.ptr) };
                                try ls.move_owner(from, to);
                                continue;
                            },
                            else => continue,
                        }
                    }
                }
                print("\n", .{});
            },
            .load => |load| {
                if (type_is_ref(load.type)) {
                    const load_type = t.types.get(@intFromEnum(load.type));

                    if (load_type == .ptr and load_type.ptr.cap == .tir_own) {
                        const to = FieldId{ .inst = index };

                        const ptr_inst = t.instructions.get(@intFromEnum(load.ptr));
                        switch (ptr_inst) {
                            .get_element_ptr => |gep| {
                                var from = FieldId{ .inst = @intFromEnum(gep.aggregate_ptr) };

                                const slice = t.extra.items[gep.indeces_start..gep.indeces_end];
                                for (slice) |f_index| {
                                    from.fields[from.field_end] = @intCast(f_index);
                                    from.field_end += 1;
                                }
                                try ls.move_owner(from, to);
                                continue;
                            },
                            .alloca => {
                                const from = FieldId{ .inst = @intFromEnum(load.ptr) };
                                try ls.move_owner(from, to);
                                continue;
                            },
                            else => continue,
                        }

                        // const from = FieldId{ .inst = @intFromEnum(load.ptr) };
                        // try ls.move_owner(from, to);
                        // continue;
                    }
                }
                print("\n", .{});
            },
            else => {
                print("Ignoring\n", .{});
            },
        }
    }
}

fn analyse_fn(t: *const Tir, fn_def: TirInst.FnDef) !SafetyRes {
    // TODO: Analyse args.
    var ls = LinearState{
        .owners = std.AutoHashMap(FieldId, TirInst.Index).init(t.allocator),
    };
    defer ls.owners.deinit();
    try analyse_bb(t, &ls, fn_def.blk);
    if (ls.owners.count() > 0) {
        print("Memory leak detected.\n[", .{});
        var it = ls.owners.keyIterator();
        while (it.next()) |key| {
            print("{} allocation at  {any}\n", .{ key, ls.owners.get(key.*) });
        }
        print("]\n", .{});
        return error.MemoryLeak;
    }
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
