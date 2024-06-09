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

const MemoryNode = union(enum) {
    const Index = u32;
    const RefStart = 4294967040;
    const IndexRef = enum(Index) { val = RefStart, uninit, _ };
    const List = std.ArrayList(MemoryNode);
    const FieldArr = [4]MemoryNode.IndexRef;
    const FieldArrUninit = .{ .uninit, .uninit, .uninit, .uninit };

    const Aggregrate = struct {
        // memalloc/alloc inst that created this node.
        // Used as a sort of UID for memnodes.
        alloc_inst: TirInst.Index,
        num_fields: u8 = 0,
        fields: FieldArr = FieldArrUninit,
    };

    heap_aggregrate: Aggregrate,
    stack_aggregrate: Aggregrate,
    owning_ptr: struct {
        // alloc_inst: TirInst.Index,
        dst: MemoryNode.IndexRef,
    },
    unowned_ptr,
};

const LinearState = struct {
    mem_nodes: std.ArrayList(MemoryNode),
    inst_states: std.AutoHashMap(TirInst.Index, MemoryNode.IndexRef),

    fn append_mem_node(ls: *LinearState, node: MemoryNode) !MemoryNode.Index {
        const index: MemoryNode.Index = @intCast(ls.mem_nodes.items.len);
        try ls.mem_nodes.append(node);
        return index;
    }

    fn put_own_node(ls: *LinearState, inst: TirInst.Index, mem_ref: MemoryNode.IndexRef) !void {
        if (ls.inst_states.get(inst)) |to_res| {
            const to_index: MemoryNode.Index = @intFromEnum(to_res);
            if (to_index < MemoryNode.RefStart and ls.mem_nodes.items[to_index] == .unowned_ptr) {
                // TODO: Remove unowned node?
                _ = ls.inst_states.remove(inst);
            } else {
                print("Error: moving ownership to node with existing ref {}\n", .{to_res});
                return error.MovingToOwner;
            }
        }
        _ = try ls.inst_states.putNoClobber(inst, mem_ref);
    }

    fn move_mem_node(ls: *LinearState, from: TirInst.Index, to: TirInst.Index) !void {
        print("Moving owner from {} to {}\n", .{ from, to });
        const mem_node = ls.inst_states.get(from) orelse return error.MovingNonOwner;
        print("Mem node moved is {}\n", .{mem_node});
        _ = ls.inst_states.remove(from);
        try put_own_node(ls, to, mem_node);
    }

    fn print_mem_nodes(ls: *LinearState) void {
        print("---- Mem nodes: ----\n", .{});
        for (ls.mem_nodes.items) |node| {
            print("{}\n", .{node});
        }
        print("---- Inst state: ----\n", .{});
        var key_it = ls.inst_states.keyIterator();
        while (key_it.next()) |key| {
            print("{d} -> {any}\n", .{ key.*, ls.inst_states.get(key.*) });
        }
        print("----            ----\n", .{});
    }
    // fn cre
};

fn init_mem_node(t: *const Tir, ls: *LinearState, type_ref: Type.IndexRef, inst: TirInst.Index, heap: bool) !MemoryNode.IndexRef {
    switch (type_ref) {
        .tir_boolean, .tir_unknown_int, .tir_u64, .tir_u32, .tir_u16, .tir_u8, .tir_i64, .tir_i32, .tir_i16, .tir_i8, .tir_void, .tir_typ, .tir_own, .tir_ref, .tir_stackref => {
            return .val;
        },
        _ => {
            const typ = t.types.get(@intFromEnum(type_ref));
            switch (typ) {
                // TODO: Distinguish by cap?
                .ptr => |ptr| {
                    // return .not_owned;
                    if (ptr.cap == .tir_own) {
                        const mem_node = .unowned_ptr;
                        return @enumFromInt(try ls.append_mem_node(mem_node));
                    } else {
                        return .val;
                    }
                },
                .tir_struct => |container_fields| {
                    var type_index = container_fields.fields_start;
                    var fields: MemoryNode.FieldArr = MemoryNode.FieldArrUninit;
                    while (type_index < container_fields.fields_end) : (type_index += 1) {
                        const field_index: u32 = @intCast(type_index - container_fields.fields_start);
                        const cur_field = t.types.get(type_index);
                        switch (cur_field) {
                            .tir_struct_field, .tir_enum_field, .tir_mut_struct_field => |field_info| {
                                fields[field_index] = try init_mem_node(t, ls, field_info.field_type, inst, heap);
                            },
                            else => unreachable,
                        }
                    }
                    const num_fields: u8 = @intCast(container_fields.fields_end - container_fields.fields_start);
                    const mem_node = if (heap)
                        MemoryNode{ .heap_aggregrate = .{ .alloc_inst = inst, .fields = fields, .num_fields = num_fields } }
                    else
                        MemoryNode{ .stack_aggregrate = .{ .alloc_inst = inst, .fields = fields, .num_fields = num_fields } };

                    return @enumFromInt(try ls.append_mem_node(mem_node));
                },
                else => unreachable,
            }
        },
    }
}

// const FromToPair = struct {
//     from : MemoryNode.IndexRef,
//     to : MemoryNode.IndexRef,
// };

// fn transfer_ownership(t : *const Tir, ls: *LinearState, pair : FromToPair) !FromToPair {
//     const from = pair.from;
//     const to = pair.to;
//     switch (to) {
//         .val => return .val,
//         .not_owned => {
//             if (from == ._){
//                 return .{.from = .not_owned, .to = from};
//             } else {
//                 return error.MovingNonOwner;
//             }
//         },
//         .uninit => return error.Unimplemented,
//         _ => {
//             const to_node = ls.mem_nodes.items[to];
//             const from_node = ls.mem_nodes.items[from];
//             switch (to_node) {
//                 .
//             }
//         }
//     }
// }

fn change_to_heap(t: *const Tir, ls: *LinearState, mem_ref: MemoryNode.IndexRef) !MemoryNode.IndexRef {
    switch (mem_ref) {
        .val => return .val,
        .uninit => return .uninit,
        _ => {
            const mem_index: MemoryNode.Index = @intFromEnum(mem_ref);
            const old_mem_node = ls.mem_nodes.items[mem_index];
            switch (old_mem_node) {
                .heap_aggregrate => {
                    print("Got {}\n", .{old_mem_node});
                    return error.ExpectedStackAggregrates;
                },
                .stack_aggregrate => |aggregrate| {
                    var new_node = MemoryNode{ .heap_aggregrate = aggregrate };
                    for (0..aggregrate.num_fields) |f| {
                        new_node.heap_aggregrate.fields[f] = try change_to_heap(t, ls, new_node.heap_aggregrate.fields[f]);
                    }

                    ls.mem_nodes.items[mem_index] = new_node;
                    return mem_ref;
                },
                .owning_ptr, .unowned_ptr => return mem_ref,
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
        ls.print_mem_nodes();
        print("\nafter {} : ", .{inst_index});
        switch (inst) {
            .alloca => |alloca| {
                const mem_node = try init_mem_node(t, ls, alloca.alloc_type, index, false);
                try ls.inst_states.put(index, mem_node);
            },
            .memalloc => |memalloc| {
                // const deref_type = t.types.get(@intFromEnum(memalloc.ptr_type)).ptr.deref_type;
                // const mem_node = try init_mem_node(t, ls, deref_type, index, true);
                const expr_mem_ref = ls.inst_states.get(@intFromEnum(memalloc.expr)).?;
                _ = try change_to_heap(t, ls, expr_mem_ref);
                try ls.move_mem_node(@intFromEnum(memalloc.expr), index);
            },
            // .memfree => |memfree| {
            // },
            .move => |move| {
                try ls.move_mem_node(move, index);
            },
            .store => |store| {
                if (type_is_ref(store.val_type)) {
                    const storing_type = t.types.get(@intFromEnum(store.val_type));
                    const from_inst: TirInst.Index = @intFromEnum(store.val);

                    if (storing_type == .ptr and storing_type.ptr.cap == .tir_own) {
                        const ptr_inst = t.instructions.get(@intFromEnum(store.ptr));
                        switch (ptr_inst) {
                            .get_element_ptr => |gep| {
                                const to_inst: TirInst.Index = @intFromEnum(gep.aggregate_ptr);
                                const from_node_ref = ls.inst_states.get(from_inst).?;
                                var to_node_ref = ls.inst_states.get(to_inst).?;
                                print("GEP: to {} from {}\n", .{ to_node_ref, from_node_ref });

                                const slice = t.extra.items[gep.indeces_start..gep.indeces_end];
                                for (slice) |f_index| {
                                    // const from_node = ls.mem_nodes.items[@intFromEnum(from_node_ref)];
                                    const to_node = ls.mem_nodes.items[@intFromEnum(to_node_ref)];

                                    // switch (from_node) {
                                    //     .heap_aggregrate, .stack_aggregrate => |aggregrate| from_node_ref = aggregrate.fields[f_index],
                                    //     else => return error.Unimplemented,
                                    // }
                                    switch (to_node) {
                                        .heap_aggregrate, .stack_aggregrate => |aggregrate| to_node_ref = aggregrate.fields[f_index],
                                        else => return error.Unimplemented,
                                    }
                                    // from_inst.fields[from_inst.field_end] = @intCast(f_index);
                                    // from_inst.field_end += 1;
                                }
                                // const final_f_index = t.extra.items[gep.indeces_end - 1];
                                var to_node = ls.mem_nodes.items[@intFromEnum(to_node_ref)];

                                print("GEP: to {} from {}\n", .{ to_node_ref, from_node_ref });
                                // var new_to_node: MemoryNode.IndexRef = undefined;

                                switch (to_node) {
                                    .unowned_ptr => {
                                        to_node = MemoryNode{ .owning_ptr = .{ .dst = from_node_ref } };
                                    },
                                    else => return error.Unimplemented,
                                }

                                _ = ls.inst_states.remove(from_inst);
                                ls.mem_nodes.items[@intFromEnum(to_node_ref)] = to_node;
                                // try ls.put_own_node(to_nst, new_to_node);

                                // try ls.move_owner(from_inst, to);
                                continue;
                            },
                            .alloca => {
                                const to: TirInst.Index = @intFromEnum(store.ptr);
                                try ls.move_mem_node(from_inst, to);
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
                        const ptr_inst = t.instructions.get(@intFromEnum(load.ptr));
                        switch (ptr_inst) {
                            .get_element_ptr => return error.Unimplemented,
                            // .get_element_ptr => |gep| {
                            //     print("GEP LOAD \n", .{});
                            //     const to_inst: TirInst.Index = index;
                            //     const from_inst: TirInst.Index = @intFromEnum(gep.aggregate_ptr);
                            //     var from_node_ref = ls.inst_states.get(from_inst).?;

                            //     const slice = t.extra.items[gep.indeces_start .. gep.indeces_end - 1];
                            //     for (slice) |f_index| {
                            //         const from_node = ls.mem_nodes.items[@intFromEnum(from_node_ref)];

                            //         switch (from_node) {
                            //             .heap_aggregrate, .stack_aggregrate => |aggregrate| from_node_ref = aggregrate.fields[f_index],
                            //             else => return error.Unimplemented,
                            //         }
                            //     }
                            //     const final_f_index = t.extra.items[gep.indeces_end - 1];
                            //     var from_node = ls.mem_nodes.items[@intFromEnum(from_node_ref)];

                            //     switch (from_node) {
                            //         .heap_aggregrate => |_| {
                            //             try ls.put_own_node(to_inst, from_node.heap_aggregrate.fields[final_f_index]);
                            //             from_node.heap_aggregrate.fields[final_f_index] = .not_owned;
                            //         },
                            //         else => return error.Unimplemented,
                            //     }

                            //     ls.mem_nodes.items[@intFromEnum(from_node_ref)] = from_node;
                            //     continue;
                            // },
                            .alloca => {
                                try ls.move_mem_node(@intFromEnum(load.ptr), index);
                                continue;
                            },
                            else => continue,
                        }
                    }
                }
                print("\n", .{});
            },
            else => {
                print("Ignoring\n", .{});
            },
        }
    }
    ls.print_mem_nodes();
}

// fn move_ownership_all_fields(t: *const Tir, ls: *LinearState, from_field: FieldId, to_field: FieldId, cur_type: Type.IndexRef) !void {
//     switch (cur_type) {
//         .tir_boolean, .tir_unknown_int, .tir_u64, .tir_u32, .tir_u16, .tir_u8, .tir_i64, .tir_i32, .tir_i16, .tir_i8, .tir_void, .tir_typ, .tir_own, .tir_ref, .tir_stackref => return,
//         _ => {
//             const typ = t.types.get(@intFromEnum(cur_type));
//             print("typ {}\n", .{typ});
//             switch (typ) {
//                 .ptr => {
//                     if (typ.ptr.cap == .tir_own) {
//                         try ls.move_owner(from_field, to_field);
//                         // if (add) {
//                         //     return;
//                         //     // try ls.new_alloc(FieldId{ .inst = inst, .fields = cur_fields });
//                         // } else {
//                         //     // try ls.move_owner(, )
//                         //     // try ls.delete_alloc(FieldId{ .inst = inst, .fields = cur_fields });
//                         // }
//                         // return;
//                     } else if (typ.ptr.cap == .tir_stackref) {
//                         try move_ownership_all_fields(t, ls, from_field, to_field, typ.ptr.deref_type);
//                     }
//                 },
//                 .tir_struct => |tir_struct| {
//                     var cur_field_index = tir_struct.fields_start;
//                     while (cur_field_index < tir_struct.fields_end) : (cur_field_index += 1) {
//                         const cur_field = t.types.get(cur_field_index);
//                         switch (cur_field) {
//                             .tir_struct_field, .tir_enum_field, .tir_mut_struct_field => |field_info| {
//                                 var new_from_field = from_field;
//                                 new_from_field.fields[new_from_field.field_end] = @intCast(cur_field_index - tir_struct.fields_start);
//                                 new_from_field.field_end += 1;

//                                 var new_to_field = to_field;
//                                 new_to_field.fields[new_to_field.field_end] = @intCast(cur_field_index - tir_struct.fields_start);
//                                 new_to_field.field_end += 1;
//                                 try move_ownership_all_fields(t, ls, new_from_field, new_to_field, field_info.field_type);
//                             },
//                             else => unreachable,
//                         }
//                     }
//                 },
//                 .tir_struct_field, .tir_enum_field, .tir_mut_struct_field => unreachable,
//                 .tir_array => return error.Unimplemented,
//                 .tir_enum => return error.Unimplemented,
//             }
//         },
//     }
// }

fn analyse_fn(t: *const Tir, fn_def: TirInst.FnDef) !SafetyRes {
    // TODO: Analyse args.
    var ls = LinearState{
        // .owners = std.AutoHashMap(FieldId, TirInst.Index).init(t.allocator),
        .mem_nodes = std.ArrayList(MemoryNode).init(t.allocator),
        .inst_states = std.AutoHashMap(TirInst.Index, MemoryNode.IndexRef).init(t.allocator),
    };
    defer ls.mem_nodes.deinit();
    defer ls.inst_states.deinit();
    try analyse_bb(t, &ls, fn_def.blk);
    // if (ls.owners.count() > 0) {
    //     print("Memory leak detected.\n[", .{});
    //     var it = ls.owners.keyIterator();
    //     while (it.next()) |key| {
    //         print("{} allocation at  {any}\n", .{ key, ls.owners.get(key.*) });
    //     }
    //     print("]\n", .{});
    //     return error.MemoryLeak;
    // }
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
