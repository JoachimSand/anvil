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
        is_heap: bool,
        is_enum: bool,
        alloc_inst: TirInst.Index,
        num_fields: u8 = 0,
        fields: FieldArr = FieldArrUninit,
    };

    aggregrate: Aggregrate,
    owning_ptr: struct {
        // alloc_inst: TirInst.Index,
        dst: MemoryNode.IndexRef,
    },
    unowned_ptr,
};

fn equal_mem_refs(ls_a: *LinearState, ls_b: *LinearState, a: MemoryNode.IndexRef, b: MemoryNode.IndexRef) bool {
    switch (a) {
        .val => {
            return (b == .val);
        },
        .uninit => return (b == .uninit),
        _ => {
            if (@intFromEnum(b) < MemoryNode.RefStart) {
                const a_node = ls_a.mem_nodes.items[@intFromEnum(a)];
                const b_node = ls_b.mem_nodes.items[@intFromEnum(b)];
                // print("\n{} with\n", .{a_node});
                // print("{}\n", .{b_node});

                switch (a_node) {
                    .aggregrate => |agg| {
                        if (b_node != .aggregrate) {
                            return false;
                        }
                        const b_agg = b_node.aggregrate;

                        if (b_agg.is_heap != agg.is_heap) {
                            return false;
                        }
                        if (b_agg.is_enum != agg.is_enum) {
                            return false;
                        }

                        for (0..agg.num_fields) |f| {
                            if (equal_mem_refs(ls_a, ls_b, agg.fields[f], b_agg.fields[f]) == false) return false;
                        }
                        return true;
                    },
                    .unowned_ptr => {
                        return b_node == .unowned_ptr;
                    },
                    .owning_ptr => {
                        if (b_node != .owning_ptr) {
                            return false;
                        } else {
                            return equal_mem_refs(ls_a, ls_b, a_node.owning_ptr.dst, b_node.owning_ptr.dst);
                        }
                    },
                }
            } else {
                return false;
            }
        },
    }
}

const LinearState = struct {
    mem_nodes: std.ArrayList(MemoryNode),
    inst_states: std.AutoHashMap(TirInst.Index, MemoryNode.IndexRef),

    fn clone_state(ls: *LinearState) !LinearState {
        const new_ls = LinearState{
            .mem_nodes = try ls.mem_nodes.clone(),
            .inst_states = try ls.inst_states.clone(),
        };
        return new_ls;
    }

    fn deinit(ls: *LinearState) void {
        ls.mem_nodes.deinit();
        ls.inst_states.deinit();
    }

    fn is_equal(ls: *LinearState, other: *LinearState, ignore_above: TirInst.Index) bool {
        var key_it = ls.inst_states.keyIterator();
        while (key_it.next()) |key| {
            if (key.* >= ignore_above) {
                print("Ignoring {}\n", .{key.*});
                continue;
            }

            print("Searching for match to key {}\n", .{key.*});
            const ref = ls.inst_states.get(key.*).?;
            if (ref == .val or ref == .uninit) {
                continue;
            }

            var match = false;
            var other_it = other.inst_states.keyIterator();
            while (other_it.next()) |o_key| {
                print("Comparing with {}\n", .{o_key.*});
                const ref_a = ls.inst_states.get(key.*).?;
                const ref_b = other.inst_states.get(o_key.*).?;
                const equals = equal_mem_refs(ls, other, ref_a, ref_b);
                if (equals) {
                    match = true;
                    continue;
                }
            }
            if (match) {
                continue;
            }
            print("Failed\n", .{});
            return false;
        }
        var other_it = other.inst_states.keyIterator();
        while (other_it.next()) |o_key| {
            if (o_key.* >= ignore_above) {
                print("Ignoring {}\n", .{o_key.*});
                continue;
            }

            print("Searching for match to key {}\n", .{o_key.*});
            const ref = other.inst_states.get(o_key.*).?;
            if (ref == .val or ref == .uninit) {
                continue;
            }

            var match = false;
            var second_key_it = ls.inst_states.keyIterator();
            while (second_key_it.next()) |key| {
                print("Comparing with {}\n", .{key.*});
                const ref_a = other.inst_states.get(o_key.*).?;
                const ref_b = ls.inst_states.get(key.*).?;
                const equals = equal_mem_refs(ls, other, ref_a, ref_b);
                if (equals) {
                    match = true;
                    continue;
                }
            }
            if (match) {
                continue;
            }
            print("Failed\n", .{});
            return false;
        }
        return true;
    }

    fn copy_over_node(ls: *LinearState, other: *LinearState, index: MemoryNode.Index) void {
        const cur_node = ls.mem_nodes.items[index];
        switch (cur_node) {
            .aggregrate => |agg| {
                for (0..agg.num_fields) |f| {
                    if (@intFromEnum(agg.fields[f]) < MemoryNode.RefStart) {
                        copy_over_node(ls, other, @intFromEnum(agg.fields[f]));
                    }
                }
            },
            .owning_ptr => |ptr| {
                if (@intFromEnum(ptr.dst) < MemoryNode.RefStart)
                    copy_over_node(ls, other, @intFromEnum(ptr.dst));
            },
            else => {},
        }
        ls.mem_nodes.items[index] = other.mem_nodes.items[index];
    }

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

    fn print_linear_state(ls: *LinearState) void {
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
        .tir_boolean, .tir_unknown_int, .tir_u64, .tir_u32, .tir_u16, .tir_u8, .tir_i64, .tir_i32, .tir_i16, .tir_i8, .tir_void, .tir_typ, .tir_own, .tir_ref, .tir_stackref, .tir_address_of_self => {
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
                .tir_struct, .tir_enum => |container_fields| {
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
                    const is_enum: bool = typ == .tir_enum;
                    const num_fields: u8 = @intCast(container_fields.fields_end - container_fields.fields_start);
                    const mem_node = MemoryNode{ .aggregrate = .{ .is_heap = heap, .is_enum = is_enum, .alloc_inst = inst, .fields = fields, .num_fields = num_fields } };

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

fn change_to_heap(t: *const Tir, ls: *LinearState, mem_ref: MemoryNode.IndexRef, alloc_inst: TirInst.Index) !MemoryNode.IndexRef {
    switch (mem_ref) {
        .val => return .val,
        .uninit => return .uninit,
        _ => {
            const mem_index: MemoryNode.Index = @intFromEnum(mem_ref);
            const old_mem_node = ls.mem_nodes.items[mem_index];
            switch (old_mem_node) {
                .aggregrate => |aggregrate| {
                    if (aggregrate.is_heap) {
                        print("Got {}\n", .{old_mem_node});
                        return error.ExpectedStackAggregrates;
                    }

                    var new_node = MemoryNode{
                        .aggregrate = aggregrate,
                    };
                    new_node.aggregrate.is_heap = true;
                    new_node.aggregrate.alloc_inst = alloc_inst;
                    for (0..aggregrate.num_fields) |f| {
                        new_node.aggregrate.fields[f] = try change_to_heap(t, ls, new_node.aggregrate.fields[f], alloc_inst);
                    }

                    ls.mem_nodes.items[mem_index] = new_node;
                    return mem_ref;
                },
                .owning_ptr, .unowned_ptr => return mem_ref,
            }
        },
    }
}

fn change_to_stack(t: *const Tir, ls: *LinearState, mem_ref: MemoryNode.IndexRef, alloc_inst: TirInst.Index) !MemoryNode.IndexRef {
    switch (mem_ref) {
        .val => return .val,
        .uninit => return .uninit,
        _ => {
            const mem_index: MemoryNode.Index = @intFromEnum(mem_ref);
            const old_mem_node = ls.mem_nodes.items[mem_index];
            switch (old_mem_node) {
                .aggregrate => |aggregrate| {
                    if (aggregrate.is_heap == false) {
                        print("Got {}\n", .{old_mem_node});
                        return error.ExpectedHeapAggregrates;
                    }

                    var new_node = MemoryNode{ .aggregrate = aggregrate };
                    new_node.aggregrate.is_heap = false;
                    new_node.aggregrate.alloc_inst = alloc_inst;
                    for (0..aggregrate.num_fields) |f| {
                        new_node.aggregrate.fields[f] = try change_to_stack(t, ls, new_node.aggregrate.fields[f], alloc_inst);
                    }

                    ls.mem_nodes.items[mem_index] = new_node;
                    return mem_ref;
                },
                .owning_ptr, .unowned_ptr => return mem_ref,
            }
        },
    }
}

fn analyse_bb(t: *const Tir, ls: *LinearState, bb: TirInst.Index, continue_children: bool) !LinearState {
    const instructions = t.instructions.slice();

    const block = instructions.get(bb).block;

    for (block.start..block.end + 1) |inst_index| {
        const index: u32 = @intCast(inst_index);
        const inst = instructions.get(inst_index);
        ls.print_linear_state();
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
                _ = try change_to_heap(t, ls, expr_mem_ref, index);
                try ls.move_mem_node(@intFromEnum(memalloc.expr), index);
            },
            .memfree => |memfree| {
                const ptr_mem_ref = ls.inst_states.get(@intFromEnum(memfree.ptr)).?;
                _ = try change_to_stack(t, ls, ptr_mem_ref, index);
                try ls.move_mem_node(@intFromEnum(memfree.ptr), index);
            },
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
                                        .aggregrate => |aggregrate| to_node_ref = aggregrate.fields[f_index],
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
                            .alloca, .enum_project => {
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
                            .get_element_ptr => |gep| {
                                const from_inst: TirInst.Index = @intFromEnum(gep.aggregate_ptr);
                                print("Extrating a node from inst {}\n", .{from_inst});
                                var from_node_ref = ls.inst_states.get(from_inst) orelse return error.LoadingMovedValue;

                                const slice = t.extra.items[gep.indeces_start..gep.indeces_end];
                                for (slice) |f_index| {
                                    const from_node = ls.mem_nodes.items[@intFromEnum(from_node_ref)];

                                    switch (from_node) {
                                        .aggregrate => |aggregrate| from_node_ref = aggregrate.fields[f_index],
                                        else => return error.Unimplemented,
                                    }
                                }
                                const old_from_node = ls.mem_nodes.items[@intFromEnum(from_node_ref)];
                                // Remove ownership from the old node
                                ls.mem_nodes.items[@intFromEnum(from_node_ref)] = .unowned_ptr;
                                print("Extracting {}\n", .{old_from_node});

                                // Update
                                _ = ls.inst_states.remove(from_inst);
                                try ls.put_own_node(index, old_from_node.owning_ptr.dst);
                                continue;
                            },
                            .alloca, .enum_project => {
                                try ls.move_mem_node(@intFromEnum(load.ptr), index);
                                continue;
                            },
                            else => continue,
                        }
                    }
                }
                print("\n", .{});
            },
            .update_enum_ptr_with_ptr => |up_enum| {
                // Destination is a memory node owned by enum_ptr.
                const enum_ptr_index: TirInst.Index = @intFromEnum(up_enum.enum_ptr);
                const enum_node_ref = ls.inst_states.get(enum_ptr_index).?;
                const enum_node = ls.mem_nodes.items[@intFromEnum(enum_node_ref)];

                // Source is the pointer to the new tag contents.
                const tag_ptr_index: TirInst.Index = @intFromEnum(up_enum.new_tag_ptr);
                const source_node_ref = ls.inst_states.get(tag_ptr_index).?;
                // const source_node = ls.mem_nodes.items[@intFromEnum(enum_node_ref)];
                _ = ls.inst_states.remove(tag_ptr_index);

                // Change the unowned pointer to an owned one with reference to the new
                // tag contents.
                const unowned_ptr_ref = enum_node.aggregrate.fields[up_enum.new_tag];
                const unowned_ptr_node = &ls.mem_nodes.items[@intFromEnum(unowned_ptr_ref)];

                if (unowned_ptr_node.* != .unowned_ptr) {
                    return error.MovingToOwner;
                }
                unowned_ptr_node.* = .{ .owning_ptr = .{ .dst = source_node_ref } };
                ls.mem_nodes.items[@intFromEnum(enum_node_ref)] = enum_node;
                // try ls.put_own_node(, )
            },
            .enum_project => |project| {
                const ret_type = t.types.get(@intFromEnum(project.ret_type));
                if (ret_type == .ptr) {
                    if (type_is_ref(ret_type.ptr.deref_type) == false) {
                        continue;
                    }
                    const own_type = t.types.get(@intFromEnum(ret_type.ptr.deref_type));
                    if (own_type == .ptr and own_type.ptr.cap == .tir_own) {} else {
                        continue;
                    }
                } else {
                    return error.UnexpectedProjectType;
                }

                // Source is enum_ptr : This is the enum we are projecting from.
                // Specifically, the field with the same tag as in the enum project inst.
                const enum_node_ref = ls.inst_states.get(project.enum_ptr).?;
                const enum_node = ls.mem_nodes.items[@intFromEnum(enum_node_ref)];

                // Get the source node from the enum.
                const owning_ptr_ref = enum_node.aggregrate.fields[project.tag];
                const owning_ptr_node = ls.mem_nodes.items[@intFromEnum(owning_ptr_ref)];
                const dst = owning_ptr_node.owning_ptr.dst;

                // Remove it from the enum itself.
                ls.mem_nodes.items[@intFromEnum(owning_ptr_ref)] = .unowned_ptr;

                // Destination is this instruction.
                try ls.put_own_node(index, dst);
            },
            .match => |match| {
                const case_slice = t.extra.items[match.cases_start..match.cases_end];
                var case_states: [2]LinearState = .{ try ls.clone_state(), try ls.clone_state() };
                // defer case_states[0].deinit();
                defer case_states[1].deinit();
                defer ls.deinit();
                for (case_slice, 0..) |blk, ci| {
                    _ = try analyse_bb(t, &case_states[ci], blk, false);
                }
                print("\n\n", .{});
                const enum_node_ref = ls.inst_states.get(@intFromEnum(match.enum_ptr)).?;
                const enum_node_index: MemoryNode.Index = @intFromEnum(enum_node_ref);

                const case_0_enum = case_states[0].mem_nodes.items[enum_node_index];
                const case_1_enum = case_states[1].mem_nodes.items[enum_node_index];
                case_states[0].copy_over_node(&case_states[1], @intFromEnum(case_1_enum.aggregrate.fields[1]));
                case_states[1].copy_over_node(&case_states[0], @intFromEnum(case_0_enum.aggregrate.fields[0]));

                // case_0_enum.aggregrate.fields[1] = case_1_enum.aggregrate.fields[1];
                // case_1_enum.aggregrate.fields[0] = case_0_enum.aggregrate.fields[0];

                case_states[0].print_linear_state();
                case_states[1].print_linear_state();

                if (case_states[0].is_equal(&case_states[1], index) == false) {
                    return error.OwnershipDiverges;
                }
                return case_states[0];
            },
            // .br => |br| {
            //     return analyse_bb(t, ls, br);
            // },
            .br_either => |br| {
                if (continue_children == false) {
                    return error.Unimplemented;
                } else {
                    var then_ls = try ls.clone_state();
                    var else_ls = try ls.clone_state();
                    defer then_ls.deinit();
                    // defer else_ls.deinit();
                    _ = try analyse_bb(t, &then_ls, br.then_blk, false);
                    _ = try analyse_bb(t, &else_ls, br.else_blk, false);
                    print("\n Then LS {}", .{then_ls});
                    then_ls.print_linear_state();
                    print("\n Else LS {}\n", .{else_ls});
                    else_ls.print_linear_state();

                    if (then_ls.is_equal(&else_ls, index) == false) {
                        return error.OwnershipDiverges;
                    }
                    ls.deinit();
                    // ls.mem_nodes.items
                    const then_block_end = t.instructions.get(br.then_blk).block.end;
                    const next_block = t.instructions.get(then_block_end).br;
                    return analyse_bb(t, &else_ls, next_block, true);
                }
                return;
            },
            else => {
                print("Ignoring\n", .{});
            },
        }
    }
    ls.print_linear_state();
    return ls.*;
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
    var res = try analyse_bb(t, &ls, fn_def.blk, true);
    defer res.deinit();

    for (ls.mem_nodes.items) |node| {
        switch (node) {
            .aggregrate => |agg| {
                if (agg.is_heap) {
                    return error.MemoryLeak;
                }
            },
            .owning_ptr => |_| {
                return error.MemoryLeak;
            },
            else => continue,
        }
    }

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
