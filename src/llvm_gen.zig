const std = @import("std");
const print = std.debug.print;

const llvm = @import("llvm");
const target = llvm.target;
const target_machine = llvm.target_machine;
const types = llvm.types;
const core = llvm.core;
const bitwriter = llvm.bitwriter;

const tir_mod = @import("tir.zig");
const Type = tir_mod.Type;
const Value = tir_mod.Value;
const Tir = tir_mod.Tir;
const TirInst = tir_mod.TirInst;
const type_is_ref = tir_mod.type_is_ref;

const LLVMState = struct {
    tir: *Tir,

    tir_llvm_val_map: std.AutoHashMap(TirInst.IndexRef, types.LLVMValueRef),
    tir_llvm_blk_map: std.AutoHashMap(TirInst.Index, types.LLVMBasicBlockRef),
    tir_llvm_typ_map: std.AutoHashMap(Type.IndexRef, types.LLVMTypeRef),

    print_func: types.LLVMValueRef,
    print_func_type: types.LLVMTypeRef,

    module: types.LLVMModuleRef,
    builder: types.LLVMBuilderRef,
    cur_func: types.LLVMValueRef,

    fn get_inst_mapping(s: *LLVMState, inst: TirInst.IndexRef) !types.LLVMValueRef {
        return s.tir_llvm_val_map.get(inst) orelse {
            print("Missing mapping for {}\n", .{inst});
            return error.MissingMapping;
        };
    }

    fn get_blk_mapping(s: *LLVMState, blk: TirInst.Index) !types.LLVMBasicBlockRef {
        return s.tir_llvm_blk_map.get(blk) orelse {
            print("Missing mapping for {}\n", .{blk});
            return error.MissingMapping;
        };
    }

    fn get_typ_mapping(s: *LLVMState, typ: Type.IndexRef) !types.LLVMTypeRef {
        return s.tir_llvm_typ_map.get(typ) orelse {
            print("Missing mapping for {}\n", .{typ});
            return error.MissingMapping;
        };
    }
};

fn tir_type_to_llvm(s: *LLVMState, type_ref: Type.IndexRef) !types.LLVMTypeRef {
    if (type_is_ref(type_ref)) {
        const typ = s.tir.types.get(@intFromEnum(type_ref));
        switch (typ) {
            .tir_struct => |tir_struct| {
                var field_types = std.ArrayList(types.LLVMTypeRef).init(s.tir.allocator);
                defer field_types.deinit();

                var cur_field_index = tir_struct.fields_start;
                while (cur_field_index < tir_struct.fields_end) : (cur_field_index += 1) {
                    const cur_field = s.tir.types.get(cur_field_index);
                    switch (cur_field) {
                        .tir_struct_field, .tir_mut_struct_field => |field| {
                            const field_type = field.field_type;
                            const llvm_type = try tir_type_to_llvm(s, field_type);
                            try field_types.append(llvm_type);
                        },
                        else => unreachable,
                    }
                }
                return core.LLVMStructType(field_types.items.ptr, @intCast(field_types.items.len), 0);
            },
            .tir_enum => |_| {
                var field_types = std.ArrayList(types.LLVMTypeRef).init(s.tir.allocator);
                defer field_types.deinit();
                try field_types.append(try s.get_typ_mapping(.tir_u8));
                // try field_types.append(core.LLVMArrayType(try s.get_typ_mapping(.tir_i32), 4));
                try field_types.append(core.LLVMPointerType(core.LLVMVoidType(), 0));

                // var cur_field_index = tir_enum.fields_start;
                // while (cur_field_index < tir_enum.fields_end) : (cur_field_index += 1) {
                //     const cur_field = s.tir.types.get(cur_field_index);
                //     switch (cur_field) {
                //         .tir_enum_field => |field| {
                //             const field_type = field.field_type;
                //             const llvm_type = try tir_type_to_llvm(s, field_type);
                //             try field_types.append(llvm_type);
                //         },
                //         else => unreachable,
                //     }
                // }
                return core.LLVMStructType(field_types.items.ptr, @intCast(field_types.items.len), 0);
            },
            .ptr => |ptr| {
                const element_typ = try tir_type_to_llvm(s, ptr.deref_type);
                return core.LLVMPointerType(element_typ, 0);
            },
            else => return error.Unimplemented,
        }
    } else {
        return s.get_typ_mapping(type_ref);
    }
}

fn tir_val_to_llvm(tir: *Tir, index: Value.Index) !types.LLVMValueRef {
    const value = tir.values.get(index);
    switch (value) {
        .null_val => return error.Unimplemented,
        .unknown_int => |val| return core.LLVMConstInt(core.LLVMInt64Type(), @intCast(val), 0),
        .u8 => |val| return core.LLVMConstInt(core.LLVMInt8Type(), @intCast(val), 0),
        .u16 => |val| return core.LLVMConstInt(core.LLVMInt16Type(), @intCast(val), 0),
        .u32 => |val| return core.LLVMConstInt(core.LLVMInt32Type(), @intCast(val), 0),
        .u64 => |val| return core.LLVMConstInt(core.LLVMInt64Type(), @intCast(val), 0),
        .i8 => |val| return core.LLVMConstInt(core.LLVMInt8Type(), @intCast(val), 1),
        .i16 => |val| return core.LLVMConstInt(core.LLVMInt16Type(), @intCast(val), 1),
        .i32 => |val| return core.LLVMConstInt(core.LLVMInt32Type(), @intCast(val), 1),
        .i64 => |val| return core.LLVMConstInt(core.LLVMInt64Type(), @intCast(val), 1),
        .boolean => |val| return core.LLVMConstInt(core.LLVMInt1Type(), @intFromBool(val), 0),
        // .u8 => |val| return core.LLVMConstInt(core.LLVMInt8Type(), val, 0),
    }
}

fn copy_struct(s: *LLVMState, topmost_agg_type: types.LLVMTypeRef, type_ref: Type.IndexRef, src_val: types.LLVMValueRef, dest_val: types.LLVMValueRef, gep_fields: *std.ArrayList(types.LLVMValueRef)) !void {
    switch (type_ref) {
        .tir_typ, .tir_own, .tir_ref, .tir_stackref, .tir_void, .tir_unknown_int => return error.Unimplemented,
        .tir_boolean, .tir_i8, .tir_i16, .tir_i32, .tir_i64, .tir_u8, .tir_u16, .tir_u32, .tir_u64, .tir_opaque => {
            const llvm_src_gep = core.LLVMBuildGEP2(s.builder, topmost_agg_type, src_val, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
            const llvm_load = core.LLVMBuildLoad2(s.builder, try tir_type_to_llvm(s, type_ref), llvm_src_gep, "");

            const llvm_dst_gep = core.LLVMBuildGEP2(s.builder, topmost_agg_type, dest_val, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
            const llvm_store = core.LLVMBuildStore(s.builder, llvm_load, llvm_dst_gep);
            _ = llvm_store;
        },
        _ => {
            const typ = s.tir.types.get(@intFromEnum(type_ref));
            switch (typ) {
                .tir_struct => |agg| {
                    var type_index = agg.fields_start;
                    while (type_index < agg.fields_end) : (type_index += 1) {
                        const cur_field = s.tir.types.get(type_index);
                        const field_index: u32 = @intCast(type_index - agg.fields_start);

                        switch (cur_field) {
                            .tir_struct_field, .tir_mut_struct_field => |field| {
                                try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), @intCast(field_index), 0));
                                try copy_struct(s, topmost_agg_type, field.field_type, src_val, dest_val, gep_fields);
                                _ = gep_fields.pop();
                            },
                            else => {
                                print("{} {}\n", .{ cur_field, type_index });
                                unreachable;
                            },
                        }
                    }
                },
                .tir_enum => |_| {
                    try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), 0, 0));
                    try copy_struct(s, topmost_agg_type, .tir_i8, src_val, dest_val, gep_fields);
                    _ = gep_fields.pop();
                    try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), 1, 0));
                    try copy_struct(s, topmost_agg_type, .tir_u64, src_val, dest_val, gep_fields);
                    _ = gep_fields.pop();
                },
                .ptr => |_| {
                    // try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), 0, 0));
                    const llvm_src_gep = core.LLVMBuildGEP2(s.builder, topmost_agg_type, src_val, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
                    const llvm_load = core.LLVMBuildLoad2(s.builder, try tir_type_to_llvm(s, type_ref), llvm_src_gep, "");

                    const llvm_dst_gep = core.LLVMBuildGEP2(s.builder, topmost_agg_type, dest_val, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
                    const llvm_store = core.LLVMBuildStore(s.builder, llvm_load, llvm_dst_gep);
                    _ = llvm_store;
                    // _ = gep_fields.pop();
                },
                else => return error.Unimplemented,
            }
        },
    }
}

pub fn generate_llvm_ir_block(s: *LLVMState, blk_index: TirInst.Index, new_bb: bool) !void {
    if (s.tir_llvm_blk_map.contains(blk_index)) {
        return;
    }

    const blk = s.tir.instructions.get(blk_index).block;
    if (new_bb) {
        const llvm_bb: types.LLVMBasicBlockRef = core.LLVMAppendBasicBlock(s.cur_func, "");
        core.LLVMPositionBuilderAtEnd(s.builder, llvm_bb);
        try s.tir_llvm_blk_map.put(blk_index, llvm_bb);
    }

    const instructions = s.tir.instructions.slice();
    var inst_index = blk.start;
    while (inst_index <= blk.end) : (inst_index += 1) {
        // print("TIR Index: {}\n", .{inst_index});
        // const inst_index: TirInst.Index = @intCast(inst_index_usize);
        const instruction = s.tir.instructions.get(inst_index);
        const inst_ref: TirInst.IndexRef = @enumFromInt(inst_index);

        switch (instruction) {
            .fn_def => |fn_def| {
                // print("Fn def\n", .{});
                var llvm_params = std.ArrayList(types.LLVMTypeRef).init(s.tir.allocator);
                defer llvm_params.deinit();

                const tir_params = s.tir.extra.items[fn_def.params.start..fn_def.params.end];
                var is_comptime = false;
                for (tir_params) |param_inst_index| {
                    const param_inst = instructions.get(param_inst_index).arg;
                    // print("Param typ ref {}\n", .{param_inst.typ_ref});
                    if (param_inst.typ_ref == .tir_typ) {
                        is_comptime = true;
                        break;
                    }
                    const llvm_param_type = try tir_type_to_llvm(s, param_inst.typ_ref);
                    try llvm_params.append(llvm_param_type);
                }

                if (is_comptime) {
                    // print("Skipping comptime function\n", .{});
                    const fn_def_blk = s.tir.instructions.get(fn_def.blk).block;
                    inst_index = @intCast(fn_def_blk.end);
                    llvm_params.deinit();
                    continue;
                }

                // var params: [2]types.LLVMTypeRef = [_]types.LLVMTypeRef{
                //     core.LLVMInt32Type(),
                //     core.LLVMInt32Type(),
                // };
                const llvm_ret_type = try tir_type_to_llvm(s, fn_def.ret_type);
                const func_type: types.LLVMTypeRef = core.LLVMFunctionType(llvm_ret_type, llvm_params.items.ptr, @intCast(llvm_params.items.len), 0);
                const func: types.LLVMValueRef = core.LLVMAddFunction(s.module, "anvilmain", func_type);
                try s.tir_llvm_val_map.put(inst_ref, func);

                // map the arguments as well

                for (tir_params, 0..) |param_inst_index, i| {
                    const arg = core.LLVMGetParam(func, @intCast(i));
                    try s.tir_llvm_val_map.put(@enumFromInt(param_inst_index), arg);
                }

                core.LLVMSetLinkage(func, types.LLVMLinkage.LLVMExternalLinkage);
                s.cur_func = func;
                try generate_llvm_ir_block(s, fn_def.blk, true);
                // const entry_blk = instructions.get(fn_def.blk).block;
                // inst_index = entry_blk.end;
                return;
            },
            .arg => continue,
            .block => |_| {
                try generate_llvm_ir_block(s, inst_index, true);
                return;
                // inst_index = new_blk.end;
            },
            .br_either => |br| {
                // TODO: Could insert br inst. then modify contents after target
                // blocks have been generated.

                try generate_llvm_ir_block(s, br.then_blk, true);
                try generate_llvm_ir_block(s, br.else_blk, true);
                const this_bb = try s.get_blk_mapping(blk_index);
                core.LLVMPositionBuilderAtEnd(s.builder, this_bb);

                const llvm_then = try s.get_blk_mapping(br.then_blk);
                const llvm_else = try s.get_blk_mapping(br.else_blk);
                const llvm_cond = try s.get_inst_mapping(@enumFromInt(br.cond));
                const llvm_br_cond = core.LLVMBuildCondBr(s.builder, llvm_cond, llvm_then, llvm_else);
                try s.tir_llvm_val_map.put(@enumFromInt(blk_index), llvm_br_cond);

                return;
            },
            .br => |target_blk| {
                try generate_llvm_ir_block(s, target_blk, true);
                const this_bb = try s.get_blk_mapping(blk_index);
                core.LLVMPositionBuilderAtEnd(s.builder, this_bb);

                const llvm_target_blk = try s.get_blk_mapping(target_blk);
                const llvm_br = core.LLVMBuildBr(s.builder, llvm_target_blk);
                try s.tir_llvm_val_map.put(@enumFromInt(blk_index), llvm_br);
                return;
            },
            .ret_void => {
                const llvm_ret_void = core.LLVMBuildRetVoid(s.builder);
                try s.tir_llvm_val_map.put(inst_ref, llvm_ret_void);
            },
            // .ret_empty => {
            //     const llvm_ret_void = core.LLVMBuildRetVoid(s.builder);
            //     try s.tir_llvm_val_map.put(inst_ref, llvm_ret_void);
            // },
            .alloca => |alloca| {
                // const u8_index: u8 = @intCast(inst_index);
                // var buf: [4:0]u8 = .{ u8_index % 10 + 48, 0, 0, 0 };
                const llvm_allcoca = core.LLVMBuildAlloca(s.builder, try tir_type_to_llvm(s, alloca.alloc_type), "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_allcoca);
            },
            .store => |store| {
                const llvm_ptr = try s.get_inst_mapping(store.ptr);
                const llvm_val = try s.get_inst_mapping(store.val);

                const llvm_store = core.LLVMBuildStore(s.builder, llvm_val, llvm_ptr);
                try s.tir_llvm_val_map.put(inst_ref, llvm_store);
            },
            .load => |load| {
                const llvm_ptr = try s.get_inst_mapping(load.ptr);

                const llvm_load = core.LLVMBuildLoad2(s.builder, try tir_type_to_llvm(s, load.type), llvm_ptr, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_load);
            },
            .get_element_ptr => |gep| {
                var gep_fields = std.ArrayList(types.LLVMValueRef).init(s.tir.allocator);
                try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), 0, 0));
                defer gep_fields.deinit();

                const slice = s.tir.extra.items[gep.indeces_start..gep.indeces_end];
                for (slice) |f| {
                    try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), @intCast(f), 0));
                }
                const llvm_agg_ptr = try s.get_inst_mapping(gep.aggregate_ptr);
                const llvm_agg_typ = try tir_type_to_llvm(s, gep.aggregate_type);

                const llvm_gep = core.LLVMBuildGEP2(s.builder, llvm_agg_typ, llvm_agg_ptr, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_gep);
            },
            .lt_i8, .lt_i16, .lt_i32, .lt_i64 => |lt| {
                const llvm_lhs = try s.get_inst_mapping(lt.lhs);
                const llvm_rhs = try s.get_inst_mapping(lt.rhs);
                const llvm_lt = core.LLVMBuildICmp(s.builder, types.LLVMIntPredicate.LLVMIntSLT, llvm_lhs, llvm_rhs, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_lt);
            },
            .lt_u8, .lt_u16, .lt_u32, .lt_u64 => |lt| {
                const llvm_lhs = try s.get_inst_mapping(lt.lhs);
                const llvm_rhs = try s.get_inst_mapping(lt.rhs);
                const llvm_lt = core.LLVMBuildICmp(s.builder, types.LLVMIntPredicate.LLVMIntULT, llvm_lhs, llvm_rhs, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_lt);
            },
            .add_i8, .add_i16, .add_i32, .add_i64, .add_u8, .add_u16, .add_u32, .add_u64 => |add| {
                const llvm_lhs = try s.get_inst_mapping(add.lhs);
                const llvm_rhs = try s.get_inst_mapping(add.rhs);
                const llvm_add = core.LLVMBuildAdd(s.builder, llvm_lhs, llvm_rhs, "");
                // const llvm_lt = core.LLVMBuildICmp(s.builder, types.LLVMIntPredicate.LLVMIntSLT, llvm_lhs, llvm_rhs, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_add);
            },
            .constant_type => |type_ref| {
                const typ = try tir_type_to_llvm(s, type_ref);
                try s.tir_llvm_typ_map.put(type_ref, typ);
            },
            .constant_val => |val| {
                const llvm_val = try tir_val_to_llvm(s.tir, val);

                try s.tir_llvm_val_map.put(inst_ref, llvm_val);
            },
            .move => |mov| {
                try s.tir_llvm_val_map.put(inst_ref, try s.get_inst_mapping(@enumFromInt(mov)));
            },
            .memalloc => |memalloc| {
                const ptr_typ = s.tir.types.get(@intFromEnum(memalloc.ptr_type));
                const agg_typ_ref = ptr_typ.ptr.deref_type;
                const llvm_typ = try tir_type_to_llvm(s, agg_typ_ref);
                const llvm_alloc = core.LLVMBuildMalloc(s.builder, llvm_typ, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_alloc);

                const llvm_src = try s.get_inst_mapping(memalloc.expr);
                var gep_fields = std.ArrayList(types.LLVMValueRef).init(s.tir.allocator);
                try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), 0, 0));
                defer gep_fields.deinit();
                // const alignment = core.LLVMAlignOf(llvm_typ);
                // print("ALIGNMENT: {any}\n", .{alignment});
                // _ = core.LLVMBuildMemCpy(s.builder, llvm_alloc, alignment, llvm_src, alignment, null);
                try copy_struct(s, llvm_typ, agg_typ_ref, llvm_src, llvm_alloc, &gep_fields);
            },
            .memfree => |memfree| {

                // memfree returns a stackref pointer, get the type of the contents
                const expr_ptr_type = s.tir.types.get(@intFromEnum(memfree.expr_type));
                const agg_typ_ref = expr_ptr_type.ptr.deref_type;
                const expr_llvm_typ = try tir_type_to_llvm(s, agg_typ_ref);

                const llvm_ptr = try s.get_inst_mapping(memfree.ptr);

                const llvm_alloca = core.LLVMBuildAlloca(s.builder, expr_llvm_typ, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_alloca);
                var gep_fields = std.ArrayList(types.LLVMValueRef).init(s.tir.allocator);
                defer gep_fields.deinit();
                try gep_fields.append(core.LLVMConstInt(core.LLVMInt32Type(), 0, 0));
                try copy_struct(s, expr_llvm_typ, agg_typ_ref, llvm_ptr, llvm_alloca, &gep_fields);

                _ = core.LLVMBuildFree(s.builder, llvm_ptr);
            },
            .print => |p| {
                const llvm_str = core.LLVMBuildGlobalStringPtr(s.builder, "%i\n", "");
                const llvm_arg = try s.get_inst_mapping(p.val);
                var args = [_]types.LLVMValueRef{ llvm_str, llvm_arg };

                _ = core.LLVMBuildCall2(s.builder, s.print_func_type, s.print_func, &args, 2, "");
            },
            .update_enum_ptr_with_ptr => |update_enum| {
                var content_fields = [_]types.LLVMValueRef{ core.LLVMConstInt(core.LLVMInt32Type(), 0, 0), core.LLVMConstInt(core.LLVMInt32Type(), 1, 0) };
                var tag_fields = [_]types.LLVMValueRef{ core.LLVMConstInt(core.LLVMInt32Type(), 0, 0), core.LLVMConstInt(core.LLVMInt32Type(), 0, 0) };
                const llvm_enum_type = try tir_type_to_llvm(s, update_enum.enum_type);
                const llvm_enum_ptr = try s.get_inst_mapping(update_enum.enum_ptr);
                const llvm_contents_ptr = try s.get_inst_mapping(update_enum.new_tag_ptr);

                const llvm_load_contents = core.LLVMBuildLoad2(s.builder, core.LLVMPointerType(core.LLVMVoidType(), 0), llvm_contents_ptr, "");

                const llvm_contents_dest = core.LLVMBuildGEP2(s.builder, llvm_enum_type, llvm_enum_ptr, &content_fields, @intCast(content_fields.len), "");
                const llvm_store_contents = core.LLVMBuildStore(s.builder, llvm_load_contents, llvm_contents_dest);
                _ = llvm_store_contents;

                const llvm_tag_dest = core.LLVMBuildGEP2(s.builder, llvm_enum_type, llvm_enum_ptr, &tag_fields, @intCast(tag_fields.len), "");
                const llvm_store_tag = core.LLVMBuildStore(s.builder, core.LLVMConstInt(core.LLVMInt32Type(), update_enum.new_tag, 0), llvm_tag_dest);
                _ = llvm_store_tag;
                // const llvm_tag_contents_ptr = try s.get_inst_mapping(update_enum.new_tag_ptr);

                // const llvm_gep_tag_contents = core.LLVMBuildGEP2(s.builder, llvm_enum_type, , &tag_content_fields, @intCast(tag_content_fields.len), "");
                // const llvm_load_tag = core.LLVMBuildLoad2(s.builder, try tir_type_to_llvm(s, try s.get_typ_mapping(.tir_u8)), llvm_gep_tag_contents, "");

                // const llvm_dst_gep = core.LLVMBuildGEP2(s.builder, llvm_enum_type, dest_val, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
                // const llvm_store = core.LLVMBuildStore(s.builder, llvm_load_tag, llvm_contents_dest);
            },
            .update_enum_ptr_with_val => |update_enum| {
                var content_fields = [_]types.LLVMValueRef{ core.LLVMConstInt(core.LLVMInt32Type(), 0, 0), core.LLVMConstInt(core.LLVMInt32Type(), 1, 0) };
                var tag_fields = [_]types.LLVMValueRef{ core.LLVMConstInt(core.LLVMInt32Type(), 0, 0), core.LLVMConstInt(core.LLVMInt32Type(), 0, 0) };
                const llvm_enum_type = try tir_type_to_llvm(s, update_enum.enum_type);
                const llvm_enum_ptr = try s.get_inst_mapping(update_enum.enum_ptr);

                if (update_enum.new_tag_val != .tir_null_lit) {
                    const llvm_val = try tir_val_to_llvm(s.tir, @intFromEnum(update_enum.new_tag_val));
                    const llvm_contents_dest = core.LLVMBuildGEP2(s.builder, llvm_enum_type, llvm_enum_ptr, &content_fields, @intCast(content_fields.len), "");
                    const llvm_store_contents = core.LLVMBuildStore(s.builder, llvm_val, llvm_contents_dest);
                    _ = llvm_store_contents;
                }

                const llvm_tag_dest = core.LLVMBuildGEP2(s.builder, llvm_enum_type, llvm_enum_ptr, &tag_fields, @intCast(tag_fields.len), "");
                const llvm_store_tag = core.LLVMBuildStore(s.builder, core.LLVMConstInt(core.LLVMInt32Type(), update_enum.new_tag, 0), llvm_tag_dest);
                _ = llvm_store_tag;
                // const llvm_tag_contents_ptr = try s.get_inst_mapping(update_enum.new_tag_ptr);

                // const llvm_gep_tag_contents = core.LLVMBuildGEP2(s.builder, llvm_enum_type, , &tag_content_fields, @intCast(tag_content_fields.len), "");
                // const llvm_load_tag = core.LLVMBuildLoad2(s.builder, try tir_type_to_llvm(s, try s.get_typ_mapping(.tir_u8)), llvm_gep_tag_contents, "");

                // const llvm_dst_gep = core.LLVMBuildGEP2(s.builder, llvm_enum_type, dest_val, gep_fields.items.ptr, @intCast(gep_fields.items.len), "");
                // const llvm_store = core.LLVMBuildStore(s.builder, llvm_load_tag, llvm_contents_dest);
            },
            .match => {
                continue;
            },
            else => {
                print("LLVM Generation so far: \n", .{});
                core.LLVMDumpModule(s.module);

                print("LLVM Generation unimplemented for: \n", .{});
                try tir_mod.print_tir(s.tir, inst_index, inst_index + 1, 0);

                return error.Unimplemented;
            },
        }
    }
}

pub fn generate_llvm_ir(tir: *Tir) !void {

    // Initialize LLVM
    _ = target.LLVMInitializeNativeTarget();
    _ = target.LLVMInitializeNativeAsmPrinter();
    _ = target.LLVMInitializeNativeAsmParser();

    var llvm_state = LLVMState{
        .tir = tir,

        .tir_llvm_val_map = std.AutoHashMap(TirInst.IndexRef, types.LLVMValueRef).init(tir.allocator),
        .tir_llvm_blk_map = std.AutoHashMap(TirInst.Index, types.LLVMBasicBlockRef).init(tir.allocator),
        .tir_llvm_typ_map = std.AutoHashMap(Type.IndexRef, types.LLVMTypeRef).init(tir.allocator),

        .print_func = undefined,
        .print_func_type = undefined,

        .module = core.LLVMModuleCreateWithName("anvil_module"),
        .builder = core.LLVMCreateBuilder(),
        .cur_func = undefined,
    };
    defer llvm_state.tir_llvm_val_map.deinit();
    defer llvm_state.tir_llvm_blk_map.deinit();
    defer llvm_state.tir_llvm_typ_map.deinit();
    defer core.LLVMDisposeBuilder(llvm_state.builder);
    defer core.LLVMDisposeModule(llvm_state.module);
    defer core.LLVMShutdown();

    const triple = target_machine.LLVMGetDefaultTargetTriple() orelse return error.MissingTriple;

    target.LLVMInitializeAllTargetInfos();
    target.LLVMInitializeAllTargets();
    target.LLVMInitializeAllTargetMCs();
    target.LLVMInitializeAllAsmParsers();
    target.LLVMInitializeAllAsmPrinters();

    const cpu = "generic";
    // var error_buf : [*c]u8 = undefined;
    var target_ref: types.LLVMTargetRef = undefined;
    _ = target_machine.LLVMGetTargetFromTriple(triple, &target_ref, null); //orelse return error.TargetRefNotFound;
    const machine_ref = target_machine.LLVMCreateTargetMachine(target_ref, triple, cpu, "", types.LLVMCodeGenOptLevel.LLVMCodeGenLevelNone, types.LLVMRelocMode.LLVMRelocPIC, types.LLVMCodeModel.LLVMCodeModelSmall);
    _ = machine_ref;
    // const data_layout = target_machine.LLVMCreateTargetDataLayout(machine_ref);

    // const res = target_machine.LLVMTargetMachineEmitToFile(machine_ref, llvm_state.module, "output.o", types.LLVMCodeGenFileType.LLVMObjectFile, null);
    // if (res != 0) {
    //     return error.EmittingObjectFileFailed;
    // }

    // Specify target and data layout
    // core.LLVMSetDataLayout(llvm_state.module, data_layout);
    core.LLVMSetTarget(llvm_state.module, triple);

    try llvm_state.tir_llvm_typ_map.put(.tir_boolean, core.LLVMInt1Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_i8, core.LLVMInt8Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_i16, core.LLVMInt16Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_i32, core.LLVMInt32Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_i64, core.LLVMInt64Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_u8, core.LLVMInt8Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_u16, core.LLVMInt16Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_u32, core.LLVMInt32Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_u64, core.LLVMInt64Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_unknown_int, core.LLVMInt64Type());
    try llvm_state.tir_llvm_typ_map.put(.tir_void, core.LLVMVoidType());
    try llvm_state.tir_llvm_typ_map.put(.tir_opaque, core.LLVMPointerType(core.LLVMVoidType(), 0));

    // Add printf function
    var param_types = [_]types.LLVMTypeRef{core.LLVMPointerType(core.LLVMInt8Type(), 0)};
    llvm_state.print_func_type = core.LLVMFunctionType(core.LLVMInt32Type(), &param_types, @intCast(param_types.len), 1);
    llvm_state.print_func = core.LLVMAddFunction(llvm_state.module, "printf", llvm_state.print_func_type);

    try generate_llvm_ir_block(&llvm_state, 0, false);

    print("LLVM Generation: \n", .{});
    core.LLVMDumpModule(llvm_state.module);
    _ = bitwriter.LLVMWriteBitcodeToFile(llvm_state.module, "outputs/output.bc");
}
