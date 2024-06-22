const std = @import("std");
const print = std.debug.print;

const llvm = @import("llvm");
const target = llvm.target;
const target_machine = llvm.target_machine;
const types = llvm.types;
const core = llvm.core;

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
            else => return error.Unimplemented,
        }
    } else {
        return s.get_typ_mapping(type_ref);
    }
}

fn tir_val_to_llvm(tir: *Tir, index: Value.Index) !types.LLVMValueRef {
    const value = tir.values.get(index);
    switch (value) {
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
        // const inst_index: TirInst.Index = @intCast(inst_index_usize);
        const instruction = s.tir.instructions.get(inst_index);
        const inst_ref: TirInst.IndexRef = @enumFromInt(inst_index);

        switch (instruction) {
            .fn_def => |fn_def| {
                var llvm_params = std.ArrayList(types.LLVMTypeRef).init(s.tir.allocator);
                defer llvm_params.deinit();

                const tir_params = s.tir.extra.items[fn_def.params.start..fn_def.params.end];
                for (tir_params) |param_inst_index| {
                    const param_inst = instructions.get(param_inst_index).arg;
                    const llvm_param_type = try s.get_typ_mapping(param_inst.typ_ref);
                    try llvm_params.append(llvm_param_type);
                }

                // var params: [2]types.LLVMTypeRef = [_]types.LLVMTypeRef{
                //     core.LLVMInt32Type(),
                //     core.LLVMInt32Type(),
                // };
                const func_type: types.LLVMTypeRef = core.LLVMFunctionType(core.LLVMInt32Type(), llvm_params.items.ptr, @intCast(llvm_params.items.len), 0);
                const func: types.LLVMValueRef = core.LLVMAddFunction(s.module, "main", func_type);
                try s.tir_llvm_val_map.put(inst_ref, func);

                // map the arguments as well

                for (tir_params, 0..) |param_inst_index, i| {
                    const arg = core.LLVMGetParam(func, @intCast(i));
                    try s.tir_llvm_val_map.put(@enumFromInt(param_inst_index), arg);
                }

                core.LLVMSetLinkage(func, types.LLVMLinkage.LLVMExternalLinkage);
                s.cur_func = func;
                try generate_llvm_ir_block(s, fn_def.blk, true);
                const entry_blk = instructions.get(fn_def.blk).block;
                inst_index = entry_blk.end;
            },
            .arg => continue,
            .block => |new_blk| {
                try generate_llvm_ir_block(s, inst_index, true);
                inst_index = new_blk.end;
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
            },
            .br => |target_blk| {
                try generate_llvm_ir_block(s, target_blk, true);
                const this_bb = try s.get_blk_mapping(blk_index);
                core.LLVMPositionBuilderAtEnd(s.builder, this_bb);

                const llvm_target_blk = try s.get_blk_mapping(target_blk);
                const llvm_br = core.LLVMBuildBr(s.builder, llvm_target_blk);
                try s.tir_llvm_val_map.put(@enumFromInt(blk_index), llvm_br);
            },
            .alloca => |alloca| {
                // const u8_index: u8 = @intCast(inst_index);
                // var buf: [4:0]u8 = .{ u8_index % 10 + 48, 0, 0, 0 };
                const llvm_allcoca = core.LLVMBuildAlloca(s.builder, try s.get_typ_mapping(alloca.alloc_type), "");
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

                const llvm_load = core.LLVMBuildLoad2(s.builder, try s.get_typ_mapping(load.type), llvm_ptr, "");
                try s.tir_llvm_val_map.put(inst_ref, llvm_load);
            },
            .get_element_ptr => |gep| {
                var gep_fields = std.ArrayList(types.LLVMValueRef).init(s.tir.allocator);
                defer gep_fields.deinit();

                const slice = s.tir.extra.items[gep.indeces_start..gep.indeces_end];
                for (slice) |f| {
                    try gep_fields.append(core.LLVMConstInt(core.LLVMInt16Type(), @intCast(f), 0));
                }
                const llvm_agg_ptr = try s.get_inst_mapping(gep.aggregate_ptr);
                const llvm_agg_typ = try s.get_typ_mapping(gep.aggregate_type);

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
            .constant_type => |type_ref| {
                const typ = try tir_type_to_llvm(s, type_ref);
                try s.tir_llvm_typ_map.put(type_ref, typ);
            },
            .constant_val => |val| {
                const llvm_val = try tir_val_to_llvm(s.tir, val);

                try s.tir_llvm_val_map.put(inst_ref, llvm_val);
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

    try generate_llvm_ir_block(&llvm_state, 0, false);

    print("LLVM Generation: \n", .{});
    core.LLVMDumpModule(llvm_state.module);
    // const entry: types.LLVMBasicBlockRef = core.LLVMAppendBasicBlock(main_func, "entry");
    // const builder: types.LLVMBuilderRef = ;
    // core.LLVMPositionBuilderAtEnd(builder, entry);
    // const arg1: types.LLVMValueRef = core.LLVMGetParam(main_func, 0);
    // const arg2: types.LLVMValueRef = core.LLVMGetParam(main_func, 1);
    // const sum: types.LLVMValueRef = core.LLVMBuildAdd(builder, arg1, arg2, "main");
    // _ = core.LLVMBuildRet(builder, sum);

    // Dump the LLVM module to stdout
    // core.LLVMDumpModule(module);

    // Clean up LLVM resources
}
