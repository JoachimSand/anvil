const std = @import("std");
const print = std.debug.print;

const llvm = @import("llvm");
const target = llvm.target;
const target_machine = llvm.target_machine;
const types = llvm.types;
const core = llvm.core;

const tir_mod = @import("tir.zig");
const Type = tir_mod.Type;
const Tir = tir_mod.Tir;
const TirInst = tir_mod.TirInst;
const type_is_ref = tir_mod.type_is_ref;

const LLVMState = struct { tir: *Tir, module: types.LLVMModuleRef, builder: types.LLVMBuilderRef };

fn tir_type_to_llvm(tir: *Tir, ref: Type.IndexRef) !types.LLVMTypeRef {
    switch (ref) {
        .tir_boolean => return core.LLVMInt1Type(),
        .tir_i8, .tir_u8 => return core.LLVMInt8Type(),
        .tir_i16, .tir_u16 => return core.LLVMInt16Type(),
        .tir_i32, .tir_u32 => return core.LLVMInt32Type(),
        .tir_i64, .tir_u64 => return core.LLVMInt64Type(),
        .tir_unknown_int => return core.LLVMInt64Type(),
        .tir_void => return core.LLVMVoidType(),
        .tir_stackref, .tir_typ, .tir_own, .tir_ref => return error.Unimplemented,
        _ => {
            // const tir_type = tir.types.get(@intFromEnum(ref));
            _ = tir;
            return error.Unimplemented;
        },
    }
}

pub fn generate_llvm_ir_block(s: *LLVMState, blk_index: TirInst.Index) !void {
    const blk = s.tir.instructions.get(blk_index).block;

    const instructions = s.tir.instructions.slice();
    var inst_index = blk.start;
    while (inst_index < blk.end) : (inst_index += 1) {
        // const inst_index: TirInst.Index = @intCast(inst_index_usize);
        const instruction = s.tir.instructions.get(inst_index);

        switch (instruction) {
            .fn_def => |fn_def| {
                var llvm_params = std.ArrayList(types.LLVMTypeRef).init(s.tir.allocator);
                defer llvm_params.deinit();

                const tir_params = s.tir.extra.items[fn_def.params.start..fn_def.params.end];
                for (tir_params) |param_inst_index| {
                    const param_inst = instructions.get(param_inst_index).arg;
                    const llvm_param_type = try tir_type_to_llvm(s.tir, param_inst.typ_ref);
                    try llvm_params.append(llvm_param_type);
                }

                // var params: [2]types.LLVMTypeRef = [_]types.LLVMTypeRef{
                //     core.LLVMInt32Type(),
                //     core.LLVMInt32Type(),
                // };

                const func_type: types.LLVMTypeRef = core.LLVMFunctionType(core.LLVMInt32Type(), llvm_params.items.ptr, @intCast(llvm_params.items.len), 0);
                const func: types.LLVMValueRef = core.LLVMAddFunction(s.module, "main", func_type);
                core.LLVMSetLinkage(func, types.LLVMLinkage.LLVMExternalLinkage);
                const entry: types.LLVMBasicBlockRef = core.LLVMAppendBasicBlock(func, "entry");
                core.LLVMPositionBuilderAtEnd(s.builder, entry);
            },
            .arg => continue,
            .block => |new_blk| {
                try generate_llvm_ir_block(s, inst_index);
                inst_index = new_blk.end;
            },
            .alloca => |alloca| {
                // const u8_index: u8 = @intCast(inst_index);
                // var buf: [4:0]u8 = .{ u8_index % 10 + 48, 0, 0, 0 };
                _ = core.LLVMBuildAlloca(s.builder, try tir_type_to_llvm(s.tir, alloca.alloc_type), "");
            },
            // .store => |store| {
            //     store.ptr
            // },
            .constant_type => continue,
            .constant_val => continue,
            else => {
                print("LLVM Generation unimplemented for: \n", .{});
                try tir_mod.print_tir(s.tir, inst_index, inst_index + 1, 0);

                print("LLVM Generation so far: \n", .{});
                core.LLVMDumpModule(s.module);
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
        .module = core.LLVMModuleCreateWithName("anvil_module"),
        .builder = core.LLVMCreateBuilder(),
    };
    defer core.LLVMDisposeBuilder(llvm_state.builder);
    defer core.LLVMDisposeModule(llvm_state.module);
    defer core.LLVMShutdown();

    try generate_llvm_ir_block(&llvm_state, 0);

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
