const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Token = tokeniser_mod.Token;
const TokenType = tokeniser_mod.TokenType;

const parser_mod = @import("parser.zig");
const Ast = parser_mod.Ast;
const Node = parser_mod.Node;

const pretty_print_mod = @import("pretty_print.zig");

// Anvil Intermediate Representation

// mut myvar := 2;
// anothervar := myvar
// myvar += 2;
// %1 = int(2);
// %2 = %1;
// %3 = add(2, %1);

// mut myvar := 2;
// if myvar > 3 {
//     another := 4;
//     myvar += another;
// }
// %1 = int(2)
// %2 = int(3)
// %3 = gt(%1, %2)
// %4 = br(%3, blk, blk)
// %5 = block {
//     %6 = int(4);
//     // %7 = as_val(%6)
//     %8 = add(%1, %6)
// }

// BinNode := struct {
// 	a : u32;
// 	b : u32;
// };
// %1 = struct(a : Ref.u32, b : Ref.u32)

// my_type := i32;
// BinNode := struct {
// 	a : my_type;
// 	b : u32;
// };
// %1 = Ref.i32
// %2 = struct(a : %1, b : Ref.u32)

// Node := struct { c : bool};
// BinNode := struct {
// 	a : Node;
// 	b : u32;
// };
// %1 = struct(c : Ref.bool)
// %2 = struct(a : %1, b : Ref.u32)

// BinNode := struct {
// 	a : struct { c : bool};
// 	b : u32;
// };
// %1 = struct { c : bool};
// %2 = struct(a : %1, b : Ref.u32)

// LLVM has ways of operating directly on structs.

// Some sort of updated mapping between identifiers and
// instruction index that last wrote to it... Scopes.

pub const AirInst = union(enum) {
    pub const Index = u32;
    pub const IndexRef = enum(Index) {
        // 2^32 -256
        bool = 4294967040,
        true_lit,
        false_lit,
        i8,
        i16,
        i32,
        i64,

        u8,
        u16,
        u32,
        u64,
        type,
        _,
    };
    const List = std.MultiArrayList(AirInst);
    const ExtraSlice = packed struct {
        start: Air.ExtraIndex,
        end: Air.ExtraIndex,
    };
    pub const DeclInfo = packed struct {
        var_name: Air.StringIndex,
        type_inst: IndexRef,
        mutable: bool,
    };
    pub const BrEither = packed struct {
        cond: IndexRef,
        then_blk: IndexRef,
        else_blk: IndexRef,
    };
    pub const GetElementPtr = packed struct {
        aggregate_ptr: IndexRef,
        fields: ExtraSlice,
    };
    pub const FnDef = packed struct {
        name: Air.StringIndex,
        params: ExtraSlice,
        ret_type: IndexRef,
        blk: IndexRef,
    };
    fn_def: Air.ExtraIndex,
    struct_def: struct {
        start: Air.ExtraIndex,
        end: Air.ExtraIndex,
    },

    block: struct {
        start: IndexRef,
        end: IndexRef,
    },
    br: IndexRef,
    br_cond: struct {
        cond: IndexRef,
        blk: IndexRef,
    },
    br_either: Air.ExtraIndex,

    type_as: struct {
        type: IndexRef,
        expr: IndexRef,
    },
    type_of: IndexRef,
    add: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
    sub: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
    lt: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
    // gt: struct {
    //     lhs: IndexRef,
    //     rhs: IndexRef,
    // },
    get_element_ptr: Air.ExtraIndex,
    alloca: struct {
        type: IndexRef,
    },

    // Like alloca but for arrays
    zero_array: IndexRef,
    load: struct {
        ptr: IndexRef,
    },
    // extract_val: struct {
    //     target: IndexRef,
    //     ptr: IndexRef,
    // },
    store: struct {
        val: IndexRef,
        ptr: IndexRef,
    },
    move: IndexRef,
    address_of: IndexRef,
    indexing: struct {
        target: IndexRef,
        index: IndexRef,
    },
    // Access an argument to the function
    arg: struct { name: Air.StringIndex, type: IndexRef },
    int: u64,
};

const PrimitiveIdMap = std.ComptimeStringMap(AirInst.IndexRef, .{
    .{ "bool", AirInst.IndexRef.bool },
    .{ "true", AirInst.IndexRef.true_lit },
    .{ "false", AirInst.IndexRef.false_lit },
    .{ "i8", AirInst.IndexRef.i8 },
    .{ "i16", AirInst.IndexRef.i16 },
    .{ "i32", AirInst.IndexRef.i32 },
    .{ "i64", AirInst.IndexRef.i64 },
    .{ "u8", AirInst.IndexRef.u8 },
    .{ "u16", AirInst.IndexRef.u16 },
    .{ "u32", AirInst.IndexRef.u32 },
    .{ "u64", AirInst.IndexRef.u64 },
    .{ "type", AirInst.IndexRef.type },
    // .{ "enum", .keyword_enum },
    // .{ "if", .keyword_if },
    // .{ "else", .keyword_else },
    // .{ "mut", .keyword_mut },
    // .{ "or", .keyword_or },
    // .{ "and", .keyword_and },
});

const Scope = union(enum) {
    const IdentifierInfo = struct {
        // Whether new assignments may be made to the identifier
        mutable: bool,
        // Index of Air inst that last wrote to the identifier
        inst: AirInst.IndexRef,
        type_inst: AirInst.IndexRef,
    };
    const IdentifierMap = std.AutoHashMap(Air.StringIndex, IdentifierInfo);
    top: IdentifierMap,
    // func_params: struct { params: IdentifierMap, block: IdentifierMap },
};

const AirSpecificError = error{ Unimplemented, Shadowing, AssignToImm, UndefinedVar };

pub const AirError = AirSpecificError || Allocator.Error || std.fmt.ParseIntError;

pub const Air = struct {
    instructions: AirInst.List,
    extra: ExtraList,

    string_index_map: std.StringHashMap(StringIndex),
    string_len_map: std.AutoHashMap(StringIndex, StringLen),
    strings: std.ArrayList(u8),

    allocator: std.mem.Allocator,

    pub const ExtraIndex = u32;
    pub const ExtraList = std.ArrayList(u32);
    pub const StringLen = u32;
    pub const StringIndex = u32;

    pub fn deinit(air: *Air) void {
        air.instructions.deinit(air.allocator);
        air.extra.deinit();

        air.string_index_map.deinit();
        air.string_len_map.deinit();
        air.strings.deinit();
    }

    fn intern_string(a: *Air, str: []const u8) Allocator.Error!StringIndex {
        const maybe_index = a.string_index_map.get(str);

        if (maybe_index) |index| {
            return index;
        } else {
            const index: StringIndex = @intCast(a.strings.items.len);
            try a.strings.appendSlice(str);
            try a.string_index_map.put(str, index);
            try a.string_len_map.put(index, @intCast(str.len));
            return index;
        }
    }

    pub fn get_string(a: *Air, index: StringIndex) []const u8 {
        const len = a.string_len_map.get(index).?;
        return a.strings.items[index .. index + len];
    }

    fn append_extra_struct(a: *Air, comptime s_typ: type, val: s_typ) Allocator.Error!Air.ExtraIndex {
        const type_info = comptime @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = comptime type_info.Struct;
            const index = a.extra.items.len;
            inline for (s_info.fields) |field| {
                if (field.type == AirInst.IndexRef) {
                    const field_val = @field(val, field.name);
                    try a.extra.append(@intFromEnum(field_val));
                } else if (field.type == u32) {
                    const field_val = @field(val, field.name);
                    try a.extra.append(field_val);
                } else if (field.type == bool) {
                    const field_val = @field(val, field.name);
                    try a.extra.append(@intFromBool(field_val));
                } else {
                    _ = try a.append_extra_struct(field.type, @field(val, field.name));
                }
            }
            return @intCast(index);
        } else {
            @panic("Can only append structs of structs/u32s");
        }
    }

    pub fn get_extra_struct(a: *Air, comptime s_typ: type, extra_index: Node.ExtraIndex) s_typ {
        const type_info = @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = type_info.Struct;
            var s: s_typ = undefined;

            comptime var offset = 0;
            inline for (s_info.fields) |field| {
                const field_val = a.extra.items[extra_index + offset];
                if (field.type == AirInst.IndexRef) {
                    @field(s, field.name) = @enumFromInt(field_val);
                    offset += 1;
                } else if (field.type == u32) {
                    @field(s, field.name) = field_val;
                    offset += 1;
                } else if (field.type == bool) {
                    @field(s, field.name) = (field_val != 0);
                    offset += 1;
                } else {
                    const field_struct = a.get_extra_struct(field.type, extra_index + offset);
                    @field(s, field.name) = field_struct;
                    offset += @typeInfo(field.type).Struct.fields.len;
                }
            }
            return s;
        } else {
            @panic("Can only retrieve structs of structs/u32s");
        }
    }
};

pub const AirState = struct {
    scratch: std.ArrayList(u32),

    air: Air,

    scopes: ScopeList,
    ast: *Ast,

    pub const ScopeList = std.ArrayList(Scope);

    fn get_air(a: *AirState) Air {
        return a.air;
    }

    fn intern_token(s: *AirState, index: Token.Index) Allocator.Error!Air.StringIndex {
        const tok = s.ast.tokens.get(index);
        const var_str = tokeniser_mod.token_to_str(tok, s.ast.src);
        return s.air.intern_string(var_str);
    }

    fn push_var(s: *AirState, var_tok: Token.Index, mutable: bool, inst: AirInst.IndexRef, type_inst: AirInst.IndexRef) AirError!void {
        const str_index = try s.intern_token(var_tok);

        print("Pushing identifier {s}\n", .{tokeniser_mod.token_to_str(s.ast.tokens.get(var_tok), s.ast.src)});
        switch (s.scopes.items[s.scopes.items.len - 1]) {
            .top => |*top| {
                if (top.get(str_index)) |_| {
                    return error.Shadowing;
                }
                try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst, .type_inst = type_inst });
            },
            // .func_params => |*func_scope| {
            //     const param_scope = func_scope.params;
            //     const blk_scope = func_scope.block;
            //     if (param_scope.get(str_index)) |_| {
            //         return error.Shadowing;
            //     }
            //     if (blk_scope.get(str_index)) |_| {
            //         return error.Shadowing;
            //     }
            //     try func_scope.block.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
            // },
        }
    }

    fn get_var(s: *AirState, var_tok: Token.Index) AirError!*Scope.IdentifierInfo {
        const str_index = try s.intern_token(var_tok);
        var scope_i = s.scopes.items.len;
        while (scope_i > 0) {
            scope_i -= 1;
            const scope = s.scopes.items[scope_i];
            switch (scope) {
                .top => |*top| {
                    if (top.getPtr(str_index)) |info| {
                        return info;
                    }
                    // try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
                },
                // .func_params => |*func_scope| {
                //     const param_scope = func_scope.params;
                //     const blk_scope = func_scope.block;
                //     if (param_scope.getPtr(str_index)) |info| {
                //         return info;
                //     }
                //     if (blk_scope.getPtr(str_index)) |info| {
                //         return info;
                //     }
                //     // try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
                // },
            }
        }
        print("Undefined identifier {s}\n", .{tokeniser_mod.token_to_str(s.ast.tokens.get(var_tok), s.ast.src)});
        return error.UndefinedVar;
    }

    fn append_scratch_struct(s: *AirState, comptime s_typ: type, val: s_typ) Allocator.Error!Air.ExtraIndex {
        const type_info = comptime @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = comptime type_info.Struct;
            const index = s.air.extra.items.len;
            inline for (s_info.fields) |field| {
                if (field.type == AirInst.IndexRef) {
                    const field_val = @field(val, field.name);
                    try s.scratch.append(@intFromEnum(field_val));
                } else if (field.type == u32) {
                    const field_val = @field(val, field.name);
                    try s.scratch.append(field_val);
                } else if (field.type == bool) {
                    const field_val = @field(val, field.name);
                    try s.scratch.append(@intFromBool(field_val));
                } else {
                    _ = try s.append_scratch_struct(field.type, @field(val, field.name));
                }
            }
            return @intCast(index);
        } else {
            @panic("Can only append structs of structs/u32s");
        }
    }

    fn pop_scratch_to_extra(s: *AirState, count: usize) !AirInst.ExtraSlice {
        print("\nPopping {} to extra, scratch has length {} \n", .{ count, s.scratch.items.len });
        const elems = s.scratch.items[s.scratch.items.len - count ..];

        const start: u32 = @intCast(s.air.extra.items.len);
        try s.air.extra.appendSlice(elems);
        const end: u32 = @intCast(s.air.extra.items.len);

        s.scratch.items.len -= count;
        return .{ .start = start, .end = end };
    }

    fn append_inst(s: *AirState, instr: AirInst) !AirInst.IndexRef {
        try s.air.instructions.append(s.air.allocator, instr);
        return @enumFromInt(s.air.instructions.len - 1);
    }

    fn deinit(s: *AirState) void {
        std.debug.assert(s.scratch.items.len == 0);
        s.scratch.deinit();

        for (0..s.scopes.items.len) |index| {
            switch (s.scopes.items[index]) {
                .top => |*top| top.deinit(),
                // .func_params => |*func_scope| {
                //     func_scope.params.deinit();
                //     func_scope.block.deinit();
                // },
            }
        }
        s.scopes.deinit();
    }
};

pub fn print_air(a: *Air, start: u32, stop: u32, indent: u32) !void {
    // print("------------ Printing AIR ----------\n", .{});
    // print("Instruction count : {}\n", .{s.instructions.len});

    var index: u32 = start;
    while (index < stop) {
        for (0..indent) |_| {
            print("    ", .{});
        }

        print("%{} = ", .{index});
        const inst = a.instructions.get(index);
        switch (inst) {
            .int => |int| {
                print("int({})", .{int});
            },
            .move => |move| {
                print("move({})", .{move});
            },
            .address_of => |ad| {
                print("address of ({})", .{ad});
            },
            .indexing => |id| {
                print("indexing ({}, {})", .{ id.target, id.index });
            },
            .zero_array => |zero| {
                print("zero array with type {}", .{zero});
            },
            .add => |add| {
                print("add(%{}, %{})", .{ add.lhs, add.rhs });
            },
            .lt => |lt| {
                print("lt(%{} < %{})", .{ lt.lhs, lt.rhs });
            },
            // .gt => |gt| {
            //     print("gt(%{} > %{})", .{ gt.lhs, gt.rhs });
            // },
            .type_as => |type_as| {
                print("type_as(%{}, %{})", .{ type_as.type, type_as.expr });
            },
            .type_of => |type_of| {
                print("type_of(%{})", .{type_of});
            },
            .br => |br| {
                print("br %{}", .{br});
            },
            .br_cond => |br| {
                print("br_cond %{}, %{}", .{ br.cond, br.blk });
            },
            .br_either => |br_extra| {
                const br = a.get_extra_struct(AirInst.BrEither, br_extra);
                print("br_either %{}, (%{} else %{})", .{ br.cond, br.then_blk, br.else_blk });
            },

            .block => |blk| {
                print("block(%{d}, %{d}){{\n", .{ @intFromEnum(blk.start), @intFromEnum(blk.end) });
                try print_air(a, @intFromEnum(blk.start) + 1, @intFromEnum(blk.end) + 1, indent + 1);
                index = @intFromEnum(blk.end);

                for (0..indent) |_| {
                    print("    ", .{});
                }
                print("}}", .{});
            },
            .struct_def => |def| {
                const type_info = @typeInfo(AirInst.DeclInfo);
                const field_count: Air.ExtraIndex = @intCast(type_info.Struct.fields.len);
                print("struct def (", .{});
                var extra = def.start;
                while (extra < def.end) {
                    const field_info = a.get_extra_struct(AirInst.DeclInfo, extra);
                    const var_name = a.get_string(field_info.var_name);
                    print("{s} : %{}, ", .{ var_name, field_info.type_inst });

                    extra += field_count;
                }
                print(")", .{});
            },
            .arg => |arg| {
                print("arg({s}, {})", .{ a.get_string(arg.name), arg.type });
            },
            .alloca => |alloc| {
                print("alloca {} ", .{alloc.type});
            },
            .get_element_ptr => |extra_index| {
                const get_element_ptr = a.get_extra_struct(AirInst.GetElementPtr, extra_index);

                print("get_element_ptr {}, (", .{get_element_ptr.aggregate_ptr});
                var cur_field = get_element_ptr.fields.start;
                while (cur_field <= get_element_ptr.fields.end) : (cur_field += 1) {
                    const field_str_index: Air.StringIndex = a.extra.items[cur_field];
                    const field_name = a.get_string(field_str_index);
                    print("{s}, ", .{field_name});
                }
                print(")", .{});
            },
            .load => |load| {
                print("load({})", .{load.ptr});
            },
            .store => |store| {
                print("store({}, {})", .{ store.val, store.ptr });
            },
            .fn_def => |fn_extra| {
                const fn_def = a.get_extra_struct(AirInst.FnDef, fn_extra);
                const fn_name = a.get_string(fn_def.name);
                print("fn {s}(...)", .{fn_name});

                // const params = a.extra.items[fn_def.params.start..fn_def.params.end];
                // for (params) |param| {
                //     const arg = a.instructions.get(param).arg;
                //     const var_name = a.get_string(arg.name);
                //     print("{s} : %{}, ", .{ var_name, arg.type });
                // }
                print("-> {} at blk %{d}", .{ fn_def.ret_type, @intFromEnum(fn_def.blk) });
            },
            else => {
                print("AIR print unimplemented for {}", .{inst});
                return error.Unimplemented;
            },
        }
        print(";\n", .{});
        index += 1;
    }
}

fn air_gen_decl_info_list(s: *AirState, d_indeces: []const Node.Index) AirError!AirInst.ExtraSlice {
    // const start_extra: AirState.ExtraIndex = @intCast(s.extra.items.len);
    // var end_extra: AirState.ExtraIndex = undefined;
    var count: Air.ExtraIndex = 0;
    for (d_indeces) |d_index| {
        const decl = s.ast.nodes.items[d_index];
        switch (decl) {
            .var_decl_full, .mut_var_decl_full, .var_decl_expr, .mut_var_decl_expr => return error.Unimplemented,
            .mut_var_decl_type, .var_decl_type => |type_decl| {
                // TODO: PUSH SCOPE TO ALLOW REFERRING TO OTHER IDENTIFIERS IN STRUCT
                const decl_name = try s.intern_token(type_decl.identifier);
                const type_inst = try air_gen_expr(s, type_decl.decl_type);

                var field_info: AirInst.DeclInfo = undefined;
                if (decl == .mut_var_decl_type) {
                    field_info = AirInst.DeclInfo{ .var_name = decl_name, .mutable = true, .type_inst = type_inst };
                } else {
                    field_info = AirInst.DeclInfo{ .var_name = decl_name, .mutable = false, .type_inst = type_inst };
                }
                _ = try s.append_scratch_struct(AirInst.DeclInfo, field_info);
                print("Scratch len: {}\n", .{s.scratch.items.len});
                count += 1;
            },
            // .mut_var_decl_type => return error.Unimplemented,
            else => unreachable,
        }
    }
    print("Encounterd {} decl in list \n", .{count});
    const field_count: Air.ExtraIndex = @intCast(@typeInfo(AirInst.DeclInfo).Struct.fields.len);
    return s.pop_scratch_to_extra(field_count * count);
}

fn air_gen_struct_def(s: *AirState, d_indeces: []const Node.Index) AirError!AirInst.IndexRef {
    const decl_list = try air_gen_decl_info_list(s, d_indeces);
    const inst = AirInst{ .struct_def = .{ .start = decl_list.start, .end = decl_list.end } };
    return s.append_inst(inst);
}

fn air_gen_expr(s: *AirState, index: Node.Index) AirError!AirInst.IndexRef {
    print("AIR Expr gen for the following node: \n", .{});
    try pretty_print_mod.print_ast_start(s.ast, index);

    const cur_node = s.ast.nodes.items[index];
    switch (cur_node) {
        .struct_definition_one => |s_def| {
            // const d_indeces: []const Node.Index = ;
            return air_gen_struct_def(s, (&s_def.statement)[0..1]);
        },
        .struct_definition => |s_def| {
            const d_indeces = s.ast.extra.items[s_def.statements_start..s_def.statements_end];
            return air_gen_struct_def(s, d_indeces);
        },
        .container_literal => |container| {
            const container_type = try air_gen_expr(s, container.target_type);
            const alloc_inst = try s.append_inst(AirInst{ .alloca = .{ .type = container_type } });

            //

            var assign_index = container.assignments_start;
            while (assign_index < container.assignments_end) : (assign_index += 1) {
                const cur_assign = s.ast.nodes.items[s.ast.extra.items[assign_index]];

                print("Curassign {}\n", .{cur_assign});
                if (cur_assign != .assignment) {
                    return error.Unimplemented;
                }
                const assign_node = cur_assign.assignment;
                const target_type = s.ast.nodes.items[assign_node.target];
                if (target_type != .identifier) {
                    return error.Unimplemented;
                }

                const field_name = try s.intern_token(target_type.identifier);
                print("Field name {s}\n", .{s.air.get_string(field_name)});
                const start_and_end: Air.ExtraIndex = @intCast(s.air.extra.items.len);
                try s.air.extra.append(field_name);
                const get_element = AirInst.GetElementPtr{ .aggregate_ptr = alloc_inst, .fields = .{ .start = start_and_end, .end = start_and_end } };
                const get_element_extra = try s.air.append_extra_struct(AirInst.GetElementPtr, get_element);
                const get_elem_inst = try s.append_inst(AirInst{ .get_element_ptr = get_element_extra });

                const expr_inst = try air_gen_expr(s, assign_node.expr);

                // To get the type of the field, load in the value and perform a type-of operation.
                const load_elem_inst = try s.append_inst(.{ .load = .{ .ptr = get_elem_inst } });
                const type_inst = try s.append_inst(.{ .type_of = load_elem_inst });
                // target_type_inst = target_info.type_inst;

                const type_as_inst = try s.append_inst(AirInst{ .type_as = .{ .type = type_inst, .expr = expr_inst } });

                _ = try s.append_inst(.{ .store = .{ .val = type_as_inst, .ptr = get_elem_inst } });
            }
            return alloc_inst;
        },
        .binary_exp => |bin_exp| {
            const bin_tok = s.ast.tokens.get(bin_exp.op_tok);
            const lhs_index = try air_gen_expr(s, bin_exp.lhs);
            const rhs_index = try air_gen_expr(s, bin_exp.rhs);

            const inst = switch (bin_tok.type) {
                .plus => AirInst{ .add = .{ .lhs = lhs_index, .rhs = rhs_index } },
                .l_arrow => AirInst{ .lt = .{ .lhs = lhs_index, .rhs = rhs_index } },
                .r_arrow => AirInst{ .lt = .{ .lhs = rhs_index, .rhs = lhs_index } },
                else => return error.Unimplemented,
            };
            return s.append_inst(inst);
        },
        .prefix_exp => |prefix| {
            const prefix_target = try air_gen_expr(s, prefix.target);
            switch (prefix.token.type) {
                .keyword_move => return s.append_inst(.{ .move = prefix_target }),
                else => return error.Unimplemented,
            }
        },
        .ref => |ref| {
            const ref_target = try air_gen_expr(s, ref.target);
            return s.append_inst(.{ .address_of = ref_target });
        },
        .indexing => |indexing| {
            const index_expr = try air_gen_expr(s, indexing.index);
            const target = try air_gen_expr(s, indexing.target);
            return s.append_inst(.{ .indexing = .{ .target = target, .index = index_expr } });
        },
        .identifier => |tok_index| {
            const id_str = s.ast.get_tok_str(tok_index);
            print("Id str {s}\n", .{id_str});
            if (PrimitiveIdMap.get(id_str)) |prim_index| {
                return prim_index;
            }
            const info = try s.get_var(tok_index);
            if (info.mutable) {
                const def_inst = s.air.instructions.get(@intFromEnum(info.inst));
                if (def_inst == .alloca) {
                    const load_inst = s.append_inst(.{ .load = .{ .ptr = info.inst } });
                    return load_inst;
                } else if (def_inst == .zero_array) {
                    return info.inst;
                } else {
                    unreachable;
                }
            } else {
                return info.inst;
            }
        },
        .integer_lit => |tok_index| {
            const lit_str = tokeniser_mod.token_to_str(s.ast.tokens.get(tok_index), s.ast.src);
            const int_val = try std.fmt.parseInt(u64, lit_str, 0);
            const inst = AirInst{ .int = int_val };
            return s.append_inst(inst);
        },
        else => {
            print("AIR generatio for the following node is unimplemented: \n", .{});
            try pretty_print_mod.print_ast_start(s.ast, index);
            return error.Unimplemented;
        },
    }
}

fn air_gen_decl(s: *AirState, id: Token.Index, mutable: bool, type_node: ?Node.Index, expr: ?Node.Index) !void {
    var expr_inst: AirInst.IndexRef = undefined;
    if (expr) |expr_node| {
        expr_inst = try air_gen_expr(s, expr_node);
    } else if (type_node) |typ| {
        // We allow type-only declarations for arrays - these are zero initialized.
        const node = s.ast.nodes.items[typ];
        if (node == .indexing) {
            const type_inst = try air_gen_expr(s, typ);
            const zero_inst = try s.append_inst(.{ .zero_array = type_inst });
            if (mutable) {
                // const alloc_inst = try s.append_inst(.{ .alloc = .{ .type = type_inst } });
                // _ = try s.append_inst(.{ .store = .{ .ptr = alloc_inst, .val = zero_inst } });
                try s.push_var(id, mutable, zero_inst, type_inst);
            } else {
                try s.push_var(id, mutable, zero_inst, type_inst);
            }
            return;
        } else {
            return error.Unimplemented;
        }
    }

    var type_inst: AirInst.IndexRef = undefined;
    if (type_node) |node| {
        type_inst = try air_gen_expr(s, node);
    } else {
        type_inst = try s.append_inst(.{ .type_of = expr_inst });
    }
    const type_as_inst = try s.append_inst(.{ .type_as = .{ .type = type_inst, .expr = expr_inst } });

    // allocate stack memory for mutable declarations
    if (mutable) {
        const alloc_inst = try s.append_inst(.{ .alloca = .{ .type = type_inst } });
        _ = try s.append_inst(.{ .store = .{ .ptr = alloc_inst, .val = type_as_inst } });
        try s.push_var(id, mutable, alloc_inst, type_inst);
    } else {
        try s.push_var(id, mutable, type_as_inst, type_inst);
    }
}

fn start_new_block(s: *AirState) !AirInst.Index {
    const start_inst: AirInst.IndexRef = @enumFromInt(s.air.instructions.len);
    return @intFromEnum(try s.append_inst(AirInst{ .block = .{ .start = start_inst, .end = undefined } }));
}

fn end_block(s: *AirState, block: AirInst.Index) void {
    const end_inst: AirInst.IndexRef = @enumFromInt(s.air.instructions.len - 1);
    var cur_blk = s.air.instructions.get(block);
    cur_blk.block.end = end_inst;
    s.air.instructions.set(block, cur_blk);
}

// fn air_gen_container_field_assignment(
//     s: *AirState,
//     assign: Node.Assignment,
// ) !void {
//     const expr_inst = try air_gen_expr(s, assign.expr);

//     const target_node = s.ast.nodes.items[assign.target];
//     if (target_node != .identifier) {
//         unreachable;
//     }
//     const id = target_node.identifier;

//     var target_ptr_inst: AirInst.IndexRef = undefined;
//     var target_type_inst: AirInst.IndexRef = undefined;
// }

fn air_gen_statements(s: *AirState, s_indeces: []const Node.Index, start_block: AirInst.Index) AirError!void {
    var cur_block_inst = start_block;
    for (s_indeces) |s_index| {
        const statement = s.ast.nodes.items[s_index];
        print("AIR gen for statement {}\n", .{statement});
        switch (statement) {
            .block => |_| _ = try air_gen_scoped_block(s, s_index, true),
            .block_one => |_| _ = try air_gen_scoped_block(s, s_index, true),

            .var_decl_full => |decl| try air_gen_decl(s, decl.identifier, false, decl.decl_type, decl.decl_expr),
            .mut_var_decl_full => |decl| try air_gen_decl(s, decl.identifier, true, decl.decl_type, decl.decl_expr),
            .var_decl_expr => |decl| try air_gen_decl(s, decl.identifier, false, null, decl.decl_expr),
            .mut_var_decl_expr => |decl| try air_gen_decl(s, decl.identifier, true, null, decl.decl_expr),

            // Only arrays currently support this syntax.
            .var_decl_type => |decl| try air_gen_decl(s, decl.identifier, false, decl.decl_type, null),
            .mut_var_decl_type => |decl| try air_gen_decl(s, decl.identifier, true, decl.decl_type, null),

            // .fn_decl => |fn_decl| {
            //     const fn_name = try s.intern_token(fn_decl.identifier);
            //     const
            // },
            .fn_decl_params => |fn_extra| {
                const fn_decl = s.ast.get_extra_struct(Node.FnDeclParams, fn_extra);
                const fn_name = try s.intern_token(fn_decl.identifier);
                const reserved_air_index = try s.append_inst(undefined);
                try s.scopes.append(Scope{ .top = Scope.IdentifierMap.init(s.air.allocator) });

                const param_indeces = s.ast.extra.items[fn_decl.params.start..fn_decl.params.end];

                var count: Air.ExtraIndex = 0;
                for (param_indeces) |param_index| {
                    const decl = s.ast.nodes.items[param_index];
                    switch (decl) {
                        .var_decl_full, .mut_var_decl_full, .var_decl_expr, .mut_var_decl_expr => return error.Unimplemented,
                        .var_decl_type => |type_decl| {
                            const decl_name = try s.intern_token(type_decl.identifier);
                            const type_inst = try air_gen_expr(s, type_decl.decl_type);

                            const arg_inst = AirInst{ .arg = .{ .name = decl_name, .type = type_inst } };
                            const arg_inst_index = try s.append_inst(arg_inst);

                            try s.push_var(type_decl.identifier, false, arg_inst_index, type_inst);

                            // Push the insts of each arg to scratch so that we can later pop them into extra
                            // as a list than the fn def can reference.
                            _ = try s.scratch.append(@intFromEnum(arg_inst_index));
                            count += 1;
                        },
                        .mut_var_decl_type => return error.Unimplemented,
                        else => unreachable,
                    }
                }
                const param_slice = try s.pop_scratch_to_extra(count);
                // const param_slice = try air_gen_decl_info_list(s, param_indeces, true);

                const ret_type = try air_gen_expr(s, fn_decl.ret_type);

                const blk = try air_gen_scoped_block(s, fn_decl.block, false);

                const fn_def = AirInst.FnDef{ .name = fn_name, .params = param_slice, .ret_type = ret_type, .blk = blk };
                const inst = AirInst{ .fn_def = try s.air.append_extra_struct(AirInst.FnDef, fn_def) };
                s.air.instructions.set(@intFromEnum(reserved_air_index), inst);
            },

            .assignment => |assign| {
                const expr_inst = try air_gen_expr(s, assign.expr);

                const target_node = s.ast.nodes.items[assign.target];
                var target_ptr_inst: AirInst.IndexRef = undefined;
                var target_type_inst: AirInst.IndexRef = undefined;
                switch (target_node) {
                    .identifier => |id| {
                        const target_info = try s.get_var(id);

                        if (target_info.mutable == false) {
                            return error.AssignToImm;
                        }
                        target_type_inst = target_info.type_inst;
                        target_ptr_inst = try s.append_inst(.{ .load = .{ .ptr = target_info.inst } });
                    },
                    .field_access => |access| {
                        var cur_node = target_node;
                        const field_names_start: Air.ExtraIndex = @intCast(s.air.extra.items.len);
                        while (cur_node == .field_access) {
                            const field_name = try s.intern_token(access.field_id);
                            _ = try s.air.extra.append(field_name);

                            cur_node = s.ast.nodes.items[cur_node.field_access.target];
                        }
                        const field_names_end: Air.ExtraIndex = @intCast(s.air.extra.items.len - 1);
                        if (cur_node != .identifier) {
                            return error.Unimplemented;
                        }
                        const id = cur_node.identifier;
                        const target_info = try s.get_var(id);

                        if (target_info.mutable == false) {
                            return error.AssignToImm;
                        }
                        const element_ptr = AirInst.GetElementPtr{ .aggregate_ptr = target_info.inst, .fields = .{ .start = field_names_start, .end = field_names_end } };
                        const extra_index = try s.air.append_extra_struct(AirInst.GetElementPtr, element_ptr);
                        const get_elem_inst = try s.append_inst(AirInst{ .get_element_ptr = extra_index });
                        target_ptr_inst = get_elem_inst;

                        // To get the type of the field, load in the value and perform a type-of operation.
                        const load_elem_inst = try s.append_inst(.{ .load = .{ .ptr = get_elem_inst } });
                        target_type_inst = try s.append_inst(.{ .type_of = load_elem_inst });
                        target_type_inst = target_info.type_inst;
                    },

                    else => return error.Unimplemented,
                }

                const assign_tok = s.ast.tokens.get(assign.token);
                var new_inst = expr_inst;
                if (assign_tok.type != .equal) {
                    const assign_inst = switch (assign_tok.type) {
                        .plus_equal => AirInst{ .add = .{ .lhs = target_ptr_inst, .rhs = expr_inst } },
                        else => return error.Unimplemented,
                    };
                    new_inst = try s.append_inst(assign_inst);
                }
                const type_as_inst = try s.append_inst(AirInst{ .type_as = .{ .type = target_type_inst, .expr = new_inst } });

                _ = try s.append_inst(.{ .store = .{ .val = type_as_inst, .ptr = target_ptr_inst } });

                // target_info.inst = type_as_inst;
            },
            .while_statement => |while_statement| {
                const prev_block_br = try s.append_inst(undefined);
                end_block(s, cur_block_inst);

                // Conditional block
                const cond_blk = try start_new_block(s);
                s.air.instructions.set(@intFromEnum(prev_block_br), .{ .br = @enumFromInt(cond_blk) });
                const cond_inst = try air_gen_expr(s, while_statement.condition);
                var cond_br = AirInst.BrEither{ .cond = cond_inst, .then_blk = undefined, .else_blk = undefined };
                const cond_br_index = try s.append_inst(undefined);

                end_block(s, cond_blk);

                // Loop body block
                const while_block = try air_gen_scoped_block(s, while_statement.block, true);
                _ = try s.append_inst(.{ .br = @enumFromInt(cond_blk) });
                end_block(s, @intFromEnum(while_block));

                // Block after loop
                cur_block_inst = try start_new_block(s);

                // Finally, write the contents of the conditional branch to either the loop body or the block after.
                cond_br.then_blk = while_block;
                cond_br.else_blk = @enumFromInt(cur_block_inst);
                const cond_br_inst = AirInst{ .br_either = try s.air.append_extra_struct(AirInst.BrEither, cond_br) };
                s.air.instructions.set(@intFromEnum(cond_br_index), cond_br_inst);
            },
            .if_else_statement => |if_else| {
                const cond_inst = try air_gen_expr(s, if_else.condition);

                const br_inst = try s.append_inst(undefined);
                end_block(s, cur_block_inst);

                const then_blk = try air_gen_scoped_block(s, if_else.block, true);
                const else_br_end = try s.append_inst(undefined);
                end_block(s, @intFromEnum(then_blk));

                const else_blk = try air_gen_scoped_block(s, if_else.else_block, true);
                const then_br_end = try s.append_inst(undefined);
                end_block(s, @intFromEnum(else_blk));

                const br_struct = AirInst.BrEither{ .cond = cond_inst, .then_blk = then_blk, .else_blk = else_blk };
                const br = try s.air.append_extra_struct(AirInst.BrEither, br_struct);

                // Block after if-else
                cur_block_inst = try start_new_block(s);

                s.air.instructions.set(@intFromEnum(br_inst), .{ .br_either = br });
                s.air.instructions.set(@intFromEnum(else_br_end), .{ .br = @enumFromInt(cur_block_inst) });
                s.air.instructions.set(@intFromEnum(then_br_end), .{ .br = @enumFromInt(cur_block_inst) });
            },
            else => {
                print("Statement {} is unimplemented\n", .{statement});
                return error.Unimplemented;
            },
        }
    }
    end_block(s, cur_block_inst);
}

fn air_gen_scoped_block(s: *AirState, n_index: Node.Index, new_scope: bool) !AirInst.IndexRef {
    print("AIR gen for block {}\n", .{n_index});
    if (new_scope) {
        try s.scopes.append(Scope{ .top = Scope.IdentifierMap.init(s.air.allocator) });
    }

    var s_indeces: []const Node.Index = undefined;
    const node = s.ast.nodes.items[n_index];
    switch (node) {
        .root => |root| {
            s_indeces = s.ast.extra.items[root.statements_start..root.statements_end];
        },
        .block => |block| {
            s_indeces = s.ast.extra.items[block.statements_start..block.statements_end];
        },
        .block_one => |*block| {
            s_indeces = (&block.statement)[0..1];
        },
        else => return error.Unimplemented,
    }
    const blk_inst = try start_new_block(s);

    _ = try air_gen_statements(s, s_indeces, blk_inst);

    if (new_scope) {
        var old_scope = s.scopes.pop();
        old_scope.top.deinit();
    }

    return @enumFromInt(blk_inst);
}

pub fn air_gen(ast: *Ast) !Air {
    var s = AirState{
        .air = Air{
            .instructions = AirInst.List{},
            .extra = Air.ExtraList.init(ast.allocator),

            .string_index_map = std.StringHashMap(Air.StringIndex).init(ast.allocator),
            .string_len_map = std.AutoHashMap(Air.StringIndex, Air.StringLen).init(ast.allocator),
            .strings = std.ArrayList(u8).init(ast.allocator),

            .allocator = ast.allocator,
        },
        .scratch = std.ArrayList(u32).init(ast.allocator),

        .scopes = AirState.ScopeList.init(ast.allocator),

        .ast = ast,
    };
    defer s.deinit();

    // const root_node = ast.nodes.items[ast.root].root;
    _ = try air_gen_scoped_block(&s, ast.root, true);
    try print_air(&s.air, 0, @intCast(s.air.instructions.len), 0);

    return s.get_air();
}
