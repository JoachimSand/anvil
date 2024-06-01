const std = @import("std");
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

// LLVM has ways of operating directly on structs.

// Some sort of updated mapping between identifiers and
// instruction index that last wrote to it... Scopes.

const AirInst = union(enum) {
    const Index = enum(u32) {
        // 2^32 -256
        bool = 4294967040,
        true_lit,
        false_lit,
        i8,

        _,
    };
    const List = std.MultiArrayList(AirInst);

    // const BlockInfo = struct { start_inst: AirInst.Index, end_inst: AirInst.Index };

    const Br = struct {
        cond: Index,
        then_blk: Index,
        else_blk: Index,
    };

    int: u64,
    add: struct {
        lhs: Index,
        rhs: Index,
    },
    sub: struct {
        lhs: Index,
        rhs: Index,
    },
    lt: struct {
        lhs: Index,
        rhs: Index,
    },
    gt: struct {
        lhs: Index,
        rhs: Index,
    },
    block: struct {
        start: Index,
        end: Index,
    },

    type_as: struct {
        type: Index,
        expr: Index,
    },

    br: AirState.ExtraIndex,
};

const PrimitiveIdMap = std.ComptimeStringMap(AirInst.Index, .{
    .{ "bool", AirInst.Index.bool },
    .{ "true", AirInst.Index.true_lit },
    .{ "false", AirInst.Index.false_lit },
    .{ "i8", AirInst.Index.i8 },
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
        inst: AirInst.Index,
    };
    const IdentifierMap = std.AutoHashMap(AirState.StringIndex, IdentifierInfo);
    top: IdentifierMap,
};

const AirState = struct {
    instructions: AirInst.List,
    extra: ExtraList,

    string_index_map: std.StringHashMap(StringIndex),
    string_len_map: std.StringHashMap(StringLen),
    strings: std.ArrayList(u8),

    scopes: ScopeList,
    ast: *Ast,

    allocator: std.mem.Allocator,

    pub const ExtraIndex = u32;
    pub const ExtraList = std.ArrayList(ExtraIndex);
    pub const StringLen = u32;
    pub const StringIndex = u32;
    pub const ScopeList = std.ArrayList(Scope);

    fn intern_token(a: *AirState, index: Token.Index) !StringIndex {
        const tok = a.ast.tokens.get(index);
        const var_str = tokeniser_mod.token_to_str(tok, a.ast.src);
        return a.intern_string(var_str);
    }

    fn intern_string(a: *AirState, str: []const u8) !StringIndex {
        const maybe_index = a.string_index_map.get(str);

        if (maybe_index) |index| {
            return index;
        } else {
            const index: StringIndex = @intCast(a.strings.items.len);
            try a.strings.appendSlice(str);
            try a.string_index_map.put(str, index);
            try a.string_len_map.put(str, @intCast(str.len));
            return index;
        }
    }

    fn get_string(s: *AirState, index: StringIndex) []const u8 {
        const len = s.string_lengths[index];
        return s.strings.items[index .. index + len];
    }

    fn push_var(s: *AirState, var_tok: Token.Index, mutable: bool, inst: AirInst.Index) !void {
        const str_index = try intern_token(s, var_tok);

        print("Pushing identifier {s}\n", .{tokeniser_mod.token_to_str(s.ast.tokens.get(var_tok), s.ast.src)});
        switch (s.scopes.items[s.scopes.items.len - 1]) {
            .top => |*top| {
                if (top.get(str_index)) |_| {
                    return error.Shadowing;
                }
                try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
            },
        }
    }

    fn get_var(s: *AirState, var_tok: Token.Index) !*Scope.IdentifierInfo {
        const str_index = try intern_token(s, var_tok);
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
            }
        }
        print("Undefined identifier {s}\n", .{tokeniser_mod.token_to_str(s.ast.tokens.get(var_tok), s.ast.src)});
        return error.UndefinedVar;
    }

    fn append_extra_struct(s: *AirState, comptime s_typ: type, val: s_typ) !ExtraIndex {
        const type_info = comptime @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = comptime type_info.Struct;
            const index = s.extra.items.len;
            inline for (s_info.fields) |field| {
                if (field.type == AirInst.Index) {
                    const field_val = @field(val, field.name);
                    try s.extra.append(@intFromEnum(field_val));
                } else {
                    _ = try s.append_extra_struct(field.type, @field(val, field.name));
                }
            }
            return @intCast(index);
        } else {
            @panic("Can only append structs of structs/u32s");
        }
    }

    pub fn get_extra_struct(a: *AirState, comptime s_typ: type, extra_index: Node.ExtraIndex) s_typ {
        const type_info = @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = type_info.Struct;
            var s: s_typ = undefined;

            comptime var offset = 0;
            inline for (s_info.fields) |field| {
                if (field.type == AirInst.Index) {
                    const field_val = a.extra.items[extra_index + offset];
                    @field(s, field.name) = @enumFromInt(field_val);
                    offset += 1;
                } else {
                    const field_val = a.get_extra_struct(field.type, extra_index + offset);
                    @field(s, field.name) = field_val;
                    offset += @typeInfo(field.type).Struct.fields.len;
                }
            }
            return s;
        } else {
            @panic("Can only retrieve structs of structs/u32s");
        }
    }

    fn append_inst(s: *AirState, instr: AirInst) !AirInst.Index {
        try s.instructions.append(s.allocator, instr);
        return @enumFromInt(s.instructions.len - 1);
    }

    fn deinit(s: *AirState) void {
        s.instructions.deinit(s.allocator);
        s.extra.deinit();
        s.string_index_map.deinit();
        s.string_len_map.deinit();
        s.strings.deinit();
        for (0..s.scopes.items.len) |index| {
            switch (s.scopes.items[index]) {
                .top => |*top| top.deinit(),
            }
        }
        s.scopes.deinit();
    }
};

fn print_air(s: *AirState, start: u32, stop: u32, indent: u32) !void {
    // print("------------ Printing AIR ----------\n", .{});
    // var res: u32 = 0;
    comptime var one: u32 = 1;
    const two = 2;
    const res: u32 = one + two;
    one = res;
    // _ = res;
    // const res2 = res;
    // print("Res {s}\n", .{@typeName(@TypeOf(res))});

    var index: u32 = start;
    while (index < stop) {
        for (0..indent) |_| {
            print("    ", .{});
        }

        print("%{} = ", .{index});
        const inst = s.instructions.get(index);
        switch (inst) {
            .int => |int| {
                print("int({})", .{int});
            },
            .add => |add| {
                print("add(%{}, %{})", .{ add.lhs, add.rhs });
            },
            .type_as => |type_as| {
                print("type_as(%{}, %{})", .{ type_as.type, type_as.expr });
            },
            .br => |br_extra| {
                const br = s.get_extra_struct(AirInst.Br, br_extra);
                print("br(%{}, %{}, %{})", .{ br.cond, br.then_blk, br.else_blk });
            },
            .block => |blk| {
                print("block(%{d}, %{d}){{\n", .{ @intFromEnum(blk.start), @intFromEnum(blk.end) });
                try print_air(s, @intFromEnum(blk.start) + 1, @intFromEnum(blk.end) + 1, indent + 1);
                index = @intFromEnum(blk.end);

                for (0..indent) |_| {
                    print("    ", .{});
                }
                print("}}", .{});
            },
            else => return error.Unimplemented,
        }
        print(";\n", .{});
        index += 1;
    }
}

fn air_gen_expr(s: *AirState, index: Node.Index) !AirInst.Index {
    const cur_node = s.ast.nodes.items[index];
    switch (cur_node) {
        .binary_exp => |bin_exp| {
            const bin_tok = s.ast.tokens.get(bin_exp.op_tok);
            const lhs_index = try air_gen_expr(s, bin_exp.lhs);
            const rhs_index = try air_gen_expr(s, bin_exp.rhs);

            const inst = switch (bin_tok.type) {
                .plus => AirInst{ .add = .{ .lhs = lhs_index, .rhs = rhs_index } },
                .l_arrow => AirInst{ .lt = .{ .lhs = lhs_index, .rhs = rhs_index } },
                .r_arrow => AirInst{ .gt = .{ .lhs = lhs_index, .rhs = rhs_index } },
                else => return error.Unimplemented,
            };
            return s.append_inst(inst);
        },
        .identifier => |tok_index| {
            const id_str = s.ast.get_tok_str(tok_index);
            if (PrimitiveIdMap.get(id_str)) |prim_index| {
                return prim_index;
            }
            const info = try s.get_var(tok_index);
            return info.inst;
        },
        .integer_lit => |tok_index| {
            const lit_str = tokeniser_mod.token_to_str(s.ast.tokens.get(tok_index), s.ast.src);
            const int_val = try std.fmt.parseInt(u64, lit_str, 0);
            const inst = AirInst{ .int = int_val };
            return s.append_inst(inst);
        },
        else => return error.Unimplemented,
    }
}

fn air_gen_decl(s: *AirState, id: Token.Index, mutable: bool, type_node: ?Node.Index, expr: Node.Index) !void {
    var inst = try air_gen_expr(s, expr);
    if (type_node) |node| {
        const type_inst = try air_gen_expr(s, node);
        inst = try s.append_inst(.{ .type_as = .{ .type = type_inst, .expr = inst } });
    }
    try s.push_var(id, mutable, inst);
}

fn air_gen_block(s: *AirState, n_index: Node.Index) !AirInst.Index {
    print("AIR gen for block {}\n", .{n_index});
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
    const start_inst: AirInst.Index = @enumFromInt(s.instructions.len);
    const blk_inst = try s.append_inst(AirInst{ .block = .{ .start = undefined, .end = undefined } });

    for (s_indeces) |s_index| {
        const statement = s.ast.nodes.items[s_index];
        print("AIR gen for statement {}\n", .{statement});
        switch (statement) {
            .block => |_| _ = try air_gen_block(s, s_index),
            .block_one => |_| _ = try air_gen_block(s, s_index),

            .var_decl_full => |decl| try air_gen_decl(s, decl.identifier, false, decl.decl_type, decl.decl_expr),
            .mut_var_decl_full => |decl| try air_gen_decl(s, decl.identifier, true, decl.decl_type, decl.decl_expr),
            .var_decl_expr => |decl| try air_gen_decl(s, decl.identifier, false, null, decl.decl_expr),
            .mut_var_decl_expr => |decl| try air_gen_decl(s, decl.identifier, true, null, decl.decl_expr),

            .var_decl_type => |_| return error.Unimplemented,
            .mut_var_decl_type => |_| return error.Unimplemented,

            .assignment => |assign| {
                const expr_inst = try air_gen_expr(s, assign.expr);

                const target_node = s.ast.nodes.items[assign.target];
                const target_info = try switch (target_node) {
                    .identifier => |id| s.get_var(id),
                    else => return error.Unimplemented,
                };

                if (target_info.mutable == false) {
                    return error.AssignToImm;
                }

                const assign_tok = s.ast.tokens.get(assign.token);
                var new_inst = expr_inst;
                if (assign_tok.type != .equal) {
                    const assign_inst = switch (assign_tok.type) {
                        .plus_equal => AirInst{ .add = .{ .lhs = target_info.inst, .rhs = expr_inst } },
                        else => return error.Unimplemented,
                    };
                    new_inst = try s.append_inst(assign_inst);
                }
                target_info.inst = new_inst;
            },
            .if_else_statement => |if_else| {
                const cond_inst = try air_gen_expr(s, if_else.condition);

                const br_inst = try s.append_inst(undefined);

                const then_blk = try air_gen_block(s, if_else.block);
                const else_blk = try air_gen_block(s, if_else.else_block);

                const br_struct = AirInst.Br{ .cond = cond_inst, .then_blk = then_blk, .else_blk = else_blk };
                const br = try s.append_extra_struct(AirInst.Br, br_struct);

                s.instructions.set(@intFromEnum(br_inst), .{ .br = br });
                // const then_blk = try air.append_inst(AirInst { .block = .{.start = air.inst}})
            },
            else => {
                print("Statement {} is unimplemented\n", .{statement});
                return error.Unimplemented;
            },
        }
    }
    const end_inst: AirInst.Index = @enumFromInt(s.instructions.len - 1);
    s.instructions.set(@intFromEnum(blk_inst), AirInst{ .block = .{ .start = start_inst, .end = end_inst } });

    return blk_inst;
}

pub fn air_gen(ast: *Ast) !void {
    var s = AirState{
        .instructions = AirInst.List{},
        .extra = AirState.ExtraList.init(ast.allocator),
        .scopes = AirState.ScopeList.init(ast.allocator),

        .string_index_map = std.StringHashMap(AirState.StringIndex).init(ast.allocator),
        .string_len_map = std.StringHashMap(AirState.StringLen).init(ast.allocator),
        .strings = std.ArrayList(u8).init(ast.allocator),

        .ast = ast,

        .allocator = ast.allocator,
    };
    defer s.deinit();
    try s.scopes.append(Scope{ .top = Scope.IdentifierMap.init(ast.allocator) });

    // const root_node = ast.nodes.items[ast.root].root;
    _ = try air_gen_block(&s, ast.root);
    try print_air(&s, 0, @intCast(s.instructions.len), 0);
}
