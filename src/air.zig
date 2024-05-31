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

// Some sort of updated mapping between identifiers and
// instruction index that last wrote to it... Scopes.

const AirInst = union(enum) {
    const Index = u32;
    const List = std.MultiArrayList(AirInst);

    const Br = struct {
        cond: Index,
        then_dst: Index,
        else_dst: Index,
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
    block: struct {
        start: Index,
        end: Index,
    },
    br: AirState.ExtraIndex,
};

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
        switch (s.scopes.items[s.scopes.items.len - 1]) {
            .top => |*top| {
                if (top.get(str_index)) |_| {
                    return error.Shadowing;
                }
                try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
            },
        }
    }

    fn get_var(s: *AirState, var_tok: Token.Index) !Scope.IdentifierInfo {
        const str_index = try intern_token(s, var_tok);
        var scope_i = s.scopes.items.len;
        while (scope_i > 0) {
            scope_i -= 1;
            const scope = s.scopes.items[scope_i];
            switch (scope) {
                .top => |*top| {
                    if (top.get(str_index)) |info| {
                        return info;
                    }
                    // try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
                },
            }
        }
        return error.UndefinedVar;
    }

    fn append_extra_struct(s: *AirState, comptime s_typ: type, val: s_typ) !ExtraIndex {
        const type_info = comptime @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = comptime type_info.Struct;
            const index = s.extra.items.len;
            inline for (s_info.fields) |field| {
                if (field.type == u32) {
                    const field_val = @field(val, field.name);
                    try s.extra.append(field_val);
                } else {
                    _ = try s.append_extra_struct(field.type, @field(val, field.name));
                }
            }
            return @intCast(index);
        } else {
            @panic("Can only append structs of structs/u32s");
        }
    }

    fn append_inst(s: *AirState, instr: AirInst) !AirInst.Index {
        try s.instructions.append(s.allocator, instr);
        return @intCast(s.instructions.len - 1);
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

fn print_air(s: *AirState) !void {
    print("------------ Printing AIR ----------\n", .{});
    for (0..s.instructions.len) |index| {
        print("%{} = ", .{index});
        const inst = s.instructions.get(index);
        switch (inst) {
            .int => |int| {
                print("int({})", .{int});
            },
            .add => |add| {
                print("add(%{}, %{})", .{ add.lhs, add.rhs });
            },
            else => return error.Unimplemented,
        }
        print(";\n", .{});
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
                else => return error.Unimplemented,
            };
            return s.append_inst(inst);
        },
        .identifier => |tok_index| {
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

pub fn air_gen(ast: *Ast) !void {
    var air = AirState{
        .instructions = AirInst.List{},
        .extra = AirState.ExtraList.init(ast.allocator),
        .scopes = AirState.ScopeList.init(ast.allocator),

        .string_index_map = std.StringHashMap(AirState.StringIndex).init(ast.allocator),
        .string_len_map = std.StringHashMap(AirState.StringLen).init(ast.allocator),
        .strings = std.ArrayList(u8).init(ast.allocator),

        .ast = ast,

        .allocator = ast.allocator,
    };
    defer air.deinit();

    const root_node = ast.nodes.items[ast.root].root;

    try pretty_print_mod.print_ast_start(ast, ast.root);

    try air.scopes.append(Scope{ .top = Scope.IdentifierMap.init(ast.allocator) });
    const s_indeces = ast.extra.items[root_node.statements_start..root_node.statements_end];
    for (s_indeces) |s_index| {
        const statement = ast.nodes.items[s_index];

        switch (statement) {
            .var_decl_full => |decl| {
                _ = try air_gen_expr(&air, decl.decl_expr);
            },
            .var_decl_expr => |decl| {
                const inst = try air_gen_expr(&air, decl.decl_expr);

                try air.push_var(decl.identifier, false, inst);

                // try air.identifier_map.put(var_str, inst);
                // air.identifier_map
            },
            else => return error.Unimplemented,
        }
    }

    try print_air(&air);
}
