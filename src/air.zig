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

// Some sort of updated mapping between identifiers and
// instruction index that last wrote to it...

const AirState = struct {
    instructions: AirInst.List,

    string_index_map: std.StringHashMap(StringIndex),
    string_len_map: std.StringHashMap(StringLen),
    strings: std.ArrayList(u8),

    allocator: std.mem.Allocator,

    pub const StringLen = u32;
    pub const StringIndex = u32;

    fn intern_string(a: *AirState, str: []const u8) StringIndex {
        const maybe_index = a.string_index_map.get(str);

        if (maybe_index) |index| {
            return index;
        } else {
            const index = a.strings.items.len;
            a.strings.appendSlice(str);
            a.string_index_map.put(str, index);
            a.string_len_map.put(str, str.len);
            return index;
        }
    }

    fn get_string(s: *AirState, index: StringIndex) []const u8 {
        const len = s.string_lengths[index];
        return s.strings.items[index .. index + len];
    }

    fn append_inst(s: *AirState, instr: AirInst) !AirInst.Index {
        try s.instructions.append(s.allocator, instr);
        return @intCast(s.instructions.len - 1);
    }

    fn deinit(s: *AirState) void {
        s.instructions.deinit(s.allocator);
        s.string_index_map.deinit();
        s.string_len_map.deinit();
        s.strings.deinit();
    }
};

const AirInst = union(enum) {
    const Index = u32;
    const List = std.MultiArrayList(AirInst);

    int: u64,
    add: struct {
        lhs: Index,
        rhs: Index,
    },
    sub: struct {
        lhs: Index,
        rhs: Index,
    },
};

fn air_gen_expr(ast: *Ast, s: *AirState, index: Node.Index) !AirInst.Index {
    const cur_node = ast.nodes.items[index];
    switch (cur_node) {
        .binary_exp => |bin_exp| {
            const bin_tok = ast.tokens.get(bin_exp.op_tok);
            const lhs_index = try air_gen_expr(ast, s, bin_exp.lhs);
            const rhs_index = try air_gen_expr(ast, s, bin_exp.rhs);

            const inst = switch (bin_tok.type) {
                .plus => AirInst{ .add = .{ .lhs = lhs_index, .rhs = rhs_index } },
                else => return error.Unimplemented,
            };
            return s.append_inst(inst);
        },
        .integer_lit => |tok_index| {
            const lit_str = tokeniser_mod.token_to_str(ast.tokens.get(tok_index), ast.src);
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
        .string_index_map = std.StringHashMap(AirState.StringIndex).init(ast.allocator),
        .string_len_map = std.StringHashMap(AirState.StringLen).init(ast.allocator),
        .strings = std.ArrayList(u8).init(ast.allocator),

        .allocator = ast.allocator,
    };
    defer air.deinit();

    const root_node = ast.nodes.items[ast.root].root;

    try pretty_print_mod.print_ast_start(ast, ast.root);

    const s_indeces = ast.extra.items[root_node.statements_start..root_node.statements_end];
    for (s_indeces) |s_index| {
        const statement = ast.nodes.items[s_index];

        switch (statement) {
            .var_decl_full => |decl| {
                _ = try air_gen_expr(ast, &air, decl.decl_expr);
            },
            .var_decl_expr => |decl| {
                _ = try air_gen_expr(ast, &air, decl.decl_expr);
            },
            else => return error.Unimplemented,
        }
    }
}
