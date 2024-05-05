const std = @import("std");
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;
const TokenFull = tokeniser_mod.TokenFull;
const Token = tokeniser_mod.Token;
const TokenType = tokeniser_mod.TokenType;
// const SourceIndex = tokeniser_mod.SourceIndex;
const token_to_str = tokeniser_mod.token_to_str;

const unicode = std.unicode;

// AST node and its associated data.
// Note that we try to avoid going beyond 16 bytes per node.
// Tag takes up 1 byte, meaning we have at most 3 available
// 4 byte fields.
const Node = union(enum) {
    // identifier ":" type
    parameter: struct {
        identifier: Parser.TokenIndex,
        param_type: Index,
    },

    var_decl_type: struct {
        identifier: Parser.TokenIndex,
        decl_type: Index,
    },
    var_decl_expr: struct {
        identifier: Parser.TokenIndex,
        decl_expr: u32,
    },
    var_decl_full: struct {
        identifier: Parser.TokenIndex,
        decl_type: Index,
        decl_expr: Index,
    },

    // No params, no ret type, no dependencies
    fn_decl: struct {
        identifier: Parser.TokenIndex,
        block: Index,
    },
    // Specified return type, no parameters/deps
    fn_decl_type: struct {
        identifier: Parser.TokenIndex,
        ret_type: Index,
        block: Index,
    },

    // Single param, no return type/deps
    fn_decl_param: struct {
        identifier: Parser.TokenIndex,
        param: Index,
        block: Index,
    },

    block: struct {
        start_brace: Parser.TokenIndex,
        statements: NodeIndexSlice,
    },
    block_one: struct {
        start_brace: Parser.TokenIndex,
        statement: Index,
    },
    empty_block: struct {
        start_brace: Parser.TokenIndex,
    },

    binary_exp: BinaryExp,

    integer_lit: Parser.TokenIndex,

    const Index = u32;
    const ExtraIndex = u32;

    const FnDeclFull = packed struct {
        identifier: Parser.TokenIndex,
        params: NodeIndexSlice,
        dependencies: Index,
        ret_type: Index,
        block: Index,
    };

    // Multiple parameters
    const FnDeclParams = packed struct {
        identifier: Parser.TokenIndex,
        params: NodeIndexSlice,
        block: Index,
    };

    // Multiple parameters and specified return type
    const FnDeclParamsType = packed struct {
        identifier: Parser.TokenIndex,
        params: NodeIndexSlice,
        ret_type: Index,
        block: Index,
    };

    // Dependencies and return type
    // TODO: Disallow this in the grammar? Seems useless...
    const FnDeclDepsType = packed struct {
        identifier: Parser.TokenIndex,
        params: NodeIndexSlice,
        ret_type: Index,
        block: Index,
    };

    const NodeIndexSlice = packed struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    const BinaryExp = struct {
        op_tok: Parser.TokenIndex,
        lhs: Index,
        rhs: Index,
    };
};

const ParseSpecificError = error{
    UnexpectedToken,
    EarlyTermination,
    Unimplemented,
};

const ParseError = ParseSpecificError || std.mem.Allocator.Error;

const Parser = struct {
    src: []const u8,

    nodes: NodeList,
    tokens: TokenList,
    cur_token: TokenIndex = 0,

    extra: std.ArrayList(u32),
    // Used for scratch allocations e.g. indeces for each statement in a block.
    // Scratch allocations must be freed after use, in effect making this a LIFO queue.
    scratch: NodeIndexList,

    const TokenIndex = u32;
    const TokenList = std.MultiArrayList(Token);
    const NodeIndex = u32;
    const NodeList = std.ArrayList(Node);
    const NodeIndexList = std.ArrayList(NodeIndex);

    const TokenInfo = struct {
        type: TokenType,
        index: TokenIndex,
    };

    fn append_node(p: *Parser, node: Node) !NodeIndex {
        try p.nodes.append(node);
        return @intCast(p.nodes.items.len - 1);
    }

    fn append_nodes(p: *Parser, nodes: []const Node) !NodeIndex {
        try p.nodes.appendSlice(nodes);
        return @intCast(p.nodes.items.len - 1);
    }

    fn next_token(p: *Parser) ParseError!TokenInfo {
        if (p.cur_token < p.tokens.len) {
            const tok = p.tokens.get(p.cur_token);
            p.cur_token += 1;
            return TokenInfo{ .type = tok.type, .index = p.cur_token - 1 };
        } else {
            return error.EarlyTermination;
        }
    }

    fn expect_token(p: *Parser, tok_type: TokenType) ParseError!TokenInfo {
        const tok = try p.next_token();

        if (tok.type != tok_type) {
            print("Expected token type {any}, got {any}\n", .{ tok_type, tok.type });
            return error.UnexpectedToken;
        } else {
            return tok;
        }
    }

    fn peek_token(p: *Parser) !TokenInfo {
        if (p.cur_token < p.tokens.len) {
            const tok = p.tokens.get(p.cur_token);
            return TokenInfo{ .type = tok.type, .index = p.cur_token };
        } else {
            return error.EarlyTermination;
        }
    }

    fn get_tok_str(p: *Parser, tok_index: TokenIndex) []const u8 {
        return token_to_str(p.tokens.get(tok_index), p.src);
    }
};
// Block <- "{" Statement* "}"

// Expressions evaluate to a value
// Statements may evaluate to a control-flow effect
// Statement
//   <- Decl / Assignment / Block / IfStatement

fn parse_block(p: *Parser) ParseError!Node.Index {
    const l_brace = try p.expect_token(.l_brace);

    // TODO: Better error reporting regardings brace like in ember
    var next_tok = try p.peek_token();
    var statements: ?[]Node.Index = null;
    while (next_tok.type != .r_brace) {
        switch (next_tok.type) {
            .keyword_fn => try p.scratch.append(try parse_fn_decl(p)),
            .l_brace => try p.scratch.append(try parse_block(p)),
            else => return error.Unimplemented,
        }

        if (statements) |*s| {
            s.len += 1;
        } else {
            statements = p.scratch.items[p.scratch.items.len - 1 ..];
        }

        next_tok = try p.peek_token();

        // return error.Unimplemented;
    }
    _ = try p.next_token();

    var block_node: Node = undefined;
    if (statements) |*s| {
        if (s.len == 1) {
            block_node = Node{ .block_one = .{ .start_brace = l_brace.index, .statement = s.*[0] } };
        } else {
            const start: u32 = @intCast(p.extra.items.len);
            try p.extra.appendSlice(s.*);
            const end: u32 = @intCast(p.extra.items.len);
            block_node = Node{ .block = .{ .start_brace = l_brace.index, .statements = .{ .start = start, .end = end } } };
        }
    } else {
        block_node = Node{ .empty_block = .{ .start_brace = l_brace.index } };
    }

    return p.append_node(block_node);
}

// Decl
//   <- VarDecl
//   / "fn" Identifier "(" (Parameter "," )* Parameter? ")" ("->" Type)? Block // Function Declaration
fn parse_fn_decl(p: *Parser) ParseError!Node.Index {
    _ = try p.expect_token(.keyword_fn);
    const id_tok = try p.expect_token(.identifier);

    _ = try p.expect_token(.l_paren);
    _ = try p.expect_token(.r_paren);

    const maybe_arrow = try p.peek_token();

    if (maybe_arrow.type == .minus_arrow) {
        return error.Unimplemented;
    }

    const block = try parse_block(p);

    const node = Node{ .fn_decl = .{ .identifier = id_tok.index, .block = block } };
    return p.append_node(node);
}

// VarDecl
//   <- Identifier ":" Type ";"
//   / Identifier ":" "=" Expr ";"
//   / Identifier ":" Type "=" Expr ";"
fn parse_var_decl(p: *Parser) ParseError!Node.Index {
    const id_tok = try p.next_token();
    const colon_tok = try p.next_token();

    if (id_tok.type != .identifier or colon_tok.type != .colon) {
        return error.UnexpectedToken;
    }

    const equal_tok = try p.peek_token();
    if (equal_tok.type != .equal) {
        // TODO: Parse type
        @panic("Type parsing not implement var decl.");
    } else {
        _ = try p.next_token();

        const expr = try parse_expr(p, 0);
        const node: Node = Node{ .var_decl_expr = .{ .identifier = id_tok.index, .decl_expr = expr } };
        _ = try p.expect_token(.semicolon);

        return try p.append_node(node);
    }

    return error.Unimplemented;
}

// PrimaryExpr
//   <- "(" Expr ")"
//   / INTEGER
//   / IfExpr / ContainerLiteral / ContainerDefinition
fn parse_primary_expr(p: *Parser) ParseError!Node.Index {
    const primary_tok = try p.next_token();

    switch (primary_tok.type) {
        .integer_bin, .integer_oct, .integer_hex, .integer_dec => {
            const node: Node = Node{ .integer_lit = primary_tok.index };
            return try p.append_node(node);
        },
        else => return error.Unimplemented,
    }
    return error.Unimplemented;
}

// 1: BoolOrExpr <- BoolAndExpr ("or" BoolAndExpr)*
// 2: BoolAndExpr <- CompareExpr ("and" CompareExpr)*
// 3: CompareExpr <- BitwiseExpr (("==" / "!=" / ">=" / "<=" / ">" / "<") BitwiseExpr)?
// 4: BitwiseExpr <- BitShiftExpr (("&" / "|" / "^" ) BitShiftExpr)*
// 5: BitShiftExpr <- AdditionExpr (("<<" / ">>") AdditionExpr)*
// 6: AdditionExpr <- MultiplyExpr (("+" / "-") MultiplyExpr)*
// 7: MultiplyExpr <- PrefixExpr (("*" / "/") PrefixExpr)*

const Precedence = u4;
inline fn operator_precedence(token_type: TokenType) ?Precedence {
    switch (token_type) {
        .keyword_or => return 1,
        .keyword_and => return 2,
        .plus, .minus => return 6,
        .asterisk, .slash => return 7,

        else => return null,
    }
}

fn parse_expr(p: *Parser, start_prec: Precedence) ParseError!Node.Index {
    // Parse an "atom"/left hand side of expression
    // TODO: Add pre-fix and post-fix parsing
    var lhs = try parse_primary_expr(p);
    var maybe_op = try p.peek_token();

    var cur_prec = start_prec;

    while (operator_precedence(maybe_op.type)) |op_prec| {
        if (op_prec <= cur_prec) {
            return lhs;
        }

        const op_token = try p.next_token();
        print("op: {any}\n", .{op_token});

        const rhs = try parse_expr(p, op_prec);
        const op_node = Node{ .binary_exp = Node.BinaryExp{ .op_tok = op_token.index, .lhs = lhs, .rhs = rhs } };

        lhs = try p.append_node(op_node);
        maybe_op = try p.peek_token();
    }

    return lhs;
}

fn print_ast(p: *Parser, prefix: *std.ArrayList(u8), is_last: bool, cur_node: Parser.NodeIndex) !void {
    // print("At node {any} ", .{cur_node});
    print("{s}", .{prefix.items});
    var pre_str: []const u8 = "";
    if (is_last) {
        print("└──", .{});
        pre_str = "    ";
        try prefix.appendSlice(pre_str);
    } else {
        print("├──", .{});
        pre_str = "│  ";
        try prefix.appendSlice(pre_str);
    }
    defer prefix.items.len -= pre_str.len;

    switch (p.nodes.items[cur_node]) {
        // Base cases
        .integer_lit => |tok_index| {
            const str = p.get_tok_str(tok_index);
            print("Integer literal {s} \n", .{str});
        },

        .binary_exp => |bin_exp| {
            const op_str = p.get_tok_str(bin_exp.op_tok);
            print("{s}\n", .{op_str});
            try print_ast(p, prefix, false, bin_exp.lhs);
            try print_ast(p, prefix, true, bin_exp.rhs);
        },

        .var_decl_expr => |decl| {
            const id_str = p.get_tok_str(decl.identifier);
            print("Var. decl for {s} \n", .{id_str});
            try print_ast(p, prefix, true, decl.decl_expr);
        },

        .fn_decl => |decl| {
            const id_str = p.get_tok_str(decl.identifier);
            print("fn {s} \n", .{id_str});
            try print_ast(p, prefix, true, decl.block);
        },

        .empty_block => {
            print("Empty block\n", .{});
        },

        .block_one => |block| {
            print("Block\n", .{});
            try print_ast(p, prefix, true, block.statement);
        },

        .block => |block| {
            print("Block\n", .{});

            const statements = p.extra.items[block.statements.start..block.statements.end];
            for (statements, 0..) |statement, index| {
                if (index == block.statements.end - 1) {
                    try print_ast(p, prefix, true, statement);
                } else {
                    try print_ast(p, prefix, false, statement);
                }
            }
        },

        else => return error.Unimplemented,
    }
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    print("All your {s} are belong to us.\n", .{"codebase"});

    print("Size of node: {any}\n", .{@sizeOf(Node)});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    var read_from_stdin = true;
    var args = std.process.args();
    while (args.next()) |arg| {
        if (std.mem.eql(u8, "-f", arg)) {
            read_from_stdin = false;
        }
    }

    if (read_from_stdin) {
        const stdin = std.io.getStdIn().reader();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);
        print("Got input {s} with length {} \n", .{ input.items, input.items.len });
    } else {
        print("Reading from file test.anv\n", .{});
        var file = try std.fs.cwd().openFile("test.anv", .{});
        defer file.close();

        _ = try file.reader().readAllArrayList(&input, 10000);
    }

    // const node_tag = NodeTag{ .param_list = .{ .hello = 10 } };
    // print("{any}\n", .{node_tag});

    var parser = Parser{ .src = input.items, .nodes = Parser.NodeList.init(allocator), .tokens = Parser.TokenList{}, .extra = std.ArrayList(u32).init(allocator), .scratch = Parser.NodeIndexList.init(allocator) };
    defer parser.tokens.deinit(allocator);
    defer parser.nodes.deinit();
    defer parser.extra.deinit();
    defer parser.scratch.deinit();

    var tokeniser = Tokeniser{ .src = input.items, .cur_pos = 0, .cur_tok_start = 0, .cur_token = null };

    while (true) {
        const tok = tokeniser.next_token() catch break;

        try parser.tokens.append(allocator, .{ .type = tok.type, .start = tok.start });
        print("Got token type {any} at {any}..{any}\n", .{ tok.type, tok.start, tok.end });
    }

    const root_id = try parse_fn_decl(&parser);

    print("Root node {any}\n", .{parser.nodes.items[root_id]});
    var prefix = std.ArrayList(u8).init(allocator);
    defer prefix.deinit();

    print("Root\n", .{});
    try print_ast(&parser, &prefix, true, root_id);
    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    // try bw.flush(); // don't forget to flush!
}
