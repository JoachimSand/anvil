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
        decl_type: u32,
    },
    var_decl_expr: struct {
        identifier: Parser.TokenIndex,
        decl_expr: u32,
    },
    var_decl_full: struct {
        identifier: Parser.TokenIndex,
        decl_type: u32,
        decl_expr: u32,
    },

    fn_decl: struct {},

    block: struct {
        statements: NodeArray,
    },

    binary_exp: BinaryExp,

    integer_lit: Parser.TokenIndex,

    const Index = u32;
    const ExtraIndex = u32;

    const NodeArray = struct {
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

    const TokenInfo = struct {
        type: TokenType,
        index: TokenIndex,
    };

    fn append_node(p: *Parser, node: Node) !NodeIndex {
        try p.nodes.append(node);
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

    const TokenIndex = u32;
    const TokenList = std.MultiArrayList(Token);
    const NodeIndex = u32;
    const NodeList = std.ArrayList(Node);
};

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

    // f is_last {
    //     print!("└──");
    //     new_prefix.push_str("    ");
    // } else {
    //     print!("├──");
    //     new_prefix.push_str("│  ");
    // }

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

    var parser = Parser{ .src = input.items, .nodes = Parser.NodeList.init(allocator), .tokens = Parser.TokenList{} };
    defer parser.tokens.deinit(allocator);
    defer parser.nodes.deinit();

    var tokeniser = Tokeniser{ .src = input.items, .cur_pos = 0, .cur_tok_start = 0, .cur_token = null };

    while (true) {
        const tok = tokeniser.next_token() catch break;

        try parser.tokens.append(allocator, .{ .type = tok.type, .start = tok.start });
        print("Got token type {any} at {any}..{any}\n", .{ tok.type, tok.start, tok.end });
    }

    const root_id = try parse_var_decl(&parser);

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
