const std = @import("std");
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;
const TokenFull = tokeniser_mod.TokenFull;
const Token = tokeniser_mod.Token;
const TokenType = tokeniser_mod.TokenType;
const SourceIndex = TokenFull.SourceIndex;

const t = std.zig.Ast;
// AST node and its associated data.
// Note that we try to avoid going beyond 16 bytes per node.
// Tag takes up 1 byte, meaning we have at most 3 available
// 4 byte fields.
const Node = union(enum) {
    // identifier ":" type
    parameter: struct {
        identifier: SourceIndex,
        param_type: Index,
    },

    var_decl_type: struct {
        identifier: SourceIndex,
        decl_type: u32,
    },
    var_decl_expr: struct {
        identifier: SourceIndex,
        decl_expr: u32,
    },
    var_decl_full: struct {
        identifier: SourceIndex,
        decl_type: u32,
        decl_expr: u32,
    },

    fn_decl: struct {},

    block: struct {
        statements: NodeArray,
    },

    add: BinaryExp,
    sub: BinaryExp,

    integer_lit: SourceIndex,

    const Index = u32;
    const ExtraIndex = u32;

    const NodeArray = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    const BinaryExp = struct {
        op_tok: SourceIndex,
        left: Index,
        right: Index,
    };
};

const ParseSpecificError = error{
    UnexpectedToken,
    EarlyTermination,
    Unimplemented,
};

const ParseError = ParseSpecificError || std.mem.Allocator.Error;

const Parser = struct {
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
        if (p.cur_token + 1 < p.tokens.len) {
            const tok = p.tokens.get(p.cur_token);
            p.cur_token += 1;
            return TokenInfo{ .type = tok.type, .index = p.cur_token - 1 };
        } else {
            return error.EarlyTermination;
        }
    }

    fn peek_token(p: *Parser) !TokenInfo {
        if (p.cur_token + 1 < p.tokens.len) {
            const tok = p.tokens.get(p.cur_token);
            return TokenInfo{ .type = tok.type, .index = p.cur_token };
        } else {
            return error.EarlyTermination;
        }
    }

    fn expect_token(p: *Parser) ParseError!TokenInfo {
        if (p.cur_token + 1 < p.tokens.len) {
            const tok = p.tokens.get(p.cur_token);
            p.cur_token += 1;
            return TokenInfo{ .type = tok.type, .index = p.cur_token - 1 };
        } else {
            return error.EarlyTermination;
        }
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

        const expr = try parse_expr(p);
        const node: Node = Node{ .var_decl_expr = .{ .identifier = id_tok.index, .decl_expr = expr } };
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

fn parse_expr(parser: *Parser) ParseError!Node.Index {
    // Parse an "atom"
    // TODO: Add pre-fix and post-fix parsing
    return try parse_primary_expr(parser);
}

fn print_ast(nodes: Parser.NodeList, tokens: Parser.TokenList, cur_node: Parser.NodeIndex) !void {
    print("At node {any} ", .{cur_node});
    switch (nodes.items[cur_node]) {
        .integer_lit => |tok_index| {
            print("Integer literal {any} \n", .{tokens.get(tok_index)});
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

    const stdin = std.io.getStdIn().reader();
    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    try stdin.streamUntilDelimiter(input.writer(), '\n', null);
    print("Got input {s} with length {} \n", .{ input.items, input.items.len });

    // const node_tag = NodeTag{ .param_list = .{ .hello = 10 } };
    // print("{any}\n", .{node_tag});

    var parser = Parser{ .nodes = Parser.NodeList.init(allocator), .tokens = Parser.TokenList{} };
    defer parser.tokens.deinit(allocator);
    defer parser.nodes.deinit();

    var tokeniser = Tokeniser{ .src = input.items, .cur_pos = 0, .cur_tok_start = 0, .cur_token = null };

    while (true) {
        const tok = tokeniser.next_token() catch break;

        try parser.tokens.append(allocator, .{ .type = tok.type, .start = tok.start });
        print("Got token type {any} at {any}..{any}\n", .{ tok.type, tok.start, tok.end });
    }

    const node_id = try parse_var_decl(&parser);

    print("Root node {any}\n", .{parser.nodes.items[node_id]});
    try print_ast(parser.nodes, parser.tokens, node_id);
    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    // try bw.flush(); // don't forget to flush!
}
