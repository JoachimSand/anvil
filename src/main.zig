const std = @import("std");
const print = std.debug.print;
const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;
const Token = tokeniser_mod.Token;
const TokenType = tokeniser_mod.TokenType;
const TokenIndex = Tokeniser.Index;

// AST node and its associated data.
// Note that we try to avoid going beyond 16 bytes per node.
// Tag takes up 1 byte, meaning we have at most 3 available
// 32 bit indeces.
const Node = union(enum) {
    // identifier ":" type
    parameter: struct {
        identifier: TokenIndex,
        param_type: Index,
    },

    var_decl_type: struct {
        identifier: TokenIndex,
        decl_type: u32,
    },
    var_decl_expr: struct {
        identifier: TokenIndex,
        decl_expr: u32,
    },
    var_decl_full: struct {
        identifier: TokenIndex,
        decl_type: u32,
        decl_expr: u32,
    },

    fn_decl: struct {},

    block: struct {
        statements: NodeArray,
    },

    add: BinaryExp,
    sub: BinaryExp,

    integer_lit: TokenIndex,

    const Index = u32;
    const ExtraIndex = u32;

    const NodeArray = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    const BinaryExp = struct {
        op_tok: TokenIndex,
        left: Index,
        right: Index,
    };
};

// const Node = struct {
//     // Similar to Zig, the main token associated with this node.
//     main_token: TokenIndex,
//     type: NodeType,

//     const Index = u32;
//     const ExtraIndex = u32;
// };

const ParseError = error{
    UnexpectedToken,
};

const Parser = struct {
    nodes: NodeList,
    const NodeList = std.MultiArrayList(i32);
};

// VarDecl
//   <- Identifier ":" Type ";"
//   / Identifier ":" "=" Expr ";"
//   / Identifier ":" Type "=" Expr ";"
fn parse_var_decl(tokeniser: *Tokeniser, nodes: Parser.NodeList) ParseError!Node.Index {
    const id_tok = tokeniser.next_token();
    const colon_tok = tokeniser.next_token();

    if (id_tok.type != .identifier or colon_tok.type != .colon) {
        return .UnexpectedToken;
    }

    const equal_tok = tokeniser.peek_token();
    if (equal_tok.type != .equal) {
        // TODO: Parse type
        @panic("Type parsing not implement var decl.");
    } else {
        _ = tokeniser.next_token();
        parse_primary_expr(tokeniser, nodes);
    }
}

// PrimaryExpr
//   <- "(" Expr ")"
//   / INTEGER
//   / IfExpr / ContainerLiteral / ContainerDefinition
fn parse_primary_expr(tokeniser: *Tokeniser, nodes: Parser.NodeList) ParseError!Node.Index {
    _ = nodes;
    const primary_tok = tokeniser.next_token();

    switch (primary_tok.type) {
        TokenType.integer_bin, .integer_oct, .integer_hex, .integer_dec => {},
    }
}

fn parse_expr(tokeniser: *Tokeniser) ParseError!Node.Index {
    // Parse an "atom"
    // TODO: Add pre-fix and post-fix parsing
    parse_primary_expr(tokeniser);
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

    var tokeniser = Tokeniser{ .buf = input.items, .cur_pos = 0, .cur_tok_start = 0, .cur_token = null };
    while (true) {
        const tok = tokeniser.next_token() catch return;
        print("Got token type {any} at {any}..{any}\n", .{ tok.type, tok.start, tok.end });
    }

    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    // try bw.flush(); // don't forget to flush!
}
