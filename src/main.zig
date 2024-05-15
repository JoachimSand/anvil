const std = @import("std");
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;
const TokenFull = tokeniser_mod.TokenFull;
const Token = tokeniser_mod.Token;
const TokenType = tokeniser_mod.TokenType;
// const SourceIndex = tokeniser_mod.SourceIndex;
const token_to_str = tokeniser_mod.token_to_str;

const assert = std.debug.assert;
const unicode = std.unicode;

// AST node and its associated data.
// Note that we try to avoid going beyond 16 bytes per node.
// Tag takes up 1 byte, meaning we have at most 3 available
// 4 byte fields.
const Node = union(enum) {
    // Specified return type, no parameters/deps
    fn_decl: struct {
        identifier: Parser.TokenIndex,
        ret_type: Index,
        block: Index,
    },

    // Multiple params
    // Index to FnDeclParams
    fn_decl_params: ExtraIndex,

    // Index to FnDeclFull
    fn_decl_full: ExtraIndex,

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

    // TODO: For future error reporting we would like to store the location of the if_token
    if_statement: struct {
        condition: Index,
        block: Index,
    },
    if_statement_capture: struct {
        capture_ref: Index,
        condition: Index,
        block: Index,
    },
    if_else_statement: struct {
        condition: Index,
        block: Index,
        else_block: Index,
    },
    if_else_statement_capture: ExtraIndex,

    assignment: struct {
        token: Parser.TokenIndex,
        target: Index,
        expr: Index,
    },

    block: struct {
        start_brace: Parser.TokenIndex,
        statements_start: Index,
        statements_end: Index,
    },
    block_one: struct {
        start_brace: Parser.TokenIndex,
        statement: Index,
    },
    block_empty: struct {
        start_brace: Parser.TokenIndex,
    },

    // TODO: Struct and enum definition nodes can both be extended
    // by a single field
    struct_definition: struct {
        struct_keyword: Parser.TokenIndex,
        statements_start: Index,
        statements_end: Index,
    },
    struct_definition_one: struct {
        struct_keyword: Parser.TokenIndex,
        statement: Index,
    },
    struct_definition_empty: struct {
        struct_keyword: Parser.TokenIndex,
    },

    enum_definition: struct {
        enum_keyword: Parser.TokenIndex,
        statements_start: Index,
        statements_end: Index,
    },
    enum_definition_one: struct {
        enum_keyword: Parser.TokenIndex,
        statement: Index,
    },
    enum_definition_empty: struct {
        enum_keyword: Parser.TokenIndex,
    },

    container_literal: struct {
        target_type: Index,
        assignments_start: Index,
        assignments_end: Index,
    },
    container_literal_one: struct {
        target_type: Index,
        assignment: Index,
    },
    container_literal_empty: struct {
        target_type: Index,
    },

    field_access: struct { target: Index, field_id: Parser.TokenIndex },

    binary_exp: BinaryExp,

    prefix_exp: Prefix,

    // Consider creating an explicit struct type for references
    ref: struct {
        ref_tok: Parser.TokenInfo,
        target: Index,
    },
    ref_mut: struct {
        ref_tok: Parser.TokenInfo,
        target: Index,
    },

    fn_call_empty: struct {
        target: Index,
        start_paren: Parser.TokenIndex,
    },
    fn_call_single: struct {
        target: Index,
        arg: Index,
        start_paren: Parser.TokenIndex,
    },
    fn_call_full: ExtraIndex,

    deref: struct {
        target: Index,
        token: Parser.TokenInfo,
    },
    // field_access: struct {
    //     token: Parser.TokenIndex,
    //     target: Index,
    //     field_name: Parser.TokenIndex,
    // },
    identifier: Parser.TokenIndex,
    integer_lit: Parser.TokenIndex,

    const Index = u32;
    const ExtraIndex = u32;

    const Prefix = struct {
        target: Index,
        token: Parser.TokenInfo,
    };

    // Multiple parameters
    const FnDeclParams = packed struct {
        identifier: Parser.TokenIndex,
        params: IndexSlice,
        ret_type: Index,
        block: Index,
    };

    const FnDeclFull = packed struct {
        identifier: Parser.TokenIndex,
        params: IndexSlice,
        dependencies: Index,
        ret_type: Index,
        block: Index,
    };

    // Dependencies and return type
    // TODO: Disallow this in the grammar? Seems useless...
    const FnDeclDepsType = packed struct {
        identifier: Parser.TokenIndex,
        params: IndexSlice,
        ret_type: Index,
        block: Index,
    };

    const FnCallFull = struct {
        target: Index,
        args: IndexSlice,
        start_paren: Parser.TokenIndex,
    };

    const IfElseCapture = struct {
        condition: Index,
        capture_ref: Index,
        block: Index,
        else_block: Index,
    };

    const IndexSlice = packed struct {
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
    ParamInit,
    BlockNotAllowed,
    Unimplemented,
};

const ParseError = ParseSpecificError || std.mem.Allocator.Error;

const Parser = struct {
    src: []const u8,

    nodes: NodeList,
    tokens: TokenList,
    cur_token: TokenIndex = 0,

    // TODO: Make this a memory allocator instead.
    // Would need to figure out how to neatly store
    // pointers to arrays in Node for this to work.
    // Since pointers are 64 bit, this would be tough.
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
            print("Next {any} \n", .{tok});
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
            print("Peeked {any} \n", .{tok});
            return TokenInfo{ .type = tok.type, .index = p.cur_token };
        } else {
            return error.EarlyTermination;
        }
    }

    fn get_tok_str(p: *Parser, tok_index: TokenIndex) []const u8 {
        return token_to_str(p.tokens.get(tok_index), p.src);
    }

    fn append_extra_struct(p: *Parser, comptime s_typ: type, s: s_typ) ParseError!Node.ExtraIndex {
        const type_info = comptime @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = comptime type_info.Struct;
            const index = p.extra.items.len;
            inline for (s_info.fields) |field| {
                if (field.type == u32) {
                    const field_val = @field(s, field.name);
                    try p.extra.append(field_val);
                } else {
                    _ = try p.append_extra_struct(field.type, @field(s, field.name));
                }
            }
            return @intCast(index);
        } else {
            @panic("Can only append structs of structs/u32s");
        }
    }

    fn get_extra_struct(p: *Parser, comptime s_typ: type, extra_index: Node.ExtraIndex) s_typ {
        const type_info = @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = type_info.Struct;
            var s: s_typ = undefined;

            comptime var offset = 0;
            inline for (s_info.fields) |field| {
                if (field.type == u32) {
                    const field_val = p.extra.items[extra_index + offset];
                    @field(s, field.name) = field_val;
                    offset += 1;
                } else {
                    const field_val = p.get_extra_struct(field.type, extra_index + offset);
                    @field(s, field.name) = field_val;
                    offset += @typeInfo(field.type).Struct.fields.len;
                }
            }
            return s;
        } else {
            @panic("Can only retrieve structs of structs/u32s");
        }
    }

    fn pop_scratch_to_extra(p: *Parser, count: usize) !Node.IndexSlice {
        const elems = p.scratch.items[p.scratch.items.len - count ..];

        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(elems);
        const end: u32 = @intCast(p.extra.items.len);

        p.scratch.items.len -= count;
        return .{ .start = start, .end = end };
    }
};

// Decl
//   <- VarDecl
//   / "fn" Identifier "(" (Parameter "," )* Parameter? ")" ("->" Type)? Block // Function Declaration
fn parse_fn_decl(p: *Parser) ParseError!Node.Index {
    _ = try p.expect_token(.keyword_fn);
    const id_tok = try p.expect_token(.identifier);

    _ = try p.expect_token(.l_paren);
    var param_count: usize = 0;
    while (true) {
        const peek = try p.peek_token();
        switch (peek.type) {
            // Anything that can start a var decl
            .keyword_mut, .identifier => {
                const param = try parse_parameter(p);
                try p.scratch.append(param);
                param_count += 1;
            },
            .r_paren => break,
            else => return error.UnexpectedToken,
        }

        const maybe_comma = try p.peek_token();
        if (maybe_comma.type == .comma) {
            _ = p.next_token() catch undefined;
        } else {
            break;
        }
    }

    _ = try p.expect_token(.r_paren);
    _ = try p.expect_token(.minus_arrow);

    const type_expr = try parse_type_expr(p);
    print("Parsed type expr \n", .{});
    const block = try parse_block(p);

    var node: Node = undefined;
    if (param_count == 0) {
        node = Node{ .fn_decl = .{ .identifier = id_tok.index, .ret_type = type_expr, .block = block } };
    } else {
        const params = try p.pop_scratch_to_extra(param_count);
        const fn_decl_params = Node.FnDeclParams{ .identifier = id_tok.index, .ret_type = type_expr, .params = params, .block = block };
        const extra_index = try p.append_extra_struct(Node.FnDeclParams, fn_decl_params);

        node = Node{ .fn_decl_params = extra_index };
    }
    return p.append_node(node);
}

// VarDecl
//   <- Identifier ":" Type ";"
//   / Identifier ":" "=" Expr ";"
//   / Identifier ":" Type "=" Expr ";"

fn parse_var_decl(p: *Parser) ParseError!Node.Index {
    const id_tok = try p.expect_token(.identifier);
    return parse_var_decl_w_id(p, id_tok, false);
}

fn parse_parameter(p: *Parser) ParseError!Node.Index {
    const id_tok = try p.expect_token(.identifier);
    return parse_var_decl_w_id(p, id_tok, true);
}

fn parse_var_decl_w_id(p: *Parser, id_tok: Parser.TokenInfo, comptime is_param_decl: bool) ParseError!Node.Index {
    const colon_tok = try p.next_token();

    if (id_tok.type != .identifier or colon_tok.type != .colon) {
        return error.UnexpectedToken;
    }

    var type_expr: ?Node.Index = null;
    var expr: ?Node.Index = null;

    var peek = try p.peek_token();
    if (peek.type != .equal) {
        type_expr = try parse_type_expr(p);
    }

    if (peek.type == .equal) {
        _ = p.next_token() catch undefined;
        expr = try parse_expr(p, 0);
    }

    var node: Node = undefined;
    if (type_expr != null and expr != null) {
        if (is_param_decl) {
            return error.ParamInit;
        }
        node = Node{ .var_decl_full = .{ .identifier = id_tok.index, .decl_type = type_expr.?, .decl_expr = expr.? } };
    } else if (type_expr != null) {
        node = Node{ .var_decl_type = .{ .identifier = id_tok.index, .decl_type = type_expr.? } };
    } else if (expr != null) {
        if (is_param_decl) {
            return error.ParamInit;
        }
        node = Node{ .var_decl_expr = .{ .identifier = id_tok.index, .decl_expr = expr.? } };
    } else {
        return error.EarlyTermination;
    }

    if (!is_param_decl) {
        _ = try p.expect_token(.semicolon);
    }

    return p.append_node(node);
}

// Block <- "{" Statement* "}"

// Expressions evaluate to a value
// Statements may evaluate to a control-flow effect
// Statement
//   <- Decl / Assignment / Block / IfStatement

fn parse_identifier(p: *Parser) ParseError!Node.Index {
    const id_tok = try p.expect_token(.identifier);
    const node = Node{ .identifier = id_tok.index };
    return p.append_node(node);
}

fn create_statements_node(p: *Parser, comptime node_name: []const u8, statements: ?[]Node.Index, comptime tok_field: []const u8, tok: Parser.TokenIndex) ParseError!Node.Index {
    var node: Node = undefined;
    if (statements) |*s| {
        if (s.len == 1) {
            node = @unionInit(Node, node_name ++ "_one", undefined);
            @field(node, node_name ++ "_one").statement = s.*[0];
            @field(@field(node, node_name ++ "_one"), tok_field) = tok;
            p.scratch.items.len -= s.len;
        } else {
            const slice = try p.pop_scratch_to_extra(s.len);
            node = @unionInit(Node, node_name, undefined);
            @field(node, node_name).statements_start = slice.start;
            @field(node, node_name).statements_end = slice.end;
            @field(@field(node, node_name), tok_field) = tok;
            // node = Node{ .block = .{ .start_brace = l_brace.index, .statements_start = slice.start, .statements_end = slice.end } };
        }
    } else {
        @field(@field(node, node_name ++ "_empty"), tok_field) = tok;
    }

    return p.append_node(node);
}

//
fn parse_block(p: *Parser) ParseError!Node.Index {
    const l_brace = try p.expect_token(.l_brace);
    const statements = try parse_statements(p);

    // var block_node: Node = undefined;
    // if (statements) |*s| {
    //     if (s.len == 1) {
    //         block_node = Node{ .block_one = .{ .start_brace = l_brace.index, .statement = s.*[0] } };
    //         p.scratch.items.len -= s.len;
    //     } else {
    //         const slice = try p.pop_scratch_to_extra(s.len);
    //         block_node = Node{ .block = .{ .start_brace = l_brace.index, .statements_start = slice.start, .statements_end = slice.end } };
    //     }
    // } else {
    //     block_node = Node{ .block_empty = .{ .start_brace = l_brace.index } };
    // }
    print("Parsed {any} statements\n", .{statements.?.len});
    const block_node = try create_statements_node(p, "block", statements, "start_brace", l_brace.index);

    return block_node;
}

// Will keep parsing statements until r_brace is encountered;
fn parse_statements(p: *Parser) ParseError!?[]Node.Index {
    // TODO: Better error reporting regarding braces like in ember
    var peek_tok = try p.peek_token();
    var statements: ?[]Node.Index = null;
    while (peek_tok.type != .r_brace) {
        switch (peek_tok.type) {
            .keyword_fn => try p.scratch.append(try parse_fn_decl(p)),
            .l_brace => try p.scratch.append(try parse_block(p)),
            // TODO: Variable-only keywords here can be used to start parsing for
            // a variable declaration unconditionally.
            .identifier => {
                const id_tok = try p.next_token();
                const maybe_colon = try p.peek_token();
                if (maybe_colon.type == .colon) {
                    const var_decl = try parse_var_decl_w_id(p, id_tok, false);
                    try p.scratch.append(var_decl);
                } else {
                    const id_node = Node{ .integer_lit = id_tok.index };
                    const target = try parse_postfix_expr_w_prim(p, try p.append_node(id_node));
                    const assignment = try parse_assigment_w_target(p, target, true);
                    try p.scratch.append(assignment);
                }
            },

            .keyword_if => {

                // IfStatement <- "if" Expr Capture? Block ("else" (IfStatement / Block))?
                var prev_else_index: ?Node.Index = null;
                while (true) {
                    const if_tok = try p.next_token();
                    _ = if_tok;
                    const expr = try parse_expr(p, 0);

                    // Possible capture
                    var peek = try p.peek_token();

                    // Capture <- "|" ReferenceOp? Identifier "|"
                    var capture: ?Node.Index = null;
                    if (peek.type == .pipe) {
                        _ = p.next_token() catch undefined;
                        capture = try parse_reference(p, parse_identifier);
                        _ = try p.expect_token(.pipe);
                    }

                    const block = try parse_block(p);

                    // Potential else case
                    peek = try p.peek_token();
                    if (peek.type == .keyword_else) {
                        _ = try p.next_token();

                        // Create an if-else node with the else case to be set as the subsequent block
                        // in the else case, and alternatively by the next iteration of the loop
                        var index: Node.Index = undefined;
                        if (capture) |cap_index| {
                            const extra = Node.IfElseCapture{ .condition = expr, .capture_ref = cap_index, .block = block, .else_block = undefined };
                            const e_index = try p.append_extra_struct(Node.IfElseCapture, extra);
                            index = try p.append_node(Node{ .if_else_statement_capture = e_index });
                        } else {
                            const node = Node{ .if_else_statement = .{ .condition = expr, .block = block, .else_block = undefined } };
                            index = try p.append_node(node);
                        }
                        try p.scratch.append(index);

                        if (prev_else_index) |else_index| {
                            p.nodes.items[else_index].if_else_statement.else_block = index;
                        }

                        peek = try p.peek_token();
                        switch (peek.type) {
                            .keyword_if => {
                                // else-if. The node created on this iteration will have it's destination set by the next
                                // iteration.
                                prev_else_index = index;
                            },
                            .l_brace => {
                                // final else
                                const else_block = try parse_block(p);
                                p.nodes.items[index].if_else_statement.else_block = else_block;
                                break;
                            },
                            else => return error.UnexpectedToken,
                        }
                    } else {
                        var node: Node = undefined;
                        if (capture) |cap_index| {
                            node = Node{ .if_statement_capture = .{ .condition = expr, .capture_ref = cap_index, .block = block } };
                        } else {
                            node = Node{ .if_statement = .{ .condition = expr, .block = block } };
                        }
                        const index = try p.append_node(node);

                        if (prev_else_index) |else_index| {
                            p.nodes.items[else_index].if_else_statement.else_block = index;
                        }

                        try p.scratch.append(index);
                        break;
                    }
                }
            },

            else => return error.Unimplemented,
        }

        if (statements) |*s| {
            s.len += 1;
        } else {
            statements = p.scratch.items[p.scratch.items.len - 1 ..];
        }

        peek_tok = try p.peek_token();
    }
    return statements;
}

// FieldAccess <- "." Identifier
// Dereference <- ".*"
// Indexing <- "[" Expr (".." Expr?)? "]"

// Assignment <- PostfixExpr AssignmentOp Expr ";"
// AssignmentOp <- "=" / "+=" / "-=" / "*=" / "/=" / "|=" / "&=" / "^=" / "<<=" / ">>="
fn parse_assignment(p: *Parser, consume_semi: bool) ParseError!Node.Index {
    const target = try parse_postfix_expr(p);
    print("Parsed target.\n", .{});
    return parse_assigment_w_target(p, target, consume_semi);
}

fn parse_assigment_w_target(p: *Parser, target: Node.Index, consume_semi: bool) ParseError!Node.Index {
    const assignment_tok = try p.next_token();
    switch (assignment_tok.type) {
        .equal, .plus_equal, .minus_equal, .slash_equal, .pipe_equal, .ampersand_equal, .caret_equal, .l_arrow2_equal, .r_arrow2_equal => {
            const expr = try parse_expr(p, 0);
            if (consume_semi) {
                _ = try p.expect_token(.semicolon);
            }
            const node = Node{ .assignment = .{ .token = assignment_tok.index, .target = target, .expr = expr } };
            return p.append_node(node);
        },
        else => return error.UnexpectedToken,
    }
}

const GenericParseFn = *const fn (p: *Parser) ParseError!Node.Index;

inline fn parse_reference(p: *Parser, comptime parse_after: GenericParseFn) ParseError!Node.Index {
    const ampersand = try p.next_token();
    if (ampersand.type != .ampersand and ampersand.type != .ampersand2) {
        return error.UnexpectedToken;
    }

    const maybe_cap = try p.peek_token();
    var prefix_node: Node = undefined;
    switch (maybe_cap.type) {
        .keyword_mut => {
            _ = try p.next_token();
            prefix_node = Node{ .ref_mut = .{ .ref_tok = ampersand, .target = try parse_after(p) } };
        },
        else => {
            prefix_node = Node{ .ref = .{ .ref_tok = ampersand, .target = try parse_after(p) } };
        },
    }

    if (ampersand.type == .ampersand2) {

        // Node for the innermost reference
        const ref_inner_index = try p.append_node(prefix_node);
        // Node for the outermost reference
        prefix_node = Node{ .ref = .{ .ref_tok = ampersand, .target = ref_inner_index } };
    }

    return p.append_node(prefix_node);
}

// ReferenceOp <- "&" "mut"?
// PrefixOps <- (ReferenceOp / "-" / "!")
// PrefixExpr <- PrefixOps* PostfixExpr

// TODO: Performance-wise, it may be worth inlining recursive parsing of prefixes into a a single loop.
fn parse_prefix_expr(p: *Parser) ParseError!Node.Index {
    const peek_tok = try p.peek_token();

    switch (peek_tok.type) {
        .minus, .not => {
            const token = p.next_token() catch undefined;
            const prefix_expr = try parse_prefix_expr(p);
            const prefix_node = Node{ .prefix_exp = .{ .token = token, .target = prefix_expr } };
            return p.append_node(prefix_node);
        },
        .ampersand, .ampersand2 => return parse_reference(p, parse_prefix_expr),
        else => return parse_postfix_expr(p),
    }
}

// TypePrefixOps = ReferenceOp / "[" "]"
// TypeExpr <- TypePrefixOps* PostfixExpr
fn parse_type_expr(p: *Parser) ParseError!Node.Index {
    const peek_tok = try p.peek_token();

    switch (peek_tok.type) {
        .l_bracket => return error.Unimplemented,
        .ampersand, .ampersand2 => return parse_reference(p, parse_type_expr),
        else => return parse_postfix_expr(p),
    }
}

// FunctionCall <- "(" (Expr ",")* Expr? ")"
// FieldAccess <- "." Identifier
// Dereference <- ".*"
// Indexing <- "[" Expr (".." Expr?)? "]"
// PostfixOps <- ( FunctionCall / FieldAccess / Dereference / Indexing )
// PostfixExpr <- PrimaryExpr PostfixOps*

// PrimaryExpr
//   <- "(" Expr ")"
//   / INTEGER
//   / IfExpr / ContainerLiteral / ContainerDefinition

fn parse_postfix_expr(p: *Parser) ParseError!Node.Index {
    // First, parse a primary expr
    const tok = try p.next_token();
    var primary: Node.Index = undefined;
    switch (tok.type) {
        .l_paren => {
            primary = try parse_expr(p, 0);
            _ = try p.expect_token(.r_paren);
        },
        .integer_bin, .integer_oct, .integer_hex, .integer_dec => primary = try p.append_node(.{ .integer_lit = tok.index }),
        .identifier => primary = try p.append_node(Node{ .identifier = tok.index }),
        .dot => return error.Unimplemented,

        // ContainerDefinition <- ("struct" / "enum") "{" Decl* "}"
        .keyword_struct => {
            _ = try p.expect_token(.l_brace);
            const statements = try parse_statements(p);
            primary = try create_statements_node(p, "struct_definition", statements, "struct_keyword", tok.index);
            _ = try p.expect_token(.r_brace);
        },
        .keyword_enum => {
            _ = try p.expect_token(.l_brace);
            const statements = try parse_statements(p);
            primary = try create_statements_node(p, "enum_definition", statements, "enum_keyword", tok.index);
            _ = try p.expect_token(.r_brace);
        },
        else => return error.UnexpectedToken,
    }
    print("Parsed primary \n", .{});

    return parse_postfix_expr_w_prim(p, primary);
}

// ContainerLiteral <- "." "{" Assignment* "}"
// fn parse_container_literal(p: *Parser) ParseError!Node.Index {
//     _ = p;}

fn parse_postfix_expr_w_prim(p: *Parser, pre_parsed_primary: Node.Index) ParseError!Node.Index {
    // Parse postfix
    var primary = pre_parsed_primary;
    while (true) {
        var peek = try p.peek_token();
        switch (peek.type) {
            .dot_asterisk => {
                const deref_node = Node{ .deref = .{ .token = try p.next_token(), .target = primary } };
                primary = try p.append_node(deref_node);
            },
            .l_paren => {
                const start_paren = p.next_token() catch undefined;

                var expr_count: usize = 0;
                while (true) {
                    const expr = try parse_expr(p, 0);
                    try p.scratch.append(expr);
                    expr_count += 1;
                    peek = try p.peek_token();

                    const maybe_comma = try p.peek_token();
                    if (maybe_comma.type == .comma) {
                        _ = p.next_token() catch undefined;
                        peek = try p.peek_token();
                        if (peek.type == .r_paren) {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                _ = try p.expect_token(.r_paren);

                var fn_node: Node = undefined;
                switch (expr_count) {
                    0 => {
                        fn_node = Node{ .fn_call_empty = .{ .target = primary, .start_paren = start_paren.index } };
                    },

                    1 => {
                        const arg = p.scratch.pop();
                        fn_node = Node{ .fn_call_single = .{ .target = primary, .start_paren = start_paren.index, .arg = arg } };
                    },

                    else => {
                        const args = try p.pop_scratch_to_extra(expr_count);
                        const fn_call = Node.FnCallFull{ .target = primary, .start_paren = start_paren.index, .args = args };
                        const extra_index = try p.append_extra_struct(Node.FnCallFull, fn_call);
                        fn_node = Node{ .fn_call_full = extra_index };
                    },
                }

                primary = try p.append_node(fn_node);
            },
            .dot => {
                _ = p.next_token() catch undefined;

                peek = try p.peek_token();
                if (peek.type == .l_brace) {
                    _ = p.next_token() catch undefined;
                    // Container literal
                    var assignments: ?[]Node.Index = null;
                    print("Got to containter literal \n", .{});
                    peek = try p.peek_token();
                    while (peek.type != .r_brace) {
                        const assignment = try parse_assignment(p, false);
                        try p.scratch.append(assignment);

                        if (assignments) |*a| {
                            a.len += 1;
                        } else {
                            assignments = p.scratch.items[p.scratch.items.len - 1 ..];
                        }
                        print("Parsed single assigment\n", .{});

                        peek = try p.peek_token();
                        if (peek.type == .semicolon) {
                            _ = p.next_token() catch undefined;
                        }
                        peek = try p.peek_token();
                    }
                    _ = try p.expect_token(.r_brace);

                    var container_node: Node = undefined;
                    if (assignments) |a| {
                        if (a.len == 1) {
                            container_node = Node{ .container_literal_one = .{ .target_type = primary, .assignment = a[0] } };
                        } else {
                            const slice = try p.pop_scratch_to_extra(a.len);
                            container_node = Node{ .container_literal = .{ .target_type = primary, .assignments_start = slice.start, .assignments_end = slice.end } };
                        }
                    } else {
                        container_node = Node{ .container_literal_empty = .{ .target_type = primary } };
                    }

                    primary = try p.append_node(container_node);
                } else if (peek.type == .identifier) {
                    // Field access
                    const field_tok = p.next_token() catch undefined;
                    const node = Node{ .field_access = .{ .target = primary, .field_id = field_tok.index } };
                    primary = try p.append_node(node);
                } else {
                    return error.UnexpectedToken;
                }
            },

            .l_bracket => return error.Unimplemented,
            else => return primary,
        }
    }
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
        .equal2, .not_equal, .l_arrow_equal, .r_arrow_equal, .l_arrow, .r_arrow => return 3,
        .ampersand, .pipe, .caret => return 4,
        .l_arrow2, .r_arrow2 => return 5,
        .plus, .minus => return 6,
        .asterisk, .slash => return 7,

        else => return null,
    }
}

fn parse_expr(p: *Parser, start_prec: Precedence) ParseError!Node.Index {
    // Parse an "atom"/left hand side of expression
    // TODO: Add pre-fix and post-fix parsing
    var lhs = try parse_prefix_expr(p);
    var maybe_op = try p.peek_token();

    var cur_prec = start_prec;

    while (operator_precedence(maybe_op.type)) |op_prec| {
        if (op_prec <= cur_prec) {
            return lhs;
        }

        const op_token = try p.next_token();

        const rhs = try parse_expr(p, op_prec);
        const op_node = Node{ .binary_exp = Node.BinaryExp{ .op_tok = op_token.index, .lhs = lhs, .rhs = rhs } };

        lhs = try p.append_node(op_node);
        maybe_op = try p.peek_token();
    }

    return lhs;
}

fn print_ast_prefix(prefix: *std.ArrayList(u8), is_last: bool) !usize {
    print("{s}", .{prefix.items});
    var indent_str: []const u8 = "";
    if (is_last) {
        print("└──", .{});
        indent_str = "    ";
        try prefix.appendSlice(indent_str);
    } else {
        print("├──", .{});
        indent_str = "│  ";
        try prefix.appendSlice(indent_str);
    }
    return indent_str.len;
}

fn print_ast_slice(
    p: *Parser,
    prefix: *std.ArrayList(u8),
    slice: Node.IndexSlice,
) !void {
    const items = p.extra.items[slice.start..slice.end];
    for (items, 0..) |item, i| {
        if (i != items.len - 1) {
            try print_ast(p, prefix, false, item);
        } else {
            try print_ast(p, prefix, true, item);
        }
    }
}

fn print_ast(p: *Parser, prefix: *std.ArrayList(u8), is_last: bool, cur_node: Parser.NodeIndex) ParseError!void {
    // print("At node {any} ", .{cur_node});
    const indent_len = try print_ast_prefix(prefix, is_last);
    defer prefix.items.len -= indent_len;

    const node = p.nodes.items[cur_node];
    switch (node) {
        .fn_decl => |decl| {
            const id_str = p.get_tok_str(decl.identifier);
            print("fn decl {s} \n", .{id_str});
            try print_ast(p, prefix, false, decl.ret_type);
            try print_ast(p, prefix, true, decl.block);
        },

        .fn_decl_params => |index| {
            const decl = p.get_extra_struct(Node.FnDeclParams, index);
            const id_str = p.get_tok_str(decl.identifier);
            print("fn params {s} \n", .{id_str});

            const param_indent = try print_ast_prefix(prefix, false);
            print("Parameters\n", .{});

            try print_ast_slice(p, prefix, decl.params);
            prefix.items.len -= param_indent;

            try print_ast(p, prefix, false, decl.ret_type);
            try print_ast(p, prefix, true, decl.block);
        },
        .var_decl_full => |decl| {
            const id_str = p.get_tok_str(decl.identifier);
            print("Full Var. decl for {s} \n", .{id_str});
            try print_ast(p, prefix, false, decl.decl_type);
            try print_ast(p, prefix, true, decl.decl_expr);
        },

        .var_decl_expr => |decl| {
            const id_str = p.get_tok_str(decl.identifier);
            print("Expr Var. decl for {s} \n", .{id_str});
            try print_ast(p, prefix, true, decl.decl_expr);
        },
        .var_decl_type => |decl| {
            const id_str = p.get_tok_str(decl.identifier);
            print("Type Var. decl for {s} \n", .{id_str});
            try print_ast(p, prefix, true, decl.decl_type);
        },

        .assignment => |assignment| {
            const str = p.get_tok_str(assignment.token);
            print("Assignment {s} \n", .{str});
            try print_ast(p, prefix, false, assignment.target);
            try print_ast(p, prefix, true, assignment.expr);
        },

        .if_statement => |statement| {
            print("if\n", .{});
            try print_ast(p, prefix, false, statement.condition);
            try print_ast(p, prefix, true, statement.block);
        },
        .if_statement_capture => |statement| {
            print("if capture\n", .{});
            try print_ast(p, prefix, false, statement.condition);
            try print_ast(p, prefix, false, statement.capture_ref);
            try print_ast(p, prefix, true, statement.block);
        },

        .if_else_statement => |statement| {
            print("if else\n", .{});
            try print_ast(p, prefix, false, statement.condition);
            try print_ast(p, prefix, false, statement.block);
            try print_ast(p, prefix, true, statement.else_block);
        },

        .if_else_statement_capture => |e_index| {
            const statement = p.get_extra_struct(Node.IfElseCapture, e_index);
            print("if else capture\n", .{});
            try print_ast(p, prefix, false, statement.condition);
            try print_ast(p, prefix, false, statement.capture_ref);
            try print_ast(p, prefix, false, statement.block);
            try print_ast(p, prefix, true, statement.else_block);
        },

        .block_empty => {
            print("Empty block\n", .{});
        },

        .block_one => |block| {
            print("Block one\n", .{});
            try print_ast(p, prefix, true, block.statement);
        },

        .block => |block| {
            print("Block\n", .{});
            try print_ast_slice(p, prefix, .{ .start = block.statements_start, .end = block.statements_end });
        },

        .struct_definition_empty => {
            print("Empty struct def.\n", .{});
        },

        .struct_definition_one => |block| {
            print("Struct def. one\n", .{});
            try print_ast(p, prefix, true, block.statement);
        },

        .struct_definition => |block| {
            print("Struct def.\n", .{});
            try print_ast_slice(p, prefix, .{ .start = block.statements_start, .end = block.statements_end });
        },

        .enum_definition_empty => {
            print("Empty struct def.\n", .{});
        },

        .enum_definition_one => |block| {
            print("Enum def. one\n", .{});
            try print_ast(p, prefix, true, block.statement);
        },

        .enum_definition => |block| {
            print("Enum def.\n", .{});
            try print_ast_slice(p, prefix, .{ .start = block.statements_start, .end = block.statements_end });
        },

        .binary_exp => |bin_exp| {
            const op_str = p.get_tok_str(bin_exp.op_tok);
            print("{s}\n", .{op_str});
            try print_ast(p, prefix, false, bin_exp.lhs);
            try print_ast(p, prefix, true, bin_exp.rhs);
        },

        .prefix_exp => |prefix_exp| {
            const prefix_str = p.get_tok_str(prefix_exp.token.index);
            print("{s} \n", .{prefix_str});
            try print_ast(p, prefix, true, prefix_exp.target);
        },
        .ref => |ref| {
            print("&\n", .{});
            try print_ast(p, prefix, true, ref.target);
        },
        .ref_mut => |ref| {
            print("&mut\n", .{});
            try print_ast(p, prefix, true, ref.target);
        },

        .deref => |deref| {
            const str = p.get_tok_str(deref.token.index);
            print("{s} \n", .{str});
            try print_ast(p, prefix, true, deref.target);
        },

        .fn_call_empty => {
            print("Empty Fn call. \n", .{});
        },
        .fn_call_single => |fn_call| {
            print("Fn call single \n", .{});
            try print_ast(p, prefix, true, fn_call.target);
        },

        .fn_call_full => |extra| {
            print("Fn call \n", .{});
            const fn_call = p.get_extra_struct(Node.FnCallFull, extra);
            try print_ast_slice(p, prefix, fn_call.args);
        },

        .container_literal_empty => |lit| {
            print("Empty container lit.\n", .{});
            try print_ast(p, prefix, true, lit.target_type);
        },

        .container_literal_one => |lit| {
            print("Container lit. single\n", .{});
            try print_ast(p, prefix, false, lit.target_type);
            try print_ast(p, prefix, true, lit.assignment);
        },

        .container_literal => |lit| {
            print("Container lit.\n", .{});
            try print_ast(p, prefix, false, lit.target_type);
            try print_ast_slice(p, prefix, .{ .start = lit.assignments_start, .end = lit.assignments_end });
        },

        .field_access => |f| {
            const field_name = p.get_tok_str(f.field_id);
            print("Field access .{s}\n", .{field_name});
            try print_ast(p, prefix, true, f.target);
        },

        .integer_lit, .identifier => |tok_index| {
            const str = p.get_tok_str(tok_index);
            print("{s} \n", .{str});
        },

        else => {
            print("\nPrint AST not implemented for {s}\n", .{@tagName(node)});
            return error.Unimplemented;
        },
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
