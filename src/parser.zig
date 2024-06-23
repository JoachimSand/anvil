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
pub const Node = union(enum) {
    root: struct {
        statements_start: Index,
        statements_end: Index,
    },

    // Specified return type, no parameters/deps
    fn_decl: struct {
        identifier: Token.Index,
        ret_type: Index,
        block: Index,
    },

    // Multiple params
    // Index to FnDeclParams
    fn_decl_params: ExtraIndex,

    // Index to FnDeclFull
    fn_decl_full: ExtraIndex,

    var_decl_type: VarDeclType,
    var_decl_expr: VarDeclExpr,
    var_decl_full: VarDeclFull,
    mut_var_decl_type: VarDeclType,
    mut_var_decl_expr: VarDeclExpr,
    mut_var_decl_full: VarDeclFull,

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

    while_statement: struct {
        condition: Index,
        block: Index,
    },
    while_statement_capture: struct {
        capture_ref: Index,
        condition: Index,
        block: Index,
    },
    match_statement: struct {
        expr: Index,
        cases_start: ExtraIndex,
        cases_end: ExtraIndex,
    },
    match_case: struct {
        tag_id: Token.Index,
        capture_ref: Index,
        block: Index,
    },
    ret_statement_empty: struct { ret_token: Token.Index },
    ret_statement: struct {
        ret_token: Token.Index,
        expr: Index,
    },

    assignment: Assignment,

    block: struct {
        start_brace: Token.Index,
        statements_start: ExtraIndex,
        statements_end: ExtraIndex,
    },
    block_empty: struct {
        start_brace: Token.Index,
    },

    // TODO: Struct and enum definition nodes can both be extended
    // by a single field
    struct_definition: struct {
        struct_keyword: Token.Index,
        statements_start: Index,
        statements_end: Index,
    },
    struct_definition_empty: struct {
        struct_keyword: Token.Index,
    },

    enum_definition: struct {
        enum_keyword: Token.Index,
        statements_start: Index,
        statements_end: Index,
    },
    enum_definition_empty: struct {
        enum_keyword: Token.Index,
    },
    enum_literal: struct {
        enum_target: Index,
        active_identifier: Token.Index,
        expr: Index,
    },

    struct_literal: struct {
        target_type: Index,
        assignments_start: ExtraIndex,
        assignments_end: ExtraIndex,
    },
    struct_literal_empty: struct {
        target_type: Index,
    },

    field_access: struct { target: Index, field_id: Token.Index },

    indexing: struct {
        l_bracket: Token.Index,
        target: Index,
        index: Index,
    },
    slice_type: struct {
        l_bracket: Token.Index,
        target: Index,
    },

    binary_exp: BinaryExp,

    prefix_exp: Prefix,

    // Consider creating an explicit struct type for references
    ref: struct {
        ref_tok: Token.Index,
        target: Index,
    },
    ref_cap: struct {
        ref_tok: Token.Index,
        // May be mut or identifier
        cap_expr: Index,
        target: Index,
    },

    fn_call_empty: struct {
        target: Index,
        start_paren: Token.Index,
    },
    fn_call_full: ExtraIndex,

    deref: struct {
        target: Index,
        token: Parser.TokenInfo,
    },
    // field_access: struct {
    //     token: Token.Index,
    //     target: Index,
    //     field_name: Token.Index,
    // },
    identifier: Token.Index,
    built_in_alloc: Token.Index,
    built_in_free: Token.Index,
    built_in_print: Token.Index,
    integer_lit: Token.Index,

    pub const Index = u32;
    pub const ExtraIndex = u32;
    pub const List = std.ArrayList(Node);

    pub const Prefix = struct {
        target: Index,
        token: Parser.TokenInfo,
    };

    // Multiple parameters
    pub const FnDeclParams = packed struct {
        identifier: Token.Index,
        params: IndexSlice,
        ret_type: Index,
        block: Index,
    };

    pub const FnDeclFull = packed struct {
        identifier: Token.Index,
        params: IndexSlice,
        dependencies: Index,
        ret_type: Index,
        block: Index,
    };

    // Dependencies and return type
    // TODO: Disallow this in the grammar? Seems useless...
    pub const FnDeclDepsType = packed struct {
        identifier: Token.Index,
        params: IndexSlice,
        ret_type: Index,
        block: Index,
    };

    pub const FnCallFull = struct {
        target: Index,
        args: IndexSlice,
        start_paren: Token.Index,
    };

    const VarDeclType = struct {
        identifier: Token.Index,
        decl_type: Index,
    };
    const VarDeclExpr = struct {
        identifier: Token.Index,
        decl_expr: u32,
    };
    const VarDeclFull = struct {
        identifier: Token.Index,
        decl_type: Index,
        decl_expr: u32,
    };

    pub const Assignment = struct {
        token: Token.Index,
        target: Index,
        expr: Index,
    };

    pub const IfElseCapture = struct {
        condition: Index,
        capture_ref: Index,
        block: Index,
        else_block: Index,
    };

    pub const IndexSlice = packed struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const BinaryExp = struct {
        op_tok: Token.Index,
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

pub const ParseError = ParseSpecificError || std.mem.Allocator.Error;

pub const Ast = struct {
    src: []const u8,
    nodes: Node.List,
    extra: std.ArrayList(u32),

    tokens: Token.List,
    allocator: std.mem.Allocator,
    root: Node.Index,

    pub fn deinit(a: *Ast) void {
        a.nodes.deinit();
        a.extra.deinit();
        a.tokens.deinit(a.allocator);
    }

    pub fn get_tok_str(a: *Ast, tok_index: Token.Index) []const u8 {
        return token_to_str(a.tokens.get(tok_index), a.src);
    }

    pub fn get_extra_struct(a: *Ast, comptime s_typ: type, extra_index: Node.ExtraIndex) s_typ {
        const type_info = @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = type_info.Struct;
            var s: s_typ = undefined;

            comptime var offset = 0;
            inline for (s_info.fields) |field| {
                if (field.type == u32) {
                    const field_val = a.extra.items[extra_index + offset];
                    @field(s, field.name) = field_val;
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
};

pub const Parser = struct {
    src: []const u8,
    allocator: std.mem.Allocator,

    nodes: Node.List,
    tokens: Token.List = Token.List{},
    cur_token: Token.Index = 0,

    // TODO: Make this a memory allocator instead.
    // Would need to figure out how to neatly store
    // pointers to arrays in Node for this to work.
    // Since pointers are 64 bit, this would be tough.
    extra: std.ArrayList(u32),
    // Used for scratch allocations e.g. indeces for each statement in a block.
    // Scratch allocations must be freed after use, in effect making this a LIFO queue.
    scratch: NodeIndexList,

    pub const NodeIndex = u32;
    pub const NodeIndexList = std.ArrayList(NodeIndex);

    const TokenInfo = struct {
        type: TokenType,
        index: Token.Index,
    };

    pub fn init(src: []const u8, tokens: Token.List, allocator: std.mem.Allocator) Parser {
        const parser = Parser{ .src = src, .allocator = allocator, .tokens = tokens, .nodes = Node.List.init(allocator), .extra = std.ArrayList(u32).init(allocator), .scratch = Parser.NodeIndexList.init(allocator) };
        return parser;
    }

    pub fn deinit(p: *Parser) void {
        p.tokens.deinit(p.allocator);
        p.nodes.deinit();
        p.extra.deinit();
        p.scratch.deinit();
    }

    pub fn get_ast(p: *Parser) !Ast {
        const root = try parse_root(p);
        const ast = Ast{
            .src = p.src,
            .nodes = p.nodes,
            .extra = p.extra,
            .tokens = p.tokens,
            .allocator = p.allocator,
            .root = root,
        };
        return ast;
    }

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
            // print("Next {any} \n", .{tok});
            p.cur_token += 1;
            return TokenInfo{ .type = tok.type, .index = p.cur_token - 1 };
        } else {
            return error.EarlyTermination;
        }
    }

    fn expect_token(p: *Parser, tok_type: TokenType) ParseError!TokenInfo {
        const tok = try p.next_token();

        if (tok.type != tok_type) {
            // print("Expected token type {any}, got {any}\n", .{ tok_type, tok.type });
            return error.UnexpectedToken;
        } else {
            return tok;
        }
    }

    fn peek_token(p: *Parser) !TokenInfo {
        if (p.cur_token < p.tokens.len) {
            const tok = p.tokens.get(p.cur_token);
            // print("Peeked {any} \n", .{tok});
            return TokenInfo{ .type = tok.type, .index = p.cur_token };
        } else {
            return error.EarlyTermination;
        }
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

    fn pop_scratch_to_extra(p: *Parser, count: usize) !Node.IndexSlice {
        // print("\nPopping {} to extra, scratch has length {} \n", .{ count, p.scratch.items.len });
        const elems = p.scratch.items[p.scratch.items.len - count ..];

        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(elems);
        const end: u32 = @intCast(p.extra.items.len);

        p.scratch.items.len -= count;
        return .{ .start = start, .end = end };
    }
};

pub fn parse_root(p: *Parser) ParseError!Node.Index {
    const maybe_statements = try parse_statements(p);

    if (maybe_statements) |statements| {
        const slice = try p.pop_scratch_to_extra(statements.len);
        const node = Node{ .root = .{ .statements_start = slice.start, .statements_end = slice.end } };
        return p.append_node(node);
    } else {
        return error.EarlyTermination;
    }
}

// Decl
//   <- VarDecl
//   / "fn" Identifier "(" (Parameter "," )* Parameter? ")" ("->" Type)? Block // Function Declaration
pub fn parse_fn_decl(p: *Parser) ParseError!Node.Index {
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
    // print("Parsed type expr \n", .{});
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
//   <- "mut"? Identifier ":" ( TypeExpr / "=" Expr / TypeExpr "=" Expr) ";"

fn parse_var_decl_or_param(p: *Parser, comptime is_param_decl: bool) ParseError!Node.Index {
    const peek_tok = try p.peek_token();
    switch (peek_tok.type) {
        .keyword_mut => {
            _ = p.next_token() catch undefined;
            const id_tok = try p.expect_token(.identifier);
            return parse_var_decl_w_id(p, id_tok, true, is_param_decl);
        },
        .identifier => {
            const id_tok = try p.expect_token(.identifier);
            return parse_var_decl_w_id(p, id_tok, false, is_param_decl);
        },
        else => return error.UnexpectedToken,
    }
}

fn parse_var_decl(p: *Parser) ParseError!Node.Index {
    return parse_var_decl_or_param(p, false);
}

fn parse_parameter(p: *Parser) ParseError!Node.Index {
    return parse_var_decl_or_param(p, true);
}

fn parse_var_decl_w_id(p: *Parser, id_tok: Parser.TokenInfo, is_mut: bool, comptime is_param_decl: bool) ParseError!Node.Index {
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

    peek = try p.peek_token();
    if (peek.type == .equal) {
        _ = p.next_token() catch undefined;
        expr = try parse_expr(p, 0);
    }

    var node: Node = undefined;
    if (type_expr != null and expr != null) {
        if (is_param_decl) {
            return error.ParamInit;
        }
        if (is_mut) {
            node = Node{ .mut_var_decl_full = .{ .identifier = id_tok.index, .decl_type = type_expr.?, .decl_expr = expr.? } };
        } else {
            node = Node{ .var_decl_full = .{ .identifier = id_tok.index, .decl_type = type_expr.?, .decl_expr = expr.? } };
        }
    } else if (type_expr != null) {
        if (is_mut) {
            node = Node{ .mut_var_decl_type = .{ .identifier = id_tok.index, .decl_type = type_expr.? } };
        } else {
            node = Node{ .var_decl_type = .{ .identifier = id_tok.index, .decl_type = type_expr.? } };
        }
    } else if (expr != null) {
        if (is_param_decl) {
            return error.ParamInit;
        }
        if (is_mut) {
            node = Node{ .mut_var_decl_expr = .{ .identifier = id_tok.index, .decl_expr = expr.? } };
        } else {
            node = Node{ .var_decl_expr = .{ .identifier = id_tok.index, .decl_expr = expr.? } };
        }
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

fn create_statements_node(p: *Parser, comptime node_name: []const u8, statements: ?[]Node.Index, comptime tok_field: []const u8, tok: Token.Index) ParseError!Node.Index {
    var node: Node = undefined;
    if (statements) |*s| {
        const slice = try p.pop_scratch_to_extra(s.len);
        node = @unionInit(Node, node_name, undefined);
        @field(node, node_name).statements_start = slice.start;
        @field(node, node_name).statements_end = slice.end;
        @field(@field(node, node_name), tok_field) = tok;
        // node = Node{ .block = .{ .start_brace = l_brace.index, .statements_start = slice.start, .statements_end = slice.end } };
        // print("\nPopping {} statements, scratch has length {} \n", .{ s.len, p.scratch.items.len });
    } else {
        @field(@field(node, node_name ++ "_empty"), tok_field) = tok;
    }

    return p.append_node(node);
}

//
fn parse_block(p: *Parser) ParseError!Node.Index {
    const l_brace = try p.expect_token(.l_brace);
    const statements = try parse_statements(p);
    // print("Parsed {any} statements\n", .{statements.?.len});
    const block_node = try create_statements_node(p, "block", statements, "start_brace", l_brace.index);
    _ = try p.expect_token(.r_brace);
    // print("Finished parsing block\n", .{});
    return block_node;
}

// Capture <- "|" ReferenceOp? Identifier "|"
fn parse_capture(p: *Parser) ParseError!Node.Index {
    _ = try p.expect_token(.pipe);
    const peek = try p.peek_token();
    var capture: Node.Index = undefined;
    if (peek.type == .ampersand or peek.type == .ampersand2) {
        capture = try parse_reference(p, parse_identifier);
    } else {
        capture = try parse_identifier(p);
    }
    _ = try p.expect_token(.pipe);
    return capture;
}

// MatchCase <- Identifier Capture "->" Block
// MatchStatement <- "match" Expr "{" MatchCase+ "}"
fn parse_match_case(p: *Parser) ParseError!Node.Index {
    const id_tok = try p.expect_token(.identifier);
    const capture_ref = try parse_capture(p);
    _ = try p.expect_token(.minus_arrow);
    const block = try parse_block(p);
    return p.append_node(Node{ .match_case = .{ .tag_id = id_tok.index, .capture_ref = capture_ref, .block = block } });
}

// Will keep parsing statements until r_brace or EOF is encountered;
pub fn parse_statements(p: *Parser) ParseError!?[]Node.Index {
    // TODO: Better error reporting regarding braces like in ember
    var peek_tok = try p.peek_token();
    var statements: ?[]Node.Index = null;
    var statement_count: usize = 0;
    while (peek_tok.type != .r_brace) {
        // print("\nParsing statement starting with {}, scract at {}\n", .{ peek_tok.type, p.scratch.items.len });
        switch (peek_tok.type) {
            .keyword_fn => try p.scratch.append(try parse_fn_decl(p)),
            .l_brace => {
                try p.scratch.append(try parse_block(p));
                // pretty_print_mod.print_ast_start(p., p.scratch.getLast());

            },
            .keyword_return => {
                const ret_token = p.next_token() catch undefined;
                peek_tok = try p.peek_token();
                var node: Node = undefined;
                if (peek_tok.type != .semicolon) {
                    const expr = try parse_expr(p, 0);
                    node = Node{ .ret_statement = .{ .ret_token = ret_token.index, .expr = expr } };
                } else {
                    node = Node{ .ret_statement_empty = .{ .ret_token = ret_token.index } };
                }
                _ = try p.expect_token(.semicolon);
                const ret_index = try p.append_node(node);
                try p.scratch.append(ret_index);
            },
            // TODO: Variable-only keywords here can be used to start parsing for
            // a variable declaration unconditionally.
            .keyword_mut => {
                const var_decl = try parse_var_decl(p);
                try p.scratch.append(var_decl);
            },
            .identifier, .built_in_alloc, .built_in_free, .built_in_print => {
                const id_tok = try p.next_token();
                const second_peek = try p.peek_token();
                switch (second_peek.type) {
                    .colon => {
                        const var_decl = try parse_var_decl_w_id(p, id_tok, false, false);
                        try p.scratch.append(var_decl);
                    },
                    .l_paren => {
                        const id_node = switch (peek_tok.type) {
                            .identifier => Node{ .identifier = id_tok.index },
                            .built_in_alloc => Node{ .built_in_alloc = id_tok.index },
                            .built_in_free => Node{ .built_in_free = id_tok.index },
                            .built_in_print => Node{ .built_in_print = id_tok.index },
                            else => unreachable,
                        };
                        const fn_call = try parse_postfix_expr_w_prim(p, try p.append_node(id_node));
                        try p.scratch.append(fn_call);
                        _ = try p.expect_token(.semicolon);
                    },
                    else => {
                        const id_node = Node{ .identifier = id_tok.index };
                        const target = try parse_postfix_expr_w_prim(p, try p.append_node(id_node));
                        const assignment = try parse_assigment_w_target(p, target, true);
                        try p.scratch.append(assignment);
                    },
                }
            },

            .keyword_while => {
                const while_tok = p.next_token() catch undefined;
                _ = while_tok;
                const cond_expr = try parse_expr(p, 0);

                // Possible capture
                const peek = try p.peek_token();

                var capture: ?Node.Index = null;
                if (peek.type == .pipe) {
                    capture = try parse_capture(p);
                }

                const while_block = try parse_block(p);

                var node: Node = undefined;
                if (capture) |cap| {
                    node = Node{ .while_statement_capture = .{ .capture_ref = cap, .condition = cond_expr, .block = while_block } };
                } else {
                    node = Node{ .while_statement = .{ .condition = cond_expr, .block = while_block } };
                }
                const node_index = try p.append_node(node);
                try p.scratch.append(node_index);
            },

            .keyword_if => {

                // IfStatement <- "if" Expr Capture? Block ("else" (IfStatement / Block))?
                var prev_else_index: ?Node.Index = null;
                var top_level_index: ?Node.Index = null;
                while (true) {
                    const if_tok = try p.expect_token(.keyword_if);
                    _ = if_tok;
                    const expr = try parse_expr(p, 0);

                    // Possible capture
                    var peek = try p.peek_token();

                    // Capture <- "|" ReferenceOp? Identifier "|"
                    var capture: ?Node.Index = null;
                    if (peek.type == .pipe) {
                        capture = try parse_capture(p);
                    }

                    const block = try parse_block(p);

                    // print("Checking potential else\n", .{});
                    // Potential else case
                    peek = try p.peek_token();
                    if (peek.type == .keyword_else) {
                        _ = try p.next_token();
                        // print("Else encountered\n", .{});

                        // Create an if-else node with the else case to be set as the subsequent block
                        // in the else case, and alternatively by the next iteration of the loop
                        var current_index: Node.Index = undefined;
                        if (capture) |cap_index| {
                            const extra = Node.IfElseCapture{ .condition = expr, .capture_ref = cap_index, .block = block, .else_block = undefined };
                            const e_index = try p.append_extra_struct(Node.IfElseCapture, extra);
                            current_index = try p.append_node(Node{ .if_else_statement_capture = e_index });
                        } else {
                            const node = Node{ .if_else_statement = .{ .condition = expr, .block = block, .else_block = undefined } };
                            current_index = try p.append_node(node);
                        }

                        if (prev_else_index) |else_index| {
                            p.nodes.items[else_index].if_else_statement.else_block = current_index;
                        }

                        if (top_level_index == null) {
                            top_level_index = current_index;
                        }

                        peek = try p.peek_token();
                        switch (peek.type) {
                            .keyword_if => {
                                // else-if. The node created on this iteration will have it's destination set by the next
                                // iteration.
                                // print("else if detected\n", .{});
                                prev_else_index = current_index;
                            },
                            .l_brace => {
                                // final else
                                const else_block = try parse_block(p);
                                p.nodes.items[current_index].if_else_statement.else_block = else_block;
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
                        const final_if_index = try p.append_node(node);

                        if (prev_else_index) |else_index| {
                            p.nodes.items[else_index].if_else_statement.else_block = final_if_index;
                        }

                        if (top_level_index == null) {
                            top_level_index = final_if_index;
                        }
                        break;
                    }
                }

                if (top_level_index) |top_level| {
                    try p.scratch.append(top_level);
                } else {
                    unreachable;
                }
            },

            .keyword_match => {
                const match_tok = p.next_token() catch undefined;
                _ = match_tok;
                const expr = try parse_expr(p, 0);
                _ = try p.expect_token(.l_brace);

                var peek = try p.peek_token();
                var match_case_count: usize = 0;
                while (peek.type != .r_brace) {
                    const match_case = try parse_match_case(p);
                    try p.scratch.append(match_case);
                    match_case_count += 1;
                    peek = try p.peek_token();
                }
                _ = try p.expect_token(.r_brace);
                const match_case_slice = try p.pop_scratch_to_extra(match_case_count);
                const match_node = Node{ .match_statement = .{ .expr = expr, .cases_start = match_case_slice.start, .cases_end = match_case_slice.end } };
                const match_index = try p.append_node(match_node);
                try p.scratch.append(match_index);
            },

            else => {
                print("Parsing statement starting with {} unimplemented.\n", .{peek_tok});
                return error.Unimplemented;
            },
        }

        statement_count += 1;
        if (statements) |*s| {
            s.len += 1;
        } else {
            const start = p.scratch.items.len - 1;
            statements = p.scratch.items[start .. start + statement_count];
        }

        peek_tok = p.peek_token() catch |err| if (err == error.EarlyTermination) break else return err;
    }
    // print("FINAL STATEMENT COUNT: {}\n", .{statement_count});
    return statements;
}

// FieldAccess <- "." Identifier
// Dereference <- ".*"
// Indexing <- "[" Expr (".." Expr?)? "]"

// Assignment <- PostfixExpr AssignmentOp Expr ";"
// AssignmentOp <- "=" / "+=" / "-=" / "*=" / "/=" / "||=" / "&&=" / "^=" / "<<=" / ">>="
fn parse_assignment(p: *Parser, consume_semi: bool) ParseError!Node.Index {
    const target = try parse_postfix_expr(p);
    // print("Parsed target.\n", .{});
    return parse_assigment_w_target(p, target, consume_semi);
}

fn parse_assigment_w_target(p: *Parser, target: Node.Index, consume_semi: bool) ParseError!Node.Index {
    const assignment_tok = try p.next_token();
    switch (assignment_tok.type) {
        .equal, .plus_equal, .minus_equal, .slash_equal, .pipe2_equal, .ampersand2_equal, .caret_equal, .l_arrow2_equal, .r_arrow2_equal => {
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

// ReferenceOp <- "&"  ("." (Expr / "mut"))?
inline fn parse_reference(p: *Parser, comptime parse_after: GenericParseFn) ParseError!Node.Index {
    const ampersand = try p.next_token();
    if (ampersand.type != .ampersand and ampersand.type != .ampersand2) {
        return error.UnexpectedToken;
    }

    const maybe_dot = try p.peek_token();
    var prefix_node: Node = undefined;
    if (maybe_dot.type == .dot) {
        _ = try p.next_token();
        const cap_expr = try parse_postfix_expr(p);
        prefix_node = Node{ .ref_cap = .{ .ref_tok = ampersand.index, .cap_expr = cap_expr, .target = try parse_after(p) } };
    } else {
        prefix_node = Node{ .ref = .{ .ref_tok = ampersand.index, .target = try parse_after(p) } };
    }

    if (ampersand.type == .ampersand2) {

        // Node for the innermost reference
        const ref_inner_index = try p.append_node(prefix_node);
        // Node for the outermost reference
        prefix_node = Node{ .ref = .{ .ref_tok = ampersand.index, .target = ref_inner_index } };
    }

    return p.append_node(prefix_node);
}

// PrefixOps <- (ReferenceOp / "-" / "!" / "move")
// PrefixExpr <- PrefixOps* PostfixExpr

// TODO: Performance-wise, it may be worth inlining recursive parsing of prefixes into a a single loop.
fn parse_prefix_expr(p: *Parser) ParseError!Node.Index {
    // print("Parsing prefixexpr\n", .{});
    const peek_tok = try p.peek_token();

    switch (peek_tok.type) {
        .minus, .not, .keyword_move => {
            const token = p.next_token() catch undefined;
            const prefix_expr = try parse_prefix_expr(p);
            const prefix_node = Node{ .prefix_exp = .{ .token = token, .target = prefix_expr } };
            return p.append_node(prefix_node);
        },
        .ampersand, .ampersand2 => return parse_reference(p, parse_prefix_expr),
        else => return parse_postfix_expr(p),
    }
}

// TypePrefixOps = ReferenceOp / "[" Expr? "]"
// TypeExpr <- TypePrefixOps* PostfixExpr
fn parse_type_expr(p: *Parser) ParseError!Node.Index {
    var peek_tok = try p.peek_token();
    switch (peek_tok.type) {
        .l_bracket => {
            const l_bracket = p.next_token() catch undefined;
            peek_tok = try p.peek_token();

            if (peek_tok.type == .r_bracket) {
                // Slice specification
                _ = try p.expect_token(.r_bracket);
                const slice_type_node = Node{ .slice_type = .{ .l_bracket = l_bracket.index, .target = try parse_postfix_expr(p) } };
                return try p.append_node(slice_type_node);
            } else {
                // Array specification
                const index = try parse_expr(p, 0);
                _ = try p.expect_token(.r_bracket);
                const indexing_node = Node{ .indexing = .{ .l_bracket = l_bracket.index, .target = try parse_postfix_expr(p), .index = index } };
                return try p.append_node(indexing_node);
            }
        },
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
        .built_in_alloc => primary = try p.append_node(Node{ .built_in_alloc = tok.index }),
        .built_in_free => primary = try p.append_node(Node{ .built_in_free = tok.index }),
        .built_in_print => primary = try p.append_node(Node{ .built_in_print = tok.index }),

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
    // print("Parsed primary \n", .{});

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
                    // Struct literal
                    var assignments: ?[]Node.Index = null;
                    // print("Got to containter literal \n", .{});
                    peek = try p.peek_token();
                    while (peek.type != .r_brace) {
                        const assignment = try parse_assignment(p, false);
                        try p.scratch.append(assignment);

                        if (assignments) |*a| {
                            a.len += 1;
                        } else {
                            assignments = p.scratch.items[p.scratch.items.len - 1 ..];
                        }
                        // print("Parsed single assigment\n", .{});

                        peek = try p.peek_token();
                        if (peek.type == .semicolon) {
                            _ = p.next_token() catch undefined;
                        }
                        peek = try p.peek_token();
                    }
                    _ = try p.expect_token(.r_brace);

                    var struct_node: Node = undefined;
                    if (assignments) |a| {
                        const slice = try p.pop_scratch_to_extra(a.len);
                        struct_node = Node{ .struct_literal = .{ .target_type = primary, .assignments_start = slice.start, .assignments_end = slice.end } };
                    } else {
                        struct_node = Node{ .struct_literal_empty = .{ .target_type = primary } };
                    }

                    primary = try p.append_node(struct_node);
                } else if (peek.type == .identifier) {
                    // Field access
                    const field_tok = p.next_token() catch undefined;
                    const node = Node{ .field_access = .{ .target = primary, .field_id = field_tok.index } };
                    primary = try p.append_node(node);
                } else {
                    return error.UnexpectedToken;
                }
            },
            .colon2 => {
                // enum literal
                _ = p.next_token() catch undefined;
                const active_id = try p.expect_token(.identifier);
                _ = try p.expect_token(.l_brace);
                const expr = try parse_expr(p, 0);
                _ = try p.expect_token(.r_brace);

                const enum_lit = Node{ .enum_literal = .{ .enum_target = primary, .active_identifier = active_id.index, .expr = expr } };
                primary = try p.append_node(enum_lit);
            },

            .l_bracket => {
                const l_bracket = p.next_token() catch undefined;
                const index = try parse_expr(p, 0);

                peek = try p.peek_token();
                if (peek.type == .dot2) {
                    return error.Unimplemented;
                } else {
                    _ = try p.expect_token(.r_bracket);
                }
                const indexing_node = Node{ .indexing = .{ .l_bracket = l_bracket.index, .target = primary, .index = index } };
                primary = try p.append_node(indexing_node);
            },
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
        .ampersand2, .pipe2, .caret => return 4,
        .l_arrow2, .r_arrow2 => return 5,
        .plus, .minus => return 6,
        .asterisk, .slash => return 7,

        else => return null,
    }
}

fn parse_expr(p: *Parser, start_prec: Precedence) ParseError!Node.Index {
    // Parse an "atom"/left hand side of expression
    // TODO: Add pre-fix and post-fix parsing
    // print("Parsing expr\n", .{});

    var lhs = try parse_prefix_expr(p);

    // print("Parsing expr: Finished prefix\n", .{});

    var maybe_op = try p.peek_token();

    const cur_prec = start_prec;

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

fn test_parser(file_name: []const u8) !void {
    const pretty_print_mod = @import("pretty_print.zig");
    const compile_mod = @import("compile.zig");

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    try compile_mod.read_file(&buf, file_name);

    print("--------- Attemping to parse file {s}: ---------\n{s}\n", .{ file_name, buf.items });

    var tokeniser = Tokeniser{ .src = buf.items };
    const token_list = try tokeniser.tokenise_all(allocator);

    var parser = Parser.init(buf.items, token_list, allocator);
    defer parser.deinit();

    var ast = try parser.get_ast();
    try pretty_print_mod.print_ast_start(&ast, ast.root);
}

// test "overall_1" {
//     try test_parser("tests/overall_1.anv");
// }

// test "if_else_0" {
//     try test_parser("tests/if_else_0.anv");
// }

// test "if_else_1" {
//     try test_parser("tests/if_else_0.anv");
// }

// test "bin_node_0" {
//     try test_parser("tests/bin_node_0.anv");
// }
