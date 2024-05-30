const std = @import("std");
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;

const parser_mod = @import("parser.zig");
const Node = parser_mod.Node;
const Parser = parser_mod.Parser;
const Ast = parser_mod.Ast;
const ParseError = parser_mod.ParseError;

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
    a: *Ast,
    prefix: *std.ArrayList(u8),
    slice: Node.IndexSlice,
) !void {
    const items = a.extra.items[slice.start..slice.end];
    for (items, 0..) |item, i| {
        if (i != items.len - 1) {
            try print_ast(a, prefix, false, item);
        } else {
            try print_ast(a, prefix, true, item);
        }
    }
}

fn print_ast(a: *Ast, prefix: *std.ArrayList(u8), is_last: bool, cur_node: Parser.NodeIndex) ParseError!void {
    // print("At node {any} ", .{cur_node});
    const indent_len = try print_ast_prefix(prefix, is_last);
    defer prefix.items.len -= indent_len;

    const node = a.nodes.items[cur_node];
    switch (node) {
        .root => |root| {
            print("Root\n", .{});
            try print_ast_slice(a, prefix, .{ .start = root.statements_start, .end = root.statements_end });
        },

        .fn_decl => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("fn decl {s} \n", .{id_str});
            try print_ast(a, prefix, false, decl.ret_type);
            try print_ast(a, prefix, true, decl.block);
        },

        .fn_decl_params => |index| {
            const decl = a.get_extra_struct(Node.FnDeclParams, index);
            const id_str = a.get_tok_str(decl.identifier);
            print("fn params {s} \n", .{id_str});

            const param_indent = try print_ast_prefix(prefix, false);
            print("Parameters\n", .{});

            try print_ast_slice(a, prefix, decl.params);
            prefix.items.len -= param_indent;

            try print_ast(a, prefix, false, decl.ret_type);
            try print_ast(a, prefix, true, decl.block);
        },
        .var_decl_full => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("Full Var. decl for {s} \n", .{id_str});
            try print_ast(a, prefix, false, decl.decl_type);
            try print_ast(a, prefix, true, decl.decl_expr);
        },

        .var_decl_expr => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("Expr Var. decl for {s} \n", .{id_str});
            try print_ast(a, prefix, true, decl.decl_expr);
        },
        .var_decl_type => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("Type Var. decl for {s} \n", .{id_str});
            try print_ast(a, prefix, true, decl.decl_type);
        },

        .mut_var_decl_full => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("Full mut Var. decl for {s} \n", .{id_str});
            try print_ast(a, prefix, false, decl.decl_type);
            try print_ast(a, prefix, true, decl.decl_expr);
        },

        .mut_var_decl_expr => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("Expr mut Var. decl for {s} \n", .{id_str});
            try print_ast(a, prefix, true, decl.decl_expr);
        },
        .mut_var_decl_type => |decl| {
            const id_str = a.get_tok_str(decl.identifier);
            print("Type mut Var. decl for {s} \n", .{id_str});
            try print_ast(a, prefix, true, decl.decl_type);
        },

        .assignment => |assignment| {
            const str = a.get_tok_str(assignment.token);
            print("Assignment {s} \n", .{str});
            try print_ast(a, prefix, false, assignment.target);
            try print_ast(a, prefix, true, assignment.expr);
        },

        .if_statement => |statement| {
            print("if\n", .{});
            try print_ast(a, prefix, false, statement.condition);
            try print_ast(a, prefix, true, statement.block);
        },
        .if_statement_capture => |statement| {
            print("if capture\n", .{});
            try print_ast(a, prefix, false, statement.condition);
            try print_ast(a, prefix, false, statement.capture_ref);
            try print_ast(a, prefix, true, statement.block);
        },

        .if_else_statement => |statement| {
            print("if else\n", .{});
            try print_ast(a, prefix, false, statement.condition);
            try print_ast(a, prefix, false, statement.block);
            try print_ast(a, prefix, true, statement.else_block);
        },

        .if_else_statement_capture => |e_index| {
            const statement = a.get_extra_struct(Node.IfElseCapture, e_index);
            print("if else capture\n", .{});
            try print_ast(a, prefix, false, statement.condition);
            try print_ast(a, prefix, false, statement.capture_ref);
            try print_ast(a, prefix, false, statement.block);
            try print_ast(a, prefix, true, statement.else_block);
        },

        .block_empty => {
            print("Empty block\n", .{});
        },

        .block_one => |block| {
            print("Block one\n", .{});
            try print_ast(a, prefix, true, block.statement);
        },

        .block => |block| {
            print("Block\n", .{});
            try print_ast_slice(a, prefix, .{ .start = block.statements_start, .end = block.statements_end });
        },

        .struct_definition_empty => {
            print("Empty struct def.\n", .{});
        },

        .struct_definition_one => |block| {
            print("Struct def. one\n", .{});
            try print_ast(a, prefix, true, block.statement);
        },

        .struct_definition => |block| {
            print("Struct def.\n", .{});
            try print_ast_slice(a, prefix, .{ .start = block.statements_start, .end = block.statements_end });
        },

        .enum_definition_empty => {
            print("Empty struct def.\n", .{});
        },

        .enum_definition_one => |block| {
            print("Enum def. one\n", .{});
            try print_ast(a, prefix, true, block.statement);
        },

        .enum_definition => |block| {
            print("Enum def.\n", .{});
            try print_ast_slice(a, prefix, .{ .start = block.statements_start, .end = block.statements_end });
        },

        .binary_exp => |bin_exp| {
            const op_str = a.get_tok_str(bin_exp.op_tok);
            print("{s}\n", .{op_str});
            try print_ast(a, prefix, false, bin_exp.lhs);
            try print_ast(a, prefix, true, bin_exp.rhs);
        },

        .prefix_exp => |prefix_exp| {
            const prefix_str = a.get_tok_str(prefix_exp.token.index);
            print("{s} \n", .{prefix_str});
            try print_ast(a, prefix, true, prefix_exp.target);
        },
        .ref => |ref| {
            print("&\n", .{});
            try print_ast(a, prefix, true, ref.target);
        },
        .ref_cap => |ref| {
            const cap_str = a.get_tok_str(ref.cap_tok);
            print("&.{s}\n", .{cap_str});
            try print_ast(a, prefix, true, ref.target);
        },

        .deref => |deref| {
            const str = a.get_tok_str(deref.token.index);
            print("{s} \n", .{str});
            try print_ast(a, prefix, true, deref.target);
        },

        .fn_call_empty => {
            print("Empty Fn call. \n", .{});
        },
        .fn_call_single => |fn_call| {
            print("Fn call single \n", .{});
            try print_ast(a, prefix, true, fn_call.target);
        },

        .fn_call_full => |extra| {
            print("Fn call \n", .{});
            const fn_call = a.get_extra_struct(Node.FnCallFull, extra);
            try print_ast_slice(a, prefix, fn_call.args);
        },

        .container_literal_empty => |lit| {
            print("Empty container lit.\n", .{});
            try print_ast(a, prefix, true, lit.target_type);
        },

        .container_literal_one => |lit| {
            print("Container lit. single\n", .{});
            try print_ast(a, prefix, false, lit.target_type);
            try print_ast(a, prefix, true, lit.assignment);
        },

        .container_literal => |lit| {
            print("Container lit.\n", .{});
            try print_ast(a, prefix, false, lit.target_type);
            try print_ast_slice(a, prefix, .{ .start = lit.assignments_start, .end = lit.assignments_end });
        },

        .field_access => |f| {
            const field_name = a.get_tok_str(f.field_id);
            print("Field access .{s}\n", .{field_name});
            try print_ast(a, prefix, true, f.target);
        },

        .integer_lit, .identifier => |tok_index| {
            const str = a.get_tok_str(tok_index);
            print("{s} \n", .{str});
        },

        else => {
            print("\nPrint AST not implemented for {s}\n", .{@tagName(node)});
            return error.Unimplemented;
        },
    }
}

pub fn print_ast_start(p: *Ast, root_id: Parser.NodeIndex) !void {
    var prefix = std.ArrayList(u8).init(p.allocator);
    defer prefix.deinit();

    try print_ast(p, &prefix, true, root_id);
}
