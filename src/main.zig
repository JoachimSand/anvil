const std = @import("std");
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;

const parser_mod = @import("parser.zig");
const Node = parser_mod.Node;
const Parser = parser_mod.Parser;

const pretty_print_mod = @import("pretty_print.zig");
const compile_mod = @import("compile.zig");

const air_mod = @import("air.zig");
const tir_mod = @import("tir.zig");

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
    var file_name: [:0]const u8 = undefined;
    while (args.next()) |arg| {
        if (std.mem.eql(u8, "-f", arg)) {
            file_name = args.next().?;
            read_from_stdin = false;
        }
    }

    if (read_from_stdin) {
        const stdin = std.io.getStdIn().reader();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);
        print("Got input {s} with length {} \n", .{ input.items, input.items.len });
    } else {
        try compile_mod.read_file(&input, file_name);
    }

    // const node_tag = NodeTag{ .param_list = .{ .hello = 10 } };
    // print("{any}\n", .{node_tag});

    var tokeniser = Tokeniser{ .src = input.items };
    const token_list = try tokeniser.tokenise_all(allocator);

    var parser = Parser.init(input.items, token_list, allocator);

    var ast = try parser.get_ast();
    try pretty_print_mod.print_ast_start(&ast, ast.root);

    var air = try air_mod.air_gen(&ast);
    defer air.deinit();
    // We can now free the parser contents: tokens, nodes etc.
    parser.deinit();

    _ = try tir_mod.tir_gen(&air, air.allocator);
    // print("Root node {any}\n", .{parser.nodes.items[root_id]});
    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    // try bw.flush(); // don't forget to flush!
}
