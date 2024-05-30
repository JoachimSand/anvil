const std = @import("std");
const print = std.debug.print;

const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;

const parser_mod = @import("parser.zig");
const Node = parser_mod.Node;
const Parser = parser_mod.Parser;

const pretty_print_mod = @import("pretty_print.zig");

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

    const root_id = try parser_mod.parse_fn_decl(&parser);

    print("Root node {any}\n", .{parser.nodes.items[root_id]});
    var prefix = std.ArrayList(u8).init(allocator);
    defer prefix.deinit();

    print("Root\n", .{});
    try pretty_print_mod.print_ast(&parser, &prefix, true, root_id);
    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    // try bw.flush(); // don't forget to flush!
}
