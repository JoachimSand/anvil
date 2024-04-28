const std = @import("std");
const print = std.debug.print;
const tokeniser_mod = @import("tokeniser.zig");
const Tokeniser = tokeniser_mod.Tokeniser;
const Token = tokeniser_mod.Token;

const NodeTag = union(enum) {
    pub const ParamList = struct {
        params: []const u8,
    };
    param_list: ParamList,
};

const Node = struct {
    tag: NodeTag,
};

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    print("All your {s} are belong to us.\n", .{"codebase"});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const stdin = std.io.getStdIn().reader();
    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    try stdin.streamUntilDelimiter(input.writer(), '\n', null);
    print("Got input {s} with length {} \n", .{ input.items, input.items.len });

    const node_tag = NodeTag{ .param_list = NodeTag.ParamList{ .params = input.items } };
    print("{any}\n", .{node_tag});

    var tokeniser = Tokeniser{ .buf = input.items, .cur_pos = 0, .cur_tok_start = 0, .cur_token = null };
    while (true) {
        const tok = tokeniser.next_token() catch return;
        print("Got token type {any} at {any}\n", .{ tok.type, tok.start });
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
