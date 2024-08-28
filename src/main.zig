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
const safety_mod = @import("safety.zig");

const llvm_gen = @import("llvm_gen.zig");

pub fn main() !void {
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
    print("\n=========== FILE CONTENTS ===========\n", .{});
    print("{s}", .{input.items});
    print("\n===========               ===========\n", .{});

    // const node_tag = NodeTag{ .param_list = .{ .hello = 10 } };
    // print("{any}\n", .{node_tag});

    var tokeniser = Tokeniser{ .src = input.items };
    const token_list = try tokeniser.tokenise_all(allocator);

    var parser = Parser.init(input.items, token_list, allocator);

    var ast = parser.get_ast() catch |e| {
        parser.deinit();
        return e;
    };
    try pretty_print_mod.print_ast_start(&ast, ast.root);

    var air = try air_mod.air_gen(&ast);
    defer air.deinit();
    // We can now free the parser contents: tokens, nodes etc.
    parser.deinit();

    // var tir = try tir_mod.tir_gen(&air, air.allocator);
    // defer tir.deinit();
    // _ = try safety_mod.check_safety(&tir);

    // try llvm_gen.generate_llvm_ir(&tir);
}

fn test_compiler(file_name: []const u8) !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    print("\n============================ TESTING FILE {s} ============================\n", .{file_name});

    print("Size of node: {any}\n", .{@sizeOf(Node)});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    try compile_mod.read_file(&input, file_name);

    print("\n=========== FILE CONTENTS ===========\n", .{});
    print("{s}", .{input.items});
    print("\n===========               ===========\n", .{});
    // const node_tag = NodeTag{ .param_list = .{ .hello = 10 } };
    // print("{any}\n", .{node_tag});

    var tokeniser = Tokeniser{ .src = input.items };
    const token_list = try tokeniser.tokenise_all(allocator);

    var parser = Parser.init(input.items, token_list, allocator);

    var ast = parser.get_ast() catch |e| {
        parser.deinit();
        return e;
    };
    try pretty_print_mod.print_ast_start(&ast, ast.root);

    var air = try air_mod.air_gen(&ast);
    defer air.deinit();
    // We can now free the parser contents: tokens, nodes etc.
    parser.deinit();

    var tir = try tir_mod.tir_gen(&air, air.allocator);
    defer tir.deinit();
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

// test "all" {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     const allocator = gpa.allocator();
//     defer _ = gpa.deinit();
//     var test_dir = try std.fs.Dir.openDir(std.fs.cwd(), "tests/", .{ .iterate = true });
//     var iterate = test_dir.iterate();
//     while (try iterate.next()) |entry| {
//         if (entry.kind == .file) {
//             var full_name = std.ArrayList(u8).init(allocator);
//             defer full_name.deinit();
//             try full_name.appendSlice("tests/");
//             try full_name.appendSlice(entry.name);

//             try test_compiler(full_name.items);
//         }
//     }
// }

test "basic_arrays.anv" {
    try test_compiler("tests/basic_arrays.anv");
}
test "basic_func_0.anv" {
    try test_compiler("tests/basic_func_0.anv");
}
test "basic_func_struct.anv" {
    try test_compiler("tests/basic_func_struct.anv");
}
test "bin_node_0.anv" {
    try test_compiler("tests/bin_node_0.anv");
}
test "experiments.anv" {
    try test_compiler("tests/experiments.anv");
}
test "if_else_0.anv" {
    try test_compiler("tests/if_else_0.anv");
}
test "if_else_1.anv" {
    try test_compiler("tests/if_else_1.anv");
}
// test "overall_1.anv" {
//     try test_compiler("tests/overall_1.anv");
// }
test "simple_add.anv" {
    try test_compiler("tests/simple_add.anv");
}
test "simple_add_mut.anv" {
    try test_compiler("tests/simple_add_mut.anv");
}
test "structs_0.anv" {
    try test_compiler("tests/structs_0.anv");
}
test "structs_1.anv" {
    try test_compiler("tests/structs_1.anv");
}
test "while_loop_0.anv" {
    try test_compiler("tests/while_loop_0.anv");
}
test "while_loop_1.anv" {
    try test_compiler("tests/while_loop_1.anv");
}
