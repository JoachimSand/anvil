const std = @import("std");
const print = std.debug.print;

pub fn read_file(buf: *std.ArrayList(u8), file_name: []const u8) !void {
    print("Reading from file {s}\n", .{file_name});
    var file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    _ = try file.reader().readAllArrayList(buf, 10000);
}
