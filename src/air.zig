const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const anvil_mal = @import("lib/anvil_multiarraylist.zig");
const MultiArrayList = anvil_mal.MultiArrayList;

const tokeniser_mod = @import("tokeniser.zig");
const Token = tokeniser_mod.Token;
const TokenType = tokeniser_mod.TokenType;

const parser_mod = @import("parser.zig");
const Ast = parser_mod.Ast;
const Node = parser_mod.Node;

const pretty_print_mod = @import("pretty_print.zig");

// Anvil Intermediate Representation

// mut myvar := 2;
// anothervar := myvar
// myvar += 2;
// %1 = int(2);
// %2 = %1;
// %3 = add(2, %1);

// mut myvar := 2;
// if myvar > 3 {
//     another := 4;
//     myvar += another;
// }
// %1 = int(2)
// %2 = int(3)
// %3 = gt(%1, %2)
// %4 = br(%3, blk, blk)
// %5 = block {
//     %6 = int(4);
//     // %7 = as_val(%6)
//     %8 = add(%1, %6)
// }

// BinNode := struct {
// 	a : u32;
// 	b : u32;
// };
// %1 = struct(a : Ref.u32, b : Ref.u32)

// my_type := i32;
// BinNode := struct {
// 	a : my_type;
// 	b : u32;
// };
// %1 = Ref.i32
// %2 = struct(a : %1, b : Ref.u32)

// Node := struct { c : bool};
// BinNode := struct {
// 	a : Node;
// 	b : u32;
// };
// %1 = struct(c : Ref.bool)
// %2 = struct(a : %1, b : Ref.u32)

// BinNode := struct {
// 	a : struct { c : bool};
// 	b : u32;
// };
// %1 = struct { c : bool};
// %2 = struct(a : %1, b : Ref.u32)

// Updates enum pointed to by $ptr_to_enum_memory in place
// %1 = update_enum_instance %enum_type, %ptr_to_enum_memory, %new_active_tag, %active_contents
// %1 = alloca enum { some : i16, else : i64 }
// %2 = i64 15;
// %3 = update_enum_instance enum { some : i16, else : i64 }, %1, 1, %2

// %4 = switch ( enum { some : i16, else : i64 },  %3) {
//     i8 0, label %6 <--- index of block
// }
// %6 = block{
//     $7 = enum_project %3, 0 // get_element_ptr equivalent
// }

pub const AirInst = union(enum) {
    pub const Index = u32;
    pub const Len = Index;
    const RefStart = 4294967040;
    pub const IndexRef = enum(Index) {
        // 2^32 -256
        bool = RefStart,
        true_lit,
        false_lit,
        null_lit,
        i8,
        i16,
        i32,
        i64,

        u8,
        u16,
        u32,
        u64,
        void,
        own,
        ref,
        type,
        address_of_self,
        _,

        pub fn is_index(ref: *const IndexRef) bool {
            if (@intFromEnum(ref.*) < RefStart) {
                return true;
            } else {
                return false;
            }
        }

        pub fn get_index(ref: *const IndexRef) Index {
            std.debug.assert(ref.is_index());
            return @intFromEnum(ref.*);
        }

        pub fn format(ref: *const IndexRef, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            if (ref.is_index()) {
                try std.fmt.format(writer, "%{d}", .{@intFromEnum(ref.*)});
            } else {
                try std.fmt.format(writer, "{s}", .{@tagName(ref.*)});
            }
        }
    };
    pub const List = MultiArrayList(AirInst);
    const Slice = List.Slice.All;

    const ExtraSlice = packed struct {
        start: Air.ExtraIndex,
        end: Air.ExtraIndex,
    };
    pub const BrEither = packed struct {
        cond: IndexRef,
        then_blk: IndexRef,
        else_blk: IndexRef,
    };
    pub const GetElementPtr = packed struct {
        aggregate_ptr: IndexRef,
        fields: ExtraSlice,
    };
    pub const Alloca = packed struct {
        type: IndexRef,
    };

    pub const ContainerDef = packed struct {
        // Number of fields in this container.
        // Their definition instructions ('FieldDef') directly trail this instruction.
        field_count: AirInst.Len,
    };
    pub const FieldDef = packed struct {
        var_name: Air.StringIndex,
        type_inst: IndexRef,
    };

    pub const MatchCase = packed struct {
        tag: Air.StringIndex,
        blk: Index,
    };
    pub const FnDef = packed struct {
        name: Air.StringIndex,
        params: ExtraSlice,
        ret_type: IndexRef,
        blk: Index,
    };
    pub const FnCall = packed struct {
        name: Air.StringIndex,
        args: ExtraSlice,
    };

    fn_def: Air.ExtraIndex,
    fn_call: Air.ExtraIndex,

    /// Container Definitions
    struct_def: ContainerDef,
    enum_def: ContainerDef,

    // Field type definitions.
    field_def: FieldDef,
    mut_field_def: FieldDef,

    /// Container instantiation and manipulation
    container_init: struct {
        container_type: IndexRef,

        // Number of field initialisations in this container init.
        // Their definition instructions ('field_init') directly trail this instruction.
        field_init_count: AirInst.Len,
    },

    field_init: struct {
        // 'get_field_type' instruction for the particular
        // field in question. Arguments reveal the field name and the
        // type of the container, while the return value is the
        // type of the field.
        field_type: IndexRef,

        // Expression to initialise container field with.
        expr: IndexRef,
    },

    // Given a pointer to/value of a container, retrieves type of a field with a given name.
    get_field_type: struct {
        container_type: IndexRef,
        field_name: Air.StringIndex,
    },

    // Given a pointer to/value of a container, retrieves value of a field with a given name.
    get_field_val: struct {
        container: IndexRef,
        field_name: Air.StringIndex,
    },

    // Given a pointer to/value of a container, retrieves value of a field with a given name.
    get_field_ptr: struct {
        container: IndexRef,
        field_name: Air.StringIndex,
    },

    block: struct {
        start: IndexRef,
        end: IndexRef,
    },
    br: IndexRef,
    br_cond: struct {
        cond: IndexRef,
        blk: IndexRef,
    },
    br_either: Air.ExtraIndex,
    ret_empty,
    ret: IndexRef,
    match: struct {
        enum_ptr: Index,
        cases_start: Air.ExtraIndex,
        cases_end: Air.ExtraIndex,
    },
    // Takes in an enum pointer and a tag. Allocates enough space for data type of the tag,
    // copies over the data from the enum_ptr and then returns a ptr to the newly allocated data type.
    enum_project: struct {
        enum_ptr: Index,
        tag: Air.StringIndex,
    },

    type_as: struct {
        type: IndexRef,
        expr: IndexRef,
    },
    type_of: IndexRef,
    type_of_deref: IndexRef,
    // type_of_enum_case: struct {
    //     enum_type: IndexRef,
    //     tag: Air.StringIndex,
    // },
    add: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
    sub: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
    lt: struct {
        lhs: IndexRef,
        rhs: IndexRef,
    },
    // gt: struct {
    //     lhs: IndexRef,
    //     rhs: IndexRef,
    // },
    // get_element_ptr: Air.ExtraIndex,
    // update_enum_ptr: struct {
    //     ptr: IndexRef,
    //     new_tag: Air.StringIndex,
    //     tag_contents: IndexRef,
    // },
    alloca: Alloca,
    alloca_mut: Alloca,
    memalloc: struct {
        expr: IndexRef,
    },
    print: struct {
        expr: IndexRef,
    },
    memfree: struct {
        expr: IndexRef,
    },
    deref: IndexRef,

    // Like alloca but for arrays
    zero_array: IndexRef,

    // 'get' converts an lvalue into an rvalue.
    get: struct {
        src: IndexRef,
        cap: IndexRef,
    },

    store: struct {
        val: IndexRef,
        ptr: IndexRef,
    },
    move: IndexRef,
    address_of: struct {
        target: IndexRef,
        cap: IndexRef,
    },
    indexing: struct {
        target: IndexRef,
        index: IndexRef,
    },
    // Access an argument to the function
    arg: struct { name: Air.StringIndex, type: IndexRef },
    int: u64,
};

const PrimitiveIdMap = std.ComptimeStringMap(AirInst.IndexRef, .{
    .{ "bool", AirInst.IndexRef.bool },
    .{ "true", AirInst.IndexRef.true_lit },
    .{ "false", AirInst.IndexRef.false_lit },
    .{ "null", AirInst.IndexRef.null_lit },
    .{ "i8", AirInst.IndexRef.i8 },
    .{ "i16", AirInst.IndexRef.i16 },
    .{ "i32", AirInst.IndexRef.i32 },
    .{ "i64", AirInst.IndexRef.i64 },
    .{ "u8", AirInst.IndexRef.u8 },
    .{ "u16", AirInst.IndexRef.u16 },
    .{ "u32", AirInst.IndexRef.u32 },
    .{ "u64", AirInst.IndexRef.u64 },
    .{ "void", AirInst.IndexRef.void },
    .{ "own", AirInst.IndexRef.own },
    .{ "ref", AirInst.IndexRef.ref },
    .{ "type", AirInst.IndexRef.type },
    // .{ "enum", .keyword_enum },
    // .{ "if", .keyword_if },
    // .{ "else", .keyword_else },
    // .{ "mut", .keyword_mut },
    // .{ "or", .keyword_or },
    // .{ "and", .keyword_and },
});

const Scope = union(enum) {
    const IdentifierInfo = struct {
        // Whether new assignments may be made to the identifier
        mutable: bool,
        // Index of Air inst that last wrote to the identifier
        inst: AirInst.IndexRef,
        type_inst: AirInst.IndexRef,
    };
    const IdentifierMap = std.AutoHashMap(Air.StringIndex, IdentifierInfo);
    top: IdentifierMap,
    // func_params: struct { params: IdentifierMap, block: IdentifierMap },
};

const AirSpecificError = error{ Unimplemented, Shadowing, AssignToImm, UndefinedVar, AllocExpectsOneArg, FreeExpectsOneArg, PrintExpectsOneArg };

pub const AirError = AirSpecificError || Allocator.Error || std.fmt.ParseIntError;

pub const Air = struct {
    instructions: AirInst.List,
    extra: ExtraList,

    string_index_map: std.StringHashMap(StringIndex),
    string_len_map: std.AutoHashMap(StringIndex, StringLen),
    strings: std.ArrayList(u8),

    allocator: std.mem.Allocator,

    pub const ExtraIndex = u32;
    pub const ExtraList = std.ArrayList(u32);
    pub const StringLen = u32;
    pub const StringIndex = u32;

    pub fn deinit(air: *Air) void {
        air.instructions.deinit(air.allocator);
        air.extra.deinit();

        air.string_index_map.deinit();
        air.string_len_map.deinit();
        air.strings.deinit();
    }

    fn intern_string(a: *Air, str: []const u8) Allocator.Error!StringIndex {
        const maybe_index = a.string_index_map.get(str);

        if (maybe_index) |index| {
            return index;
        } else {
            const index: StringIndex = @intCast(a.strings.items.len);
            try a.strings.appendSlice(str);
            try a.string_index_map.put(str, index);
            try a.string_len_map.put(index, @intCast(str.len));
            return index;
        }
    }

    pub fn get_string(a: *Air, index: StringIndex) []const u8 {
        const len = a.string_len_map.get(index).?;
        return a.strings.items[index .. index + len];
    }

    pub fn get_inst_slice(a: *Air, start: AirInst.Index, count: AirInst.Index) AirInst.Slice {
        // TODO: Inefficient to call slice repeatedly, especially if callee (e.g. TIRgen) already has.
        // The calculation only needs to be done for .data and .tags however, so this should not be very expensive.
        // More importantly is that .get() is incredibly expensive, involving a full switch-lookup.

        return a.instructions.slice().all_items_slice(start, start + count);
    }

    pub fn get_fields(a: *Air, container_index: AirInst.Index, count: AirInst.Index) AirInst.Slice {
        return a.get_inst_slice(container_index + 1, count);
    }

    fn append_extra_struct(a: *Air, val: anytype) Allocator.Error!Air.ExtraIndex {
        const type_info = comptime @typeInfo(@TypeOf(val));
        if (type_info == .Struct) {
            const s_info = comptime type_info.Struct;
            const index = a.extra.items.len;
            inline for (s_info.fields) |field| {
                if (field.type == AirInst.IndexRef) {
                    const field_val = @field(val, field.name);
                    try a.extra.append(@intFromEnum(field_val));
                } else if (field.type == u32) {
                    const field_val = @field(val, field.name);
                    try a.extra.append(field_val);
                } else if (field.type == bool) {
                    const field_val = @field(val, field.name);
                    try a.extra.append(@intFromBool(field_val));
                } else {
                    _ = try a.append_extra_struct(@field(val, field.name));
                }
            }
            return @intCast(index);
        } else {
            @panic("Can only append structs of structs/u32s");
        }
    }

    pub fn get_extra_struct(a: *Air, comptime s_typ: type, extra_index: Node.ExtraIndex) s_typ {
        const type_info = @typeInfo(s_typ);
        if (type_info == .Struct) {
            const s_info = type_info.Struct;
            var s: s_typ = undefined;

            comptime var offset = 0;
            inline for (s_info.fields) |field| {
                const field_val = a.extra.items[extra_index + offset];
                if (field.type == AirInst.IndexRef) {
                    @field(s, field.name) = @enumFromInt(field_val);
                    offset += 1;
                } else if (field.type == u32) {
                    @field(s, field.name) = field_val;
                    offset += 1;
                } else if (field.type == bool) {
                    @field(s, field.name) = (field_val != 0);
                    offset += 1;
                } else {
                    const field_struct = a.get_extra_struct(field.type, extra_index + offset);
                    @field(s, field.name) = field_struct;
                    offset += @typeInfo(field.type).Struct.fields.len;
                }
            }
            return s;
        } else {
            @panic("Can only retrieve structs of structs/u32s");
        }
    }
};

pub const AirState = struct {
    scratch: std.ArrayList(u32),
    scratch_inst: MultiArrayList(AirInst),

    air: Air,

    scopes: ScopeList,
    ast: *Ast,

    pub const ScopeList = std.ArrayList(Scope);

    fn get_air(a: *AirState) Air {
        return a.air;
    }

    fn intern_token(s: *AirState, index: Token.Index) Allocator.Error!Air.StringIndex {
        const tok = s.ast.tokens.get(index);
        const var_str = tokeniser_mod.token_to_str(tok, s.ast.src);
        return s.air.intern_string(var_str);
    }
    fn push_scope(s: *AirState) !void {
        try s.scopes.append(Scope{ .top = Scope.IdentifierMap.init(s.air.allocator) });
    }
    fn pop_scope(s: *AirState) void {
        var old_scope = s.scopes.pop();
        old_scope.top.deinit();
    }

    fn push_var(s: *AirState, var_tok: Token.Index, mutable: bool, inst: AirInst.IndexRef, type_inst: AirInst.IndexRef) AirError!void {
        const str_index = try s.intern_token(var_tok);

        // print("Pushing identifier {s}\n", .{tokeniser_mod.token_to_str(s.ast.tokens.get(var_tok), s.ast.src)});
        switch (s.scopes.items[s.scopes.items.len - 1]) {
            .top => |*top| {
                // if (top.get(str_index)) |_| {
                //     return error.Shadowing;
                // }
                try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst, .type_inst = type_inst });
            },
            // .func_params => |*func_scope| {
            //     const param_scope = func_scope.params;
            //     const blk_scope = func_scope.block;
            //     if (param_scope.get(str_index)) |_| {
            //         return error.Shadowing;
            //     }
            //     if (blk_scope.get(str_index)) |_| {
            //         return error.Shadowing;
            //     }
            //     try func_scope.block.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
            // },
        }
    }

    fn get_var(s: *AirState, var_tok: Token.Index) AirError!*Scope.IdentifierInfo {
        const str_index = try s.intern_token(var_tok);
        var scope_i = s.scopes.items.len;
        while (scope_i > 0) {
            scope_i -= 1;
            const scope = s.scopes.items[scope_i];
            switch (scope) {
                .top => |*top| {
                    if (top.getPtr(str_index)) |info| {
                        return info;
                    }
                    // try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
                },
                // .func_params => |*func_scope| {
                //     const param_scope = func_scope.params;
                //     const blk_scope = func_scope.block;
                //     if (param_scope.getPtr(str_index)) |info| {
                //         return info;
                //     }
                //     if (blk_scope.getPtr(str_index)) |info| {
                //         return info;
                //     }
                //     // try top.put(str_index, Scope.IdentifierInfo{ .mutable = mutable, .inst = inst });
                // },
            }
        }
        print("Undefined identifier {s}\n", .{tokeniser_mod.token_to_str(s.ast.tokens.get(var_tok), s.ast.src)});
        return error.UndefinedVar;
    }

    fn pop_scratch_insts(s: *AirState, count: usize) !void {
        const start: u32 = @intCast(s.air.instructions.len);
        try s.air.instructions.resize(s.air.allocator, s.air.instructions.len + count);
        const dst = s.air.get_inst_slice(start, @intCast(count));
        // const dst_data_slice = s.air.instructions.slice().items(.data)[start .. start + count];
        // const dst_tags_slice = s.air.instructions.slice().items(.tags)[start .. start + count];

        const scratch_start = s.scratch_inst.len - count;
        const src_data_slice = s.scratch_inst.slice().items(.data)[scratch_start..];
        const src_tags_slice = s.scratch_inst.slice().items(.tags)[scratch_start..];

        @memcpy(dst.tags, src_tags_slice);
        @memcpy(dst.data, src_data_slice);

        try s.scratch_inst.resize(s.air.allocator, scratch_start);
    }

    fn append_scratch_inst(s: *AirState, inst: AirInst) !void {
        try s.scratch_inst.append(s.air.allocator, inst);
    }

    fn pop_scratch_to_extra(s: *AirState, count: usize) !AirInst.ExtraSlice {
        // print("\nPopping {} to extra, scratch has length {} \n", .{ count, s.scratch.items.len });
        const elems = s.scratch.items[s.scratch.items.len - count ..];

        const start: u32 = @intCast(s.air.extra.items.len);
        try s.air.extra.appendSlice(elems);
        const end: u32 = @intCast(s.air.extra.items.len);

        s.scratch.items.len -= count;
        return .{ .start = start, .end = end };
    }

    fn append_inst(s: *AirState, instr: AirInst) !AirInst.IndexRef {
        try s.air.instructions.append(s.air.allocator, instr);
        return @enumFromInt(s.air.instructions.len - 1);
    }

    pub fn deinit(s: *AirState) void {
        // std.debug.assert(s.scratch.items.len == 0);
        s.scratch_inst.deinit(s.air.allocator);
        s.scratch.deinit();

        for (0..s.scopes.items.len) |index| {
            switch (s.scopes.items[index]) {
                .top => |*top| top.deinit(),
                // .func_params => |*func_scope| {
                //     func_scope.params.deinit();
                //     func_scope.block.deinit();
                // },
            }
        }
        s.scopes.deinit();
        s.air.deinit();
    }
};

pub fn print_air(a: *Air, start: u32, stop: u32, indent: u32) !u32 {
    // print("------------ Printing AIR ----------\n", .{});
    // print("Instruction count : {}\n", .{s.instructions.len});

    var index: u32 = start;
    while (index < stop and index < a.instructions.len) {
        for (0..indent) |_| {
            print("    ", .{});
        }

        print("%{} = ", .{index});
        const inst = a.instructions.get(index);
        switch (inst) {
            .int => |int| {
                print("int({})", .{int});
            },
            .move => |move| {
                print("move({})", .{move});
            },
            .address_of => |ad| {
                print("address of {} with cap {}", .{ ad.target, ad.cap });
            },
            .deref => |deref| {
                print("deref {}", .{deref});
            },
            .indexing => |id| {
                print("indexing {}, {}", .{ id.target, id.index });
            },
            .zero_array => |zero| {
                print("zero array with type {}", .{zero});
            },
            inline .add, .sub, .lt => |bin| {
                print("{s} {}, {}", .{ @tagName(inst), bin.lhs, bin.rhs });
            },
            // .gt => |gt| {
            //     print("gt(%{} > %{})", .{ gt.lhs, gt.rhs });
            // },
            .type_as => |type_as| {
                print("type {} as {}", .{ type_as.expr, type_as.type });
            },
            .type_of => |type_of| {
                print("typeof {}", .{type_of});
            },
            .type_of_deref => |type_of| {
                print("type_of_deref({})", .{type_of});
            },
            // .type_of_enum_case => |type_of| {
            //     print("type_of_enum_case(%{}, %{})", .{ type_of.enum_type, type_of.tag });
            // },
            .br => |br| {
                print("br {}", .{br});
            },
            .br_cond => |br| {
                print("br_cond {}, {}", .{ br.cond, br.blk });
            },
            .br_either => |br_extra| {
                const br = a.get_extra_struct(AirInst.BrEither, br_extra);
                print("br_either {}, ({} else {})", .{ br.cond, br.then_blk, br.else_blk });
            },

            .block => |blk| {
                print("block({d}, {d}){{\n", .{ @intFromEnum(blk.start), @intFromEnum(blk.end) });
                index = try print_air(a, @intFromEnum(blk.start) + 1, @intFromEnum(blk.end), indent + 1);
                // index = @intFromEnum(blk.end) - 1;

                for (0..indent) |_| {
                    print("    ", .{});
                }
                print("}}", .{});
            },
            .struct_def, .enum_def => |def| {
                if (inst == .struct_def) {
                    print("struct def \n", .{});
                } else {
                    print("enum def \n", .{});
                }
                index += 1;
                index = try print_air(a, index, index + def.field_count, indent + 1);
                continue;
            },
            .field_def, .mut_field_def => |fdef| {
                const var_name = a.get_string(fdef.var_name);
                if (inst == .field_def) {
                    print("mut_", .{});
                }
                print("field_def {s} : {};", .{ var_name, fdef.type_inst });
            },
            .container_init => |cinit| {
                print("container_init {}\n", .{cinit.container_type});
                index += 1;
                index = try print_air(a, index, index + cinit.field_init_count, indent + 1);
                continue;
            },
            .field_init => |init| {
                print("field_init {} with {}", .{ init.field_type, init.expr });
            },
            .get_field_type => |field_typ| {
                const field_name = a.get_string(field_typ.field_name);
                print("get_field_type {} {s}", .{ field_typ.container_type, field_name });
            },
            .get_field_val => |fval| {
                const field_name = a.get_string(fval.field_name);
                print("get_field_val {} {s}", .{ fval.container, field_name });
            },
            .get_field_ptr => |fptr| {
                const field_name = a.get_string(fptr.field_name);
                print("get_field_ptr {} {s}", .{ fptr.container, field_name });
            },
            .arg => |arg| {
                print("arg({s}, {})", .{ a.get_string(arg.name), arg.type });
            },
            .alloca => |alloc| {
                print("alloca {} ", .{alloc.type});
            },
            .alloca_mut => |alloc| {
                print("alloca_mut {} ", .{alloc.type});
            },
            .memalloc => |memalloc| {
                print("memalloc {} ", .{memalloc.expr});
            },
            .memfree => |memfree| {
                print("memfree {} ", .{memfree.expr});
            },
            .print => |p| {
                print("print {} ", .{p.expr});
            },
            .match => |match| {
                print("match on {}, ", .{match.enum_ptr});
                const type_info = @typeInfo(AirInst.MatchCase);
                const field_count: Air.ExtraIndex = @intCast(type_info.Struct.fields.len);
                var extra = match.cases_start;
                while (extra < match.cases_end) {
                    const match_case = a.get_extra_struct(AirInst.MatchCase, extra);
                    const tag = a.get_string(match_case.tag);
                    print("{s} -> {}, ", .{ tag, match_case.blk });

                    extra += field_count;
                }
            },
            .fn_call => |extra| {
                const fn_call = a.get_extra_struct(AirInst.FnCall, extra);
                const fn_name_str = a.get_string(fn_call.name);
                print("Fn Call {s}: (", .{fn_name_str});
                const args = a.extra.items[fn_call.args.start..fn_call.args.end];
                for (args) |arg| {
                    const arg_ref: AirInst.IndexRef = @enumFromInt(arg);
                    print("{}, ", .{arg_ref});
                }
                print(")", .{});
            },
            .enum_project => |project| {
                const tag = a.get_string(project.tag);
                print("project {} to {s}", .{ project.enum_ptr, tag });
            },
            .ret => |expr| {
                print("ret {}", .{expr});
            },
            .ret_empty => {
                print("ret_empty ", .{});
            },
            .get => |get| {
                print("get {} with cap {}", .{ get.src, get.cap });
            },
            .store => |store| {
                print("store({}, {})", .{ store.val, store.ptr });
            },
            .fn_def => |fn_extra| {
                const fn_def = a.get_extra_struct(AirInst.FnDef, fn_extra);
                const fn_name = a.get_string(fn_def.name);
                print("fn {s}(...)", .{fn_name});

                // const params = a.extra.items[fn_def.params.start..fn_def.params.end];
                // for (params) |param| {
                //     const arg = a.instructions.get(param).arg;
                //     const var_name = a.get_string(arg.name);
                //     print("{s} : %{}, ", .{ var_name, arg.type });
                // }
                print("-> {} at blk %{d}", .{ fn_def.ret_type, fn_def.blk });
            },
            // else => {
            //     print("AIR print unimplemented for {}\n", .{inst});
            //     return error.Unimplemented;
            // },
        }
        print(";\n", .{});
        index += 1;
    }
    return stop;
}

fn air_gen_container_def(s: *AirState, d_indeces: []const Node.Index, dest_id: ?Air.StringIndex, is_struct: bool) AirError!AirInst.IndexRef {
    // print("D INDECES LENGTH {}\n", .{d_indeces.len});
    // const start_extra: AirState.ExtraIndex = @intCast(s.extra.items.len);
    // var end_extra: AirState.ExtraIndex = undefined;
    var count: Air.ExtraIndex = 0;
    for (d_indeces) |d_index| {
        const decl = s.ast.nodes.items[d_index];
        switch (decl) {
            .var_decl_full, .mut_var_decl_full, .var_decl_expr, .mut_var_decl_expr => return error.Unimplemented,
            .mut_var_decl_type, .var_decl_type => |type_decl| {
                // TODO: PUSH SCOPE TO ALLOW REFERRING TO OTHER IDENTIFIERS IN STRUCT
                const decl_name = try s.intern_token(type_decl.identifier);
                // print("DECL NAME: {s}\n", .{s.air.get_string(decl_name)});
                const type_inst = try air_gen_expr(s, type_decl.decl_type, dest_id, .ref, true);

                const field_def = if (decl == .mut_var_decl_type)
                    AirInst{ .mut_field_def = .{ .var_name = decl_name, .type_inst = type_inst } }
                else
                    AirInst{ .field_def = .{ .var_name = decl_name, .type_inst = type_inst } };
                try s.append_scratch_inst(field_def);
                // print("Scratch len: {}\n", .{s.scratch.items.len});
                count += 1;
            },
            // .mut_var_decl_type => return error.Unimplemented,
            else => unreachable,
        }
    }

    const container_def = if (is_struct)
        try s.append_inst(.{ .struct_def = .{ .field_count = count } })
    else
        try s.append_inst(.{ .enum_def = .{ .field_count = count } });

    try s.pop_scratch_insts(count);
    return container_def;
}

fn air_gen_fn_call(s: *AirState, fn_call_extra: Node.ExtraIndex, get_cap: AirInst.IndexRef, dest_id: ?Air.StringIndex) AirError!AirInst.IndexRef {
    const fn_call = s.ast.get_extra_struct(Node.FnCallFull, fn_call_extra);

    const fn_target_node = s.ast.nodes.items[fn_call.target];
    // print("{}\n", .{fn_target_node});
    switch (fn_target_node) {
        .built_in_alloc => {
            const params = s.ast.extra.items[fn_call.args.start..fn_call.args.end];
            if (params.len != 1) {
                return error.AllocExpectsOneArg;
            }
            const param_expr = try air_gen_expr(s, params[0], dest_id, get_cap, true);
            return s.append_inst(AirInst{ .memalloc = .{ .expr = param_expr } });
        },
        .built_in_free => {
            const params = s.ast.extra.items[fn_call.args.start..fn_call.args.end];
            if (params.len != 1) {
                return error.FreeExpectsOneArg;
            }
            const param_expr = try air_gen_expr(s, params[0], dest_id, get_cap, true);
            return s.append_inst(AirInst{ .memfree = .{ .expr = param_expr } });
        },
        .built_in_print => {
            const params = s.ast.extra.items[fn_call.args.start..fn_call.args.end];
            if (params.len != 1) {
                return error.PrintExpectsOneArg;
            }
            const param_expr = try air_gen_expr(s, params[0], dest_id, get_cap, true);
            return s.append_inst(AirInst{ .print = .{ .expr = param_expr } });
        },
        .identifier => |id| {
            const fn_name = try s.intern_token(id);
            const args = s.ast.extra.items[fn_call.args.start..fn_call.args.end];
            for (args) |arg| {
                const param_expr = try air_gen_expr(s, arg, dest_id, get_cap, true);
                try s.scratch.append(@intFromEnum(param_expr));
            }
            const air_args = try s.pop_scratch_to_extra(args.len);
            const air_fn_call = try s.air.append_extra_struct(AirInst.FnCall{ .name = fn_name, .args = air_args });
            return s.append_inst(AirInst{ .fn_call = air_fn_call });
        },
        else => return error.Unimplemented,
    }
}

// Returns an l-value.
fn air_gen_expr(s: *AirState, index: Node.Index, dest_id: ?Air.StringIndex, get_cap: AirInst.IndexRef, r_value: bool) AirError!AirInst.IndexRef {
    // print("AIR inst. count is {}\n", .{s.air.instructions.len});
    // print("AIR Expr gen for the following node: \n", .{});
    // try pretty_print_mod.print_ast_start(s.ast, index);

    const cur_node = s.ast.nodes.items[index];
    switch (cur_node) {
        .struct_definition => |s_def| {
            const d_indeces = s.ast.extra.items[s_def.statements_start..s_def.statements_end];
            return air_gen_container_def(s, d_indeces, dest_id, true);
        },
        .enum_definition => |s_def| {
            const d_indeces = s.ast.extra.items[s_def.statements_start..s_def.statements_end];
            return air_gen_container_def(s, d_indeces, dest_id, false);
        },
        .container_literal => |struct_lit| {
            const container_type = try air_gen_expr(s, struct_lit.target_type, null, .ref, true);

            // Generate AIR for each individual assignment in the literal.
            const assignments = s.ast.extra.items[struct_lit.assignments_start..struct_lit.assignments_end];
            for (assignments) |assign_index| {
                const cur_assign = s.ast.nodes.items[assign_index];
                // print("Curassign {}\n", .{cur_assign});
                if (cur_assign != .assignment) {
                    return error.Unimplemented;
                }
                const assign_node = cur_assign.assignment;
                const target = s.ast.nodes.items[assign_node.target];
                if (target != .identifier) {
                    return error.Unimplemented;
                }

                const field_name = try s.intern_token(target.identifier);
                const expr_inst = try air_gen_expr(s, assign_node.expr, null, .ref, r_value);
                const field_type_inst = AirInst{ .get_field_type = .{ .field_name = field_name, .container_type = container_type } };
                const field_type_ref = try s.append_inst(field_type_inst);

                const type_as_inst = try s.append_inst(.{ .type_as = .{ .type = field_type_ref, .expr = expr_inst } });

                const init_inst = AirInst{ .field_init = .{ .field_type = field_type_ref, .expr = type_as_inst } };
                try s.append_scratch_inst(init_inst);
            }

            // Create the container initialisation instruction.
            const field_init_count: u32 = @intCast(assignments.len);
            const container_inst = AirInst{ .container_init = .{ .container_type = container_type, .field_init_count = field_init_count } };
            const inst = try s.append_inst(container_inst);

            // Pop the individual field initialisation instructions.
            try s.pop_scratch_insts(field_init_count);

            return inst;
        },
        .fn_call_full => |fn_call_extra| {
            return air_gen_fn_call(s, fn_call_extra, get_cap, dest_id);
        },
        .binary_exp => |bin_exp| {
            const bin_tok = s.ast.tokens.get(bin_exp.op_tok);
            const lhs_index = try air_gen_expr(s, bin_exp.lhs, null, get_cap, r_value);
            const rhs_index = try air_gen_expr(s, bin_exp.rhs, null, get_cap, r_value);

            const inst = switch (bin_tok.type) {
                .plus => AirInst{ .add = .{ .lhs = lhs_index, .rhs = rhs_index } },
                .l_arrow => AirInst{ .lt = .{ .lhs = lhs_index, .rhs = rhs_index } },
                .r_arrow => AirInst{ .lt = .{ .lhs = rhs_index, .rhs = lhs_index } },
                else => return error.Unimplemented,
            };
            return s.append_inst(inst);
        },
        .prefix_exp => |prefix| {
            switch (prefix.token.type) {
                .keyword_move => return s.append_inst(.{ .move = try air_gen_expr(s, prefix.target, dest_id, .own, r_value) }),
                .minus => {
                    const target = try air_gen_expr(s, prefix.target, dest_id, .own, r_value);
                    // TODO: Zero and other common constants should perhaps be a reference
                    const zero = try s.append_inst(AirInst{ .int = 0 });
                    return s.append_inst(AirInst{ .sub = .{ .lhs = zero, .rhs = target } });
                },
                else => return error.Unimplemented,
            }
        },
        .ref => |ref| {
            const target_node = s.ast.nodes.items[ref.target];
            // print("{} {?}\n", .{ target_node, dest_id });
            if (dest_id) |id| {
                if (target_node == .identifier and (try s.intern_token(target_node.identifier)) == id) {
                    return s.append_inst(.{ .address_of = .{ .target = .address_of_self, .cap = .ref } });
                }
            }
            // TODO: UNDO this
            // const ref_target = try air_gen_expr(s, ref.target, dest_id, load_cap, l_val);
            return s.append_inst(.{ .address_of = .{ .target = .address_of_self, .cap = .ref } });
        },
        .ref_cap => |ref| {
            const target_node = s.ast.nodes.items[ref.target];
            // print("{} {?}\n", .{ target_node, dest_id });
            if (dest_id) |id| {
                if (target_node == .identifier and (try s.intern_token(target_node.identifier)) == id) {
                    const ref_cap = try air_gen_expr(s, ref.cap_expr, dest_id, get_cap, r_value);
                    return s.append_inst(.{ .address_of = .{ .target = .address_of_self, .cap = ref_cap } });
                }
            }
            // TODO: UNDO this
            // const ref_target = try air_gen_expr(s, ref.target, dest_id, load_cap, l_val);
            const ref_cap = try air_gen_expr(s, ref.cap_expr, dest_id, get_cap, r_value);
            return s.append_inst(.{ .address_of = .{ .target = .address_of_self, .cap = ref_cap } });
        },
        .deref => |deref| {
            const deref_target = try air_gen_expr(s, deref.target, dest_id, get_cap, r_value);
            return s.append_inst(.{ .deref = deref_target });
        },
        .field_access => |access| {
            const fa_node = cur_node;
            // var fa_index = index;
            var fa_target = try air_gen_expr(s, fa_node.field_access.target, dest_id, get_cap, r_value);
            const field_name = try s.intern_token(access.field_id);
            if (r_value) {
                fa_target = try s.append_inst(.{ .get_field_val = .{ .field_name = field_name, .container = fa_target } });
            } else {
                fa_target = try s.append_inst(.{ .get_field_ptr = .{ .field_name = field_name, .container = fa_target } });
            }
            return fa_target;
        },
        .indexing => |indexing| {
            const index_expr = try air_gen_expr(s, indexing.index, dest_id, get_cap, true);
            const target = try air_gen_expr(s, indexing.target, dest_id, get_cap, r_value);
            return s.append_inst(.{ .indexing = .{ .target = target, .index = index_expr } });
        },
        .identifier => |tok_index| {
            const id_str = s.ast.get_tok_str(tok_index);
            // print("Id str {s}\n", .{id_str});
            if (PrimitiveIdMap.get(id_str)) |prim_index| {
                return prim_index;
            }
            const info = try s.get_var(tok_index);
            if (r_value) {
                const get_inst = s.append_inst(.{ .get = .{ .src = info.inst, .cap = get_cap } });
                return get_inst;
            } else {
                return info.inst;
            }

            // if (info.mutable) {
            //     const def_inst = s.air.instructions.get(@intFromEnum(info.inst));
            //     if (def_inst == .alloca_mut) {
            //         const get_inst = s.append_inst(.{ .get = .{ .src = info.inst, .cap = get_cap } });
            //         return get_inst;
            //     } else if (def_inst == .zero_array) {
            //         return info.inst;
            //     } else {
            //         return info.inst;
            //         // unreachable;
            //     }
            // } else {
            //     return info.inst;
            // }
        },
        .integer_lit => |tok_index| {
            const lit_str = tokeniser_mod.token_to_str(s.ast.tokens.get(tok_index), s.ast.src);
            const int_val = try std.fmt.parseInt(u64, lit_str, 0);
            const inst = AirInst{ .int = int_val };
            return s.append_inst(inst);
        },
        else => {
            print("AIR generatio for the following node is unimplemented: \n", .{});
            try pretty_print_mod.print_ast_start(s.ast, index);
            return error.Unimplemented;
        },
    }
}

fn expr_owns_memory(s: *AirState, expr_inst_ref: AirInst.IndexRef) bool {
    const expr_inst = s.air.instructions.get(@intFromEnum(expr_inst_ref));
    switch (expr_inst) {
        .move => |move| return expr_owns_memory(s, move),
        .alloca_mut => return true,
        .memfree => return true,
        .type_as => |type_as| return expr_owns_memory(s, type_as.expr),
        else => return false,
    }
}

fn air_gen_decl(s: *AirState, id: Token.Index, mutable: bool, type_node: ?Node.Index, expr: ?Node.Index) !void {
    // print("AIR GEN DECL WITH {s}\n", .{s.air.get_string(try s.intern_token(id))});
    const id_str = try s.intern_token(id);
    var expr_inst_ref: AirInst.IndexRef = undefined;
    if (expr) |expr_node| {
        expr_inst_ref = try air_gen_expr(s, expr_node, id_str, .ref, true);
    } else if (type_node) |typ| {
        // We allow type-only declarations for arrays - these are zero initialized.
        const node = s.ast.nodes.items[typ];
        if (node == .indexing) {
            const type_inst = try air_gen_expr(s, typ, id_str, .ref, true);
            const zero_inst = try s.append_inst(.{ .zero_array = type_inst });
            if (mutable) {
                try s.push_var(id, mutable, zero_inst, type_inst);
            } else {
                try s.push_var(id, mutable, zero_inst, type_inst);
            }
            return;
        } else {
            return error.Unimplemented;
        }
    }

    var type_inst: AirInst.IndexRef = undefined;
    if (type_node) |node| {
        type_inst = try air_gen_expr(s, node, id_str, .ref, true);
    } else {
        type_inst = try s.append_inst(.{ .type_of = expr_inst_ref });
    }
    const type_as_inst = try s.append_inst(.{ .type_as = .{ .type = type_inst, .expr = expr_inst_ref } });

    if (mutable) {
        const ptr_inst = try s.append_inst(.{ .alloca_mut = .{ .type = type_inst } });
        _ = try s.append_inst(.{ .store = .{ .ptr = ptr_inst, .val = type_as_inst } });
        try s.push_var(id, mutable, ptr_inst, type_inst);
    } else {
        try s.push_var(id, mutable, type_as_inst, type_inst);
    }
}

fn start_new_block(s: *AirState) !AirInst.Index {
    const start_inst: AirInst.IndexRef = @enumFromInt(s.air.instructions.len);
    return @intFromEnum(try s.append_inst(AirInst{ .block = .{ .start = start_inst, .end = undefined } }));
}

fn end_block(s: *AirState, block: AirInst.Index) void {
    const end_inst: AirInst.IndexRef = @enumFromInt(s.air.instructions.len);
    var cur_blk = s.air.instructions.get(block);
    cur_blk.block.end = end_inst;
    s.air.instructions.set(block, cur_blk);
}

// fn air_gen_container_field_assignment(
//     s: *AirState,
//     assign: Node.Assignment,
// ) !void {
//     const expr_inst = try air_gen_expr(s, assign.expr);

// cons, l_valt target_node = s.ast.nodes.items[assign.target];
//     if (target_node != .identifier) {
//         unreachable;
//     }
//     const id = target_node.identifier;

//     var target_ptr_inst: AirInst.IndexRef = undefined;
//     var target_type_inst: AirInst.IndexRef = undefined;
// }

fn air_gen_function(s: *AirState, fn_name_id: Token.Index, params: ?[]const Node.Index, ret_type: Node.Index, blk: Node.Index) AirError!void {
    const fn_name = try s.intern_token(fn_name_id);
    const reserved_air_index = try s.append_inst(undefined);

    try s.push_scope();
    const blk_inst = try start_new_block(s);

    var count: Air.ExtraIndex = 0;
    if (params) |param_indeces| {
        for (param_indeces) |param_index| {
            const decl = s.ast.nodes.items[param_index];
            switch (decl) {
                .var_decl_full, .mut_var_decl_full, .var_decl_expr, .mut_var_decl_expr => return error.Unimplemented,
                .var_decl_type => |type_decl| {
                    const decl_name = try s.intern_token(type_decl.identifier);
                    const type_inst = try air_gen_expr(s, type_decl.decl_type, null, .ref, true);

                    const arg_inst = AirInst{ .arg = .{ .name = decl_name, .type = type_inst } };
                    const arg_inst_index = try s.append_inst(arg_inst);
                    try s.push_var(type_decl.identifier, false, arg_inst_index, type_inst);

                    try s.scratch.append(@intFromEnum(arg_inst_index));
                    count += 1;
                },
                .mut_var_decl_type => return error.Unimplemented,
                else => unreachable,
            }
        }
    }
    const param_slice = try s.pop_scratch_to_extra(count);
    // const param_slice = try air_gen_decl_info_list(s, param_indeces, true);

    const air_ret_type = try air_gen_expr(s, ret_type, null, .ref, true);

    // Generate statements withins block
    const fn_s_indeces = try get_block_statements(s, blk);
    _ = try air_gen_statements(s, fn_s_indeces, blk_inst, null);
    s.pop_scope();
    end_block(s, blk_inst);

    const fn_def = AirInst.FnDef{ .name = fn_name, .params = param_slice, .ret_type = air_ret_type, .blk = blk_inst };
    const inst = AirInst{ .fn_def = try s.air.append_extra_struct(fn_def) };
    s.air.instructions.set(@intFromEnum(reserved_air_index), inst);
}

fn air_gen_statements(s: *AirState, s_indeces: []const Node.Index, start_block: AirInst.Index, dest_id: ?Air.StringIndex) AirError!void {
    var cur_block_inst = start_block;
    for (s_indeces) |s_index| {
        const statement = s.ast.nodes.items[s_index];
        print("AIR inst. count is {}\n", .{s.air.instructions.len});
        print("AIR gen for statement {}\n", .{statement});
        switch (statement) {
            .block => |_| _ = try air_gen_scoped_block(s, s_index, true, dest_id),

            .var_decl_full => |decl| try air_gen_decl(s, decl.identifier, false, decl.decl_type, decl.decl_expr),
            .mut_var_decl_full => |decl| try air_gen_decl(s, decl.identifier, true, decl.decl_type, decl.decl_expr),
            .var_decl_expr => |decl| try air_gen_decl(s, decl.identifier, false, null, decl.decl_expr),
            .mut_var_decl_expr => |decl| try air_gen_decl(s, decl.identifier, true, null, decl.decl_expr),

            // Only arrays currently support this syntax.
            .var_decl_type => |decl| try air_gen_decl(s, decl.identifier, false, decl.decl_type, null),
            .mut_var_decl_type => |decl| try air_gen_decl(s, decl.identifier, true, decl.decl_type, null),

            // .fn_decl => |fn_decl| {
            //     const fn_name = try s.intern_token(fn_decl.identifier);
            //     const
            // },
            .fn_decl => |fn_decl| {
                try air_gen_function(s, fn_decl.identifier, null, fn_decl.ret_type, fn_decl.block);
            },
            .fn_decl_params => |fn_extra| {
                const fn_decl = s.ast.get_extra_struct(Node.FnDeclParams, fn_extra);
                try air_gen_function(s, fn_decl.identifier, s.ast.extra.items[fn_decl.params.start..fn_decl.params.end], fn_decl.ret_type, fn_decl.block);
            },

            .assignment => |assign| {
                const target_ptr = try air_gen_expr(s, assign.target, null, .ref, false);
                const target_type = try s.append_inst(AirInst{ .type_of_deref = target_ptr });

                // Generate the assignment expression. Ensure that it's type matches the
                // target type.
                var expr_inst = try air_gen_expr(s, assign.expr, null, .ref, true);
                expr_inst = try s.append_inst(AirInst{ .type_as = .{ .type = target_type, .expr = expr_inst } });

                // Perform the assignment
                const assign_tok = s.ast.tokens.get(assign.token);
                if (assign_tok.type != .equal) {
                    // const old_val_inst_index = try s.append_inst(old_val_inst);
                    const old_val = try s.append_inst(AirInst{ .get = .{ .src = target_ptr, .cap = .ref } });
                    const assign_inst = switch (assign_tok.type) {
                        .plus_equal => AirInst{ .add = .{ .lhs = old_val, .rhs = expr_inst } },
                        else => return error.Unimplemented,
                    };
                    expr_inst = try s.append_inst(assign_inst);
                }
                _ = try s.append_inst(.{ .store = .{ .val = expr_inst, .ptr = target_ptr } });
            },
            .while_statement => |while_statement| {
                const prev_block_br = try s.append_inst(undefined);
                end_block(s, cur_block_inst);

                // Conditional block
                const cond_blk = try start_new_block(s);
                s.air.instructions.set(@intFromEnum(prev_block_br), .{ .br = @enumFromInt(cond_blk) });
                const cond_inst = try air_gen_expr(s, while_statement.condition, null, .ref, true);
                var cond_br = AirInst.BrEither{ .cond = cond_inst, .then_blk = undefined, .else_blk = undefined };
                const cond_br_index = try s.append_inst(undefined);

                end_block(s, cond_blk);

                // Loop body block
                const while_block = try air_gen_scoped_block(s, while_statement.block, true, null);
                _ = try s.append_inst(.{ .br = @enumFromInt(cond_blk) });
                end_block(s, @intFromEnum(while_block));

                // Block after loop
                cur_block_inst = try start_new_block(s);

                // Finally, write the contents of the conditional branch to either the loop body or the block after.
                cond_br.then_blk = while_block;
                cond_br.else_blk = @enumFromInt(cur_block_inst);
                const cond_br_inst = AirInst{ .br_either = try s.air.append_extra_struct(cond_br) };
                s.air.instructions.set(@intFromEnum(cond_br_index), cond_br_inst);
            },
            .if_else_statement => |if_else| {
                const cond_inst = try air_gen_expr(s, if_else.condition, null, .ref, true);

                const br_inst = try s.append_inst(undefined);
                end_block(s, cur_block_inst);

                const then_blk = try air_gen_scoped_block(s, if_else.block, true, dest_id);
                const else_br_end = try s.append_inst(undefined);
                end_block(s, @intFromEnum(then_blk));

                const else_blk = try air_gen_scoped_block(s, if_else.else_block, true, dest_id);
                const then_br_end = try s.append_inst(undefined);
                end_block(s, @intFromEnum(else_blk));

                const br_struct = AirInst.BrEither{ .cond = cond_inst, .then_blk = then_blk, .else_blk = else_blk };
                const br = try s.air.append_extra_struct(br_struct);

                // Block after if-else
                cur_block_inst = try start_new_block(s);

                s.air.instructions.set(@intFromEnum(br_inst), .{ .br_either = br });
                s.air.instructions.set(@intFromEnum(else_br_end), .{ .br = @enumFromInt(cur_block_inst) });
                s.air.instructions.set(@intFromEnum(then_br_end), .{ .br = @enumFromInt(cur_block_inst) });
            },
            .ret_statement => |ret| {
                const expr = try air_gen_expr(s, ret.expr, null, .ref, true);
                _ = try s.append_inst(AirInst{ .ret = expr });
            },
            .ret_statement_empty => {
                _ = try s.append_inst(.ret_empty);
            },
            .fn_call_full => |fn_call_extra| {
                _ = try air_gen_fn_call(s, fn_call_extra, .ref, dest_id);
            },
            // .match_statement => |match| {
            //     const match_inst_index = try s.append_inst(undefined);

            //     end_block(s, cur_block_inst);
            //     const enum_ptr = try air_gen_expr(s, match.expr, null, .ref, false);
            //     const match_cases = s.ast.extra.items[match.cases_start..match.cases_end];
            //     // print("match cases {any} {}\n", .{ match_cases, match_cases.len });
            //     for (match_cases) |case_index| {
            //         const case = s.ast.nodes.items[case_index].match_case;
            //         const case_tag = try s.intern_token(case.tag_id);

            //         try s.push_scope();

            //         const blk_inst = try start_new_block(s);
            //         const capture_node = s.ast.nodes.items[case.capture_ref];

            //         // Create the block for the case, inserting a project instruction for the active tag
            //         const project_inst = try s.append_inst(AirInst{ .enum_project = .{ .enum_ptr = @intFromEnum(enum_ptr), .tag = case_tag } });

            //         // Project inst creates a ptr to the tag data type. Load it.
            //         // TODO: Problem with aggregrates.
            //         const load_inst = try s.append_inst(AirInst{ .get = .{ .src = project_inst, .cap = .own } });

            //         // TODO: Should this be type of deref?
            //         const type_of_inst = try s.append_inst(AirInst{ .type_of = load_inst });

            //         switch (capture_node) {
            //             .identifier => {
            //                 try s.push_var(capture_node.identifier, false, load_inst, type_of_inst);
            //             },
            //             else => return error.Unimplemented,
            //         }
            //         const case_s_indeces = try get_block_statements(s, case.block);
            //         _ = try air_gen_statements(s, case_s_indeces, blk_inst, null);

            //         _ = try s.append_inst(undefined);
            //         s.pop_scope();
            //         end_block(s, blk_inst);

            //         // Finally, create a case struct as a part of the main match inst.
            //         const match_case = AirInst.MatchCase{ .tag = case_tag, .blk = blk_inst };
            //         _ = try s.append_scratch_struct(AirInst.MatchCase, match_case);
            //     }
            //     const field_count: u32 = @intCast(@typeInfo(AirInst.MatchCase).Struct.fields.len);
            //     const cases_slice = try s.pop_scratch_insts(match_cases.len * field_count);
            //     cur_block_inst = try start_new_block(s);

            //     var extra: u32 = cases_slice.start;
            //     while (extra < cases_slice.end) {
            //         const match_case = s.air.get_extra_struct(AirInst.MatchCase, extra);
            //         const match_blk = s.air.instructions.get(match_case.blk);
            //         const br_inst = AirInst{ .br = @enumFromInt(cur_block_inst) };

            //         s.air.instructions.set(@intFromEnum(match_blk.block.end) - 1, br_inst);
            //         extra += field_count;
            //     }

            //     const match_inst = AirInst{ .match = .{ .enum_ptr = @intFromEnum(enum_ptr), .cases_start = cases_slice.start, .cases_end = cases_slice.end } };
            //     s.air.instructions.set(@intFromEnum(match_inst_index), match_inst);
            // },
            else => {
                print("AIR generatio for the following node is unimplemented: \n", .{});
                try pretty_print_mod.print_ast_start(s.ast, s_index);
                return error.Unimplemented;
            },
        }
    }
    // print(" AT END AIR inst. count is {}\n", .{s.air.instructions.len});
    end_block(s, cur_block_inst);
}
fn get_block_statements(s: *AirState, n_index: Node.Index) ![]const Node.Index {
    const node = s.ast.nodes.items[n_index];
    switch (node) {
        .root => |root| {
            return s.ast.extra.items[root.statements_start..root.statements_end];
        },
        .block => |block| {
            return s.ast.extra.items[block.statements_start..block.statements_end];
        },
        else => return error.Unimplemented,
    }
}

fn air_gen_scoped_block(s: *AirState, n_index: Node.Index, new_scope: bool, dest_id: ?Air.StringIndex) !AirInst.IndexRef {
    if (new_scope) {
        try s.push_scope();
    }

    const s_indeces = try get_block_statements(s, n_index);
    const blk_inst = try start_new_block(s);

    _ = try air_gen_statements(s, s_indeces, blk_inst, dest_id);

    if (new_scope) {
        s.pop_scope();
    }

    return @enumFromInt(blk_inst);
}

pub fn new_air_state(ast: *Ast) !AirState {
    const s = AirState{
        .air = Air{
            .instructions = AirInst.List{},
            .extra = Air.ExtraList.init(ast.allocator),

            .string_index_map = std.StringHashMap(Air.StringIndex).init(ast.allocator),
            .string_len_map = std.AutoHashMap(Air.StringIndex, Air.StringLen).init(ast.allocator),
            .strings = std.ArrayList(u8).init(ast.allocator),

            .allocator = ast.allocator,
        },
        .scratch = std.ArrayList(u32).init(ast.allocator),
        .scratch_inst = MultiArrayList(AirInst){},

        .scopes = AirState.ScopeList.init(ast.allocator),

        .ast = ast,
    };
    return s;
}

pub fn air_gen(s: *AirState) !Air {
    _ = try air_gen_scoped_block(s, s.ast.root, true, null);

    print("\n=========== GENERATED AIR ===========\n", .{});
    _ = try print_air(&s.air, 0, @intCast(s.air.instructions.len), 0);
    print("\n===========               ===========\n", .{});
    return s.get_air();
}
