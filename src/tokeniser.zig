const std = @import("std");
const print = std.debug.print;

pub const TokenType = enum {
    identifier,
    integer_bin,
    integer_oct,
    integer_hex,
    integer_dec,

    equal, // "="
    equal2, // "=="
    plus, // "+"
    plus_equal, // "+="
    minus, // "-"
    minus_equal, // "-="
    asterisk, // "*"
    asterisk_equal, // "*="
    slash, // "/"
    slash_equal, // "/="
    pipe, // "|"
    pipe2, // "||"
    pipe_equal, // "|="
    ampersand, // "&"
    ampersand2, // "&&"
    ampersand_equal, // "&="
    caret, // "^"
    caret_equal, // "^="
    not, // "!"
    not_equal, // "!="
    l_arrow, // "<"
    r_arrow, // ">"
    l_arrow2, // "<<"
    r_arrow2, // ">>"
    r_arrow_equal, // ">="
    l_arrow_equal, // "<="
    l_arrow2_equal, // "<<="
    r_arrow2_equal, // ">>="
    minus_arrow, // "->"
    l_paren, // "("
    r_paren, // ")"
    l_bracket, // "["
    r_bracket, // "]"
    l_brace, // "{"
    r_brace, // "}"
    comma, // ","
    dot, // "."
    dot2, // ".."
    dot_asterisk, // ".*"
    colon, // ":"
    semicolon, // ";"

    keyword_fn,
    keyword_or,
    keyword_and,
};

// Approx. subset of token types. Enumerates the possible characters the tokeniser
// could have gone through without emitting a token.
const TokenState = enum {
    start,
    identifier_or_keyword,
    integer_unknown,
    integer_dec,
    integer_bin,
    integer_oct,
    integer_hex,
    pending_integer_bin,
    pending_integer_oct,
    pending_integer_hex,

    equal, // "="
    plus, // "+"
    minus, // "-"
    asterisk, // "*"
    slash, // "/"
    slash2, // "//"
    pipe, // "|"
    ampersand, // "&"
    caret, // "^"
    not, // "!"
    l_arrow, // "<"
    r_arrow, // ">"
    l_arrow2, // "<<"
    r_arrow2, // ">>"
    dot, // "."
};

inline fn equals_or_single(cur_char: u8, equals_tok: TokenType, single_tok: TokenType) TokenType {
    return if (cur_char == '=') equals_tok else single_tok;
}

pub const SourceIndex = u32;

pub const Token = struct {
    type: TokenType,
    start: SourceIndex,
};

pub const TokenFull: type = struct {
    type: TokenType,
    start: SourceIndex,
    end: SourceIndex,

    fn init(ttype: TokenType, start: SourceIndex, end: SourceIndex) TokenFull {
        return TokenFull{ .type = ttype, .start = start, .end = end };
    }
};

pub const Tokeniser = struct {
    src: []const u8,
    cur_tok_start: SourceIndex,
    cur_pos: SourceIndex,
    cur_token: ?TokenFull,

    pub fn next_token(tokeniser: *Tokeniser) !TokenFull {
        if (tokeniser.cur_token) |token| {
            tokeniser.cur_token = null;
            return token;
        }

        if (tokeniser.cur_pos >= tokeniser.src.len) {
            return error.EndOfInput;
        }

        const tok = try tokenise(tokeniser.src, tokeniser.cur_pos);
        tokeniser.cur_pos = tok.end;
        return tok;
    }

    pub fn peek_token(tokeniser: *Tokeniser) !TokenFull {
        if (tokeniser.cur_token) |token| {
            return token;
        }

        if (tokeniser.cur_pos >= tokeniser.src.len) {
            return error.EndOfInput;
        }

        const tok = try tokenise(tokeniser.src, tokeniser.cur_pos);
        tokeniser.cur_token = tok;
        return tok;
    }
};

fn tokenise(src: []const u8, start_from: SourceIndex) !TokenFull {
    var state: TokenState = .start;

    var start = start_from;
    var pos = start;

    while (pos < src.len) {
        var cur_char = src[pos];

        // print("State is {any}, char is {u}\n", .{ state, cur_char });

        switch (state) {
            .start => switch (cur_char) {
                'A'...'Z', 'a'...'z' => state = .identifier_or_keyword,
                '0' => {
                    state = .integer_unknown;
                },
                '1'...'9' => {
                    state = .integer_dec;
                },

                '=' => state = .equal,
                '+' => state = .plus,
                '-' => state = .minus,
                '*' => state = .asterisk,
                '/' => state = .slash,
                '|' => state = .pipe,
                '&' => state = .ampersand,
                '^' => state = .caret,
                '!' => state = .not,
                '<' => state = .l_arrow,
                '>' => state = .r_arrow,
                '.' => state = .dot,

                '(' => {
                    pos += 1;
                    return TokenFull.init(.l_paren, start, pos);
                },
                ')' => {
                    pos += 1;
                    return TokenFull.init(.r_paren, start, pos);
                },
                '[' => {
                    pos += 1;
                    return TokenFull.init(.l_bracket, start, pos);
                },
                ']' => {
                    pos += 1;
                    return TokenFull.init(.r_bracket, start, pos);
                },
                '{' => {
                    pos += 1;
                    return TokenFull.init(.l_brace, start, pos);
                },
                '}' => {
                    pos += 1;
                    return TokenFull.init(.r_brace, start, pos);
                },
                ',' => {
                    pos += 1;
                    return TokenFull.init(.comma, start, pos);
                },
                ':' => {
                    pos += 1;
                    return TokenFull.init(.colon, start, pos);
                },
                ';' => {
                    pos += 1;
                    return TokenFull.init(.semicolon, start, pos);
                },

                '\n', '\t', ' ' => {
                    while (pos < src.len and (cur_char == ' ' or cur_char == '\t')) {
                        pos += 1;
                        cur_char = src[pos];
                    }
                    start = pos;
                    if (pos >= src.len - 1) {
                        return error.EarlyTermination;
                    }

                    continue;
                },
                else => @panic("Start state not handled"),
            },
            .identifier_or_keyword => {
                switch (cur_char) {
                    'A'...'Z', 'a'...'z', '0'...'9', '_' => {},
                    else => {
                        // tok.type = .identifier;
                        // TODO: Check if keyword
                        const id_str = src[start..pos];

                        if (std.mem.eql(u8, id_str, "fn")) {
                            return TokenFull.init(.keyword_fn, start, pos);
                        } else if (std.mem.eql(u8, id_str, "and")) {
                            return TokenFull.init(.keyword_and, start, pos);
                        } else if (std.mem.eql(u8, id_str, "or")) {
                            return TokenFull.init(.keyword_or, start, pos);
                        }

                        return TokenFull.init(.identifier, start, pos);
                    },
                }
            },

            .integer_unknown => {
                switch (cur_char) {
                    'b' => {
                        state = .pending_integer_bin;
                    },
                    'o' => {
                        state = .pending_integer_oct;
                    },
                    'x' => {
                        state = .pending_integer_hex;
                    },
                    else => return error.InvalidIntegerLit,
                }
            },
            .integer_dec => switch (cur_char) {
                '0'...'9', '_' => {},
                else => return TokenFull.init(.integer_dec, start, pos),
            },

            .pending_integer_bin => switch (cur_char) {
                '0', '1' => {
                    state = .integer_bin;
                    start = pos;
                },
                else => return error.InvalidIntegerLit,
            },
            .pending_integer_oct => switch (cur_char) {
                '0'...'7' => {
                    state = .integer_oct;
                    start = pos;
                },
                else => return error.InvalidIntegerLit,
            },
            .pending_integer_hex => switch (cur_char) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    state = .integer_hex;
                    start = pos;
                },
                else => return error.InvalidIntegerLit,
            },

            .integer_bin => switch (cur_char) {
                '0', '1', '_' => {},
                else => return TokenFull.init(.integer_bin, start, pos),
            },
            .integer_oct => switch (cur_char) {
                '0'...'7', '_' => {},
                else => return TokenFull.init(.integer_oct, start, pos),
            },
            .integer_hex => switch (cur_char) {
                '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                else => return TokenFull.init(.integer_hex, start, pos),
            },

            .equal => return TokenFull.init(equals_or_single(cur_char, .equal2, .equal), start, pos),
            .plus => return TokenFull.init(equals_or_single(cur_char, .plus_equal, .plus), start, pos),
            .minus => {
                switch (cur_char) {
                    '>' => {
                        pos += 1;
                        return TokenFull.init(.minus_arrow, start, pos);
                    },
                    '=' => {
                        pos += 1;
                        return TokenFull.init(.minus_equal, start, pos);
                    },
                    else => return TokenFull.init(.minus, start, pos),
                }
            },
            .asterisk => return TokenFull.init(equals_or_single(cur_char, .asterisk_equal, .asterisk), start, pos),
            .slash => {
                switch (cur_char) {
                    '=' => {
                        pos += 1;
                        return TokenFull.init(.slash_equal, start, pos);
                    },
                    '/' => {
                        // line comment
                        state = .slash2;
                    },
                    else => return TokenFull.init(.slash, start, pos),
                }
            },
            .slash2 => {
                while (pos < src.len - 1 and cur_char != '\n') {
                    pos += 1;
                    cur_char = src[pos];
                }
                start = pos;
                state = .start;
                continue;
            },

            .pipe => {
                switch (cur_char) {
                    '=' => {
                        pos += 1;
                        return TokenFull.init(.pipe_equal, start, pos);
                    },
                    '|' => {
                        pos += 1;
                        return TokenFull.init(.pipe2, start, pos);
                    },
                    else => return TokenFull.init(.pipe2, start, pos),
                }
            },
            .ampersand => {
                switch (cur_char) {
                    '=' => {
                        pos += 1;
                        return TokenFull.init(.ampersand_equal, start, pos);
                    },
                    '&' => {
                        pos += 1;
                        return TokenFull.init(.ampersand2, start, pos);
                    },
                    else => return TokenFull.init(.ampersand, start, pos),
                }
            },
            .caret => return TokenFull.init(equals_or_single(cur_char, .caret_equal, .caret), start, pos),
            .not => return TokenFull.init(equals_or_single(cur_char, .not_equal, .not), start, pos),

            .r_arrow => {
                switch (cur_char) {
                    '>' => {
                        state = .r_arrow2;
                    },
                    '=' => {
                        pos += 1;
                        return TokenFull.init(.r_arrow_equal, start, pos);
                    },
                    else => return TokenFull.init(.r_arrow, start, pos),
                }
            },

            .r_arrow2 => {
                if (cur_char == '=') {
                    pos += 1;
                    return TokenFull.init(.r_arrow2_equal, start, pos);
                } else {
                    return TokenFull.init(.r_arrow2, start, pos);
                }
            },

            .l_arrow => {
                switch (cur_char) {
                    '<' => {
                        state = .l_arrow2;
                    },
                    '=' => {
                        pos += 1;
                        return TokenFull.init(.l_arrow_equal, start, pos);
                    },
                    else => return TokenFull.init(.l_arrow, start, pos),
                }
            },

            .l_arrow2 => {
                if (cur_char == '=') {
                    pos += 1;
                    return TokenFull.init(.l_arrow2_equal, start, pos);
                } else {
                    return TokenFull.init(.l_arrow2, start, pos);
                }
            },

            .dot => {
                switch (cur_char) {
                    '.' => {
                        pos += 1;
                        return TokenFull.init(.dot2, start, pos);
                    },
                    '*' => {
                        pos += 1;
                        return TokenFull.init(.dot_asterisk, start, pos);
                    },
                    else => return TokenFull.init(.dot, start, pos),
                }
            },
        }
        pos += 1;
    }

    // We reached the end of input without emitting a token.
    // Use the current state to emit one if possible.
    switch (state) {
        .start => return error.EndOfInput,
        .identifier_or_keyword => return TokenFull.init(.identifier, start, pos),
        .integer_unknown => return error.UnknownInteger,

        .pending_integer_bin => return error.InvalidIntegerLit,
        .pending_integer_oct => return error.InvalidIntegerLit,
        .pending_integer_hex => return error.InvalidIntegerLit,

        .integer_bin => return TokenFull.init(.integer_bin, start, pos),
        .integer_oct => return TokenFull.init(.integer_oct, start, pos),
        .integer_hex => return TokenFull.init(.integer_hex, start, pos),
        .integer_dec => return TokenFull.init(.integer_dec, start, pos),

        .equal => return TokenFull.init(.equal, start, pos),
        .plus => return TokenFull.init(.plus, start, pos),
        .minus => return TokenFull.init(.minus, start, pos),
        .asterisk => return TokenFull.init(.asterisk, start, pos),
        .slash => return TokenFull.init(.slash, start, pos),
        .slash2 => return error.EndOfInput,
        .pipe => return TokenFull.init(.pipe, start, pos),
        .ampersand => return TokenFull.init(.ampersand, start, pos),
        .caret => return TokenFull.init(.caret, start, pos),
        .not => return TokenFull.init(.not, start, pos),
        .l_arrow => return TokenFull.init(.l_arrow, start, pos),
        .r_arrow => return TokenFull.init(.r_arrow, start, pos),
        .l_arrow2 => return TokenFull.init(.l_arrow2, start, pos),
        .r_arrow2 => return TokenFull.init(.r_arrow2, start, pos),
        .dot => return TokenFull.init(.dot, start, pos),
    }
}

// // Convert a token to its full format (with an end field)
// // by trimming off trailing whitespace.
// fn token_to_full(token: Token, src: []const u8) TokenFull {
//     var end_pos = token.start;
//     var cur_char = src[end_pos];
//     while (cur_char != ' ' and cur_char != '\t' and end_pos + 1 < src.len) {
//         end_pos += 1;
//         cur_char = src[end_pos];
//     }
//     return TokenFull{
//         .type = token.type,
//         .start = token.start,
//         .end = end_pos,
//     };
// }

pub fn token_to_str(token: Token, src: []const u8) []const u8 {
    const full_tok = tokenise(src, token.start) catch unreachable;
    return src[full_tok.start..full_tok.end];
}

// fn tokenise(src: []const u8, start_pos: SourceIndex) TokenFull {
//     _ = start_pos;
//     _ = src;
// }
