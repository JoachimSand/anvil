const std = @import("std");
const print = std.debug.print;

const TokenType = enum {
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

pub const Token: type = struct {
    type: TokenType,
    start: usize,
};

pub const Tokeniser = struct {
    buf: []const u8,
    cur_tok_start: usize,
    cur_pos: usize,
    cur_token: ?Token,

    fn get_token_type(tokeniser: *Tokeniser) !TokenType {
        var state: TokenState = .start;
        const buf = tokeniser.buf;
        tokeniser.cur_tok_start = tokeniser.cur_pos;

        while (tokeniser.cur_pos < tokeniser.buf.len) {
            var cur_char = buf[tokeniser.cur_pos];

            print("State is {any}, char is {u}\n", .{ state, cur_char });

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
                        tokeniser.cur_pos += 1;
                        return .l_paren;
                    },
                    ')' => {
                        tokeniser.cur_pos += 1;
                        return .r_paren;
                    },
                    '[' => {
                        tokeniser.cur_pos += 1;
                        return .l_bracket;
                    },
                    ']' => {
                        tokeniser.cur_pos += 1;
                        return .r_bracket;
                    },
                    '{' => {
                        tokeniser.cur_pos += 1;
                        return .l_brace;
                    },
                    '}' => {
                        tokeniser.cur_pos += 1;
                        return .r_brace;
                    },
                    ',' => {
                        tokeniser.cur_pos += 1;
                        return .comma;
                    },
                    ':' => {
                        tokeniser.cur_pos += 1;
                        return .colon;
                    },
                    ';' => {
                        tokeniser.cur_pos += 1;
                        return .semicolon;
                    },

                    '\n', '\t', ' ' => {
                        while (tokeniser.cur_pos < buf.len and (cur_char == ' ' or cur_char == '\t')) {
                            tokeniser.cur_pos += 1;
                            cur_char = buf[tokeniser.cur_pos];
                        }
                        tokeniser.cur_tok_start = tokeniser.cur_pos;
                        continue;
                    },
                    else => @panic("Start state not handled"),
                },
                .identifier_or_keyword => {
                    switch (cur_char) {
                        'A'...'Z', 'a'...'z', '_' => {},
                        else => {
                            // tok.type = .identifier;
                            // TODO: Check if keyword
                            if (std.mem.eql(u8, buf[tokeniser.cur_tok_start..tokeniser.cur_pos], "fn")) {
                                return .keyword_fn;
                            }

                            return .identifier;
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
                    else => return .integer_dec,
                },

                .pending_integer_bin => switch (cur_char) {
                    '0', '1' => {
                        state = .integer_bin;
                        tokeniser.cur_tok_start = tokeniser.cur_pos;
                    },
                    else => return error.InvalidIntegerLit,
                },
                .pending_integer_oct => switch (cur_char) {
                    '0'...'7' => {
                        state = .integer_oct;
                        tokeniser.cur_tok_start = tokeniser.cur_pos;
                    },
                    else => return error.InvalidIntegerLit,
                },
                .pending_integer_hex => switch (cur_char) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        state = .integer_hex;
                        tokeniser.cur_tok_start = tokeniser.cur_pos;
                    },
                    else => return error.InvalidIntegerLit,
                },

                .integer_bin => switch (cur_char) {
                    '0', '1', '_' => {},
                    else => return .integer_bin,
                },
                .integer_oct => switch (cur_char) {
                    '0'...'7', '_' => {},
                    else => return .integer_oct,
                },
                .integer_hex => switch (cur_char) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    else => return .integer_hex,
                },

                .equal => return equals_or_single(cur_char, .equal2, .equal),
                .plus => return equals_or_single(cur_char, .plus_equal, .plus),
                .minus => {
                    switch (cur_char) {
                        '>' => {
                            tokeniser.cur_pos += 1;
                            return .minus_arrow;
                        },
                        '=' => {
                            tokeniser.cur_pos += 1;
                            return .minus_equal;
                        },
                        else => return .minus,
                    }
                },
                .asterisk => return equals_or_single(cur_char, .asterisk_equal, .asterisk),
                .slash => {
                    switch (cur_char) {
                        '=' => {
                            tokeniser.cur_pos += 1;
                            return .slash_equal;
                        },
                        '/' => {
                            // line comment
                            state = .slash2;
                        },
                        else => return .slash,
                    }
                },
                .slash2 => {
                    while (tokeniser.cur_pos < buf.len - 1 and cur_char != '\n') {
                        tokeniser.cur_pos += 1;
                        cur_char = buf[tokeniser.cur_pos];
                    }
                    tokeniser.cur_tok_start = tokeniser.cur_pos;
                    state = .start;
                    continue;
                },

                .pipe => {
                    switch (cur_char) {
                        '=' => {
                            tokeniser.cur_pos += 1;
                            return .pipe_equal;
                        },
                        '|' => {
                            tokeniser.cur_pos += 1;
                            return .pipe2;
                        },
                        else => return .pipe2,
                    }
                },
                .ampersand => {
                    switch (cur_char) {
                        '=' => {
                            tokeniser.cur_pos += 1;
                            return .ampersand_equal;
                        },
                        '&' => {
                            tokeniser.cur_pos += 1;
                            return .ampersand2;
                        },
                        else => return .ampersand,
                    }
                },
                .caret => return equals_or_single(cur_char, .caret_equal, .caret),
                .not => return equals_or_single(cur_char, .not_equal, .not),

                .r_arrow => {
                    switch (cur_char) {
                        '>' => {
                            state = .r_arrow2;
                        },
                        '=' => {
                            tokeniser.cur_pos += 1;
                            return .r_arrow_equal;
                        },
                        else => return .r_arrow,
                    }
                },

                .r_arrow2 => {
                    if (cur_char == '=') {
                        tokeniser.cur_pos += 1;
                        return .r_arrow2_equal;
                    } else {
                        return .r_arrow2;
                    }
                },

                .l_arrow => {
                    switch (cur_char) {
                        '<' => {
                            state = .l_arrow2;
                        },
                        '=' => {
                            tokeniser.cur_pos += 1;
                            return .l_arrow_equal;
                        },
                        else => return .l_arrow,
                    }
                },

                .l_arrow2 => {
                    if (cur_char == '=') {
                        tokeniser.cur_pos += 1;
                        return .l_arrow2_equal;
                    } else {
                        return .l_arrow2;
                    }
                },

                .dot => {
                    switch (cur_char) {
                        '.' => {
                            tokeniser.cur_pos += 1;
                            return .dot2;
                        },
                        '*' => {
                            tokeniser.cur_pos += 1;
                            return .dot_asterisk;
                        },
                        else => return .dot,
                    }
                },
            }
            tokeniser.cur_pos += 1;
        }

        // We reached the end of input without emitting a token.
        // Use the current state to emit one if possible.
        switch (state) {
            .start => return error.EndOfInput,
            .identifier_or_keyword => return .identifier,
            .integer_unknown => return error.UnknownInteger,

            .pending_integer_bin => return error.InvalidIntegerLit,
            .pending_integer_oct => return error.InvalidIntegerLit,
            .pending_integer_hex => return error.InvalidIntegerLit,

            .integer_bin => return .integer_bin,
            .integer_oct => return .integer_oct,
            .integer_hex => return .integer_hex,
            .integer_dec => return .integer_dec,

            .equal => return .equal,
            .plus => return .plus,
            .minus => return .minus,
            .asterisk => return .asterisk,
            .slash => return .slash,
            .slash2 => return error.EndOfInput,
            .pipe => return .pipe,
            .ampersand => return .ampersand,
            .caret => return .caret,
            .not => return .not,
            .l_arrow => return .l_arrow,
            .r_arrow => return .r_arrow,
            .l_arrow2 => return .l_arrow2,
            .r_arrow2 => return .r_arrow2,
            .dot => return .dot,
        }
    }

    pub fn next_token(tokeniser: *Tokeniser) !Token {
        if (tokeniser.cur_token) |token| {
            tokeniser.cur_token = null;
            return token;
        }

        if (tokeniser.cur_pos >= tokeniser.buf.len) {
            return error.EndOfInput;
        }

        var tok_type = try tokeniser.get_token_type();
        return Token{
            .type = tok_type,
            .start = tokeniser.cur_tok_start,
        };
    }

    pub fn peek_token(tokeniser: *Tokeniser) !Token {
        if (tokeniser.cur_token) |token| {
            return token;
        }

        if (tokeniser.cur_pos >= tokeniser.buf.len) {
            return error.EndOfInput;
        }

        var tok_type = try tokeniser.get_token_type();
        const token = Token{
            .type = tok_type,
            .start = tokeniser.cur_tok_start,
        };

        tokeniser.cur_token = token;
        return token;
    }
};
