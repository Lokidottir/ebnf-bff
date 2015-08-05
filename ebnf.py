import pyparsing as P

def synt_except(factor, exception):
    """
        return a parser that will match factor unless
        it also matches exception
    """
    def on_fail(s, loc, expr, err):
        """
            if the wrapper fails, it succeeded! boolean logic!
        """
        return

    def on_parse(s, loc, toks):
        """
            if the wrapper succeeded then we need to throw an error
            becuase it's not allowed to do that in a syntax exception
        """
        raise P.ParseSyntaxException(s, loc, "matched a syntax exception" % (s[loc]))

    except_wrap = (exception.copy()).setFailAction(on_fail).setParseAction(on_parse)
    return except_wrap & factor

def syntax():
    """
        This function is a near 1-to-1 translation of the EBNF grammar (as
        described in ISO14977) to a pyparsing parser.
    """
    """
        Define the legal characters in EBNF
    """
    letter             = P.Regex("[a-zA-Z]")("letter")
    digit              = P.Regex("[0-1]")("decimal digit")
    concat_sep         = P.Literal(",")("concatenate symbol")
    assign             = P.Literal("=")("defining symbol")
    or_sep             = (P.Literal("|") | P.Literal("/") | P.Literal("!"))("definition separator symbol")
    comment_start      = P.Literal("(*")("start comment symbol")
    comment_end        = P.Literal("*)")("end comment symbol")
    group_begin        = P.Literal("(")("start group symbol")
    group_end          = P.Literal(")")("end group symbol")
    option_begin       = (P.Literal("[") | P.Literal("(/"))("start option symbol")
    option_end         = (P.Literal("]") | P.Literal("/)"))("end option symbol")
    repeat_begin       = (P.Literal("{") | P.Literal("(:"))("start repeat symbol")
    repeat_end         = (P.Literal("}") | P.Literal(":)"))("end repeat symbol")
    exception          = P.Literal("-")("except symbol")
    quote_single       = P.Literal("'")("first quote symbol")
    quote_double       = P.Literal("\"")("second quote symbol")
    repeat_n           = P.Literal("*")("repetition symbol")
    special_sequence   = P.Literal("?")("special sequence symbol")
    termination_symbol = (P.Literal(";") | P.Literal("."))("terminator symbol")
    other_chars        = P.Regex("[\\ :+_%@&#$<>|`~\\\\]")("other characters")
    space              = P.Literal(" ")("space character")
    tab                = P.Literal("\t")("horizontal tabulation character")
    vert_tab           = P.Literal("\v")("vertical tabulation character")
    newline            = P.OneOrMore(P.Literal("\n"))("new line")
    formfeed           = P.OneOrMore(P.Literal("\f"))("form feed")

    # Non-standard extention wrapped by special sequence
    regex_begin        = P.Literal("?/")("start regex symbol")
    regex_end          = P.Literal("/?")("end regex symbol")

    """
        Define part 2 as specified in the EBNF grammar
    """
    terminal_char = (letter | digit | concat_sep | assign | or_sep
                    | comment_start | comment_end | group_begin
                    | group_end | option_begin | option_end | repeat_begin
                    | repeat_end | exception | quote_single | quote_double
                    | repeat_n | special_sequence | termination_symbol
                    | other_chars | regex_begin | regex_end)("terminal character")

    terminal_string = P.Forward()("terminal string")

    singleq_terminal_char = synt_except(terminal_char, quote_single)("first terminal character")
    doubleq_terminal_char = synt_except(terminal_char, quote_double)("second terminal character")

    no_gaps = (synt_except(terminal_char, (quote_single | quote_double)) | terminal_string)("gap free symbol")

    terminal_string <<= ((quote_single + P.OneOrMore(singleq_terminal_char) + quote_single) |
                         (quote_double + P.OneOrMore(doubleq_terminal_char) + quote_double))("terminal string")

    gap_sep = (space | tab | vert_tab | newline | formfeed)("gap seperator")

    synt = (P.ZeroOrMore(gap_sep) + no_gaps + P.ZeroOrMore(gap_sep) +
            P.ZeroOrMore(no_gaps + P.ZeroOrMore(gap_sep)))("syntax")

    """
        Define part 3 as specified in the EBNF grammar
    """

    nocomm_sym = synt_except(terminal_char, (digit))("commentless symbol")

    grammar = None
    return grammar

def main():
    try:
        # ~P.Literal("Green")("Green") |
        gram = syntax()
        v = P.Forward()
        print("vroop")
        v <<= synt_except((P.Literal("A") | P.Literal("B") | P.Literal("C")), (P.Literal("B") | P.Literal("C")))
        x = v.parseString("BBAAABCAAAAB")
        print(x.asList())
        print(x.asDict())
    except P.ParseException as details:
        print(details)
    except P.ParseSyntaxException as details:
        print(details)

if __name__ == "__main__":
    main()
