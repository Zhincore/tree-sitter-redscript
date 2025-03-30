"use strict";
// Originally based on https://github.com/alex-pinkus/tree-sitter-swift.
const PRECS = {
  multiplication: 11,
  addition: 10,
  infix_operations: 9,
  nil_coalescing: 8,
  check: 7,
  prefix_operations: 7,
  comparison: 6,
  postfix_operations: 6,
  equality: 5,
  conjunction: 4,
  disjunction: 3,
  block: 2,
  loop: 1,
  keypath: 1,
  parameter_pack: 1,
  control_transfer: 0,
  as: -1,
  tuple: -1,
  if: -1,
  switch: -1,
  do: -1,
  fully_open_range: -1,
  range: -1,
  navigation: -1,
  expr: -1,
  ty: -1,
  call: -1,
  ternary: -2,
  try: -2,
  call_suffix: -2,
  range_suffix: -2,
  ternary_binary_suffix: -2,
  await: -2,
  assignment: -3,
  comment: -3,
  lambda: -3,
  regex: -4,
};

const DYNAMIC_PRECS = {
  call: 1,
};

const DEC_DIGITS = token(sep1(/[0-9]+/, /_+/));
const HEX_DIGITS = token(sep1(/[0-9a-fA-F]+/, /_+/));
const OCT_DIGITS = token(sep1(/[0-7]+/, /_+/));
const BIN_DIGITS = token(sep1(/[01]+/, /_+/));
const REAL_EXPONENT = token(seq(/[eE]/, optional(/[+-]/), DEC_DIGITS));
const HEX_REAL_EXPONENT = token(seq(/[pP]/, optional(/[+-]/), DEC_DIGITS));

var LEXICAL_IDENTIFIER;

if (tree_sitter_version_supports_emoji()) {
  LEXICAL_IDENTIFIER =
    /[_\p{XID_Start}\p{Emoji}&&[^0-9#*]](\p{EMod}|\x{FE0F}\x{20E3}?)?([_\p{XID_Continue}\p{Emoji}\x{200D}](\p{EMod}|\x{FE0F}\x{20E3}?)?)*/;
} else {
  LEXICAL_IDENTIFIER = /[_\p{XID_Start}][_\p{XID_Continue}]*/;
}

module.exports = grammar({
  name: "redscript",
  conflicts: ($) => [
    [$._attribute_argument],
    // Is `foo { ... }` a constructor invocation or function invocation?
    [$._simple_user_type, $._expression],
    // To support nested types A.B not being interpreted as `(navigation_expression ... (type_identifier)) (navigation_suffix)`
    [$.user_type],
    // How to tell the difference between Foo.bar(with:and:), and Foo.bar(with: smth, and: other)? You need GLR
    [$.value_argument],
    // `+(...)` is ambigously either "call the function produced by a reference to the operator `+`" or "use the unary
    // operator `+` on the result of the parenthetical expression."
    [$._additive_operator, $._prefix_unary_operator],
    [$._referenceable_operator, $._prefix_unary_operator],
    // a ? b : c () could be calling c(), or it could be calling a function that's produced by the result of
    // `(a ? b : c)`. We have a small hack to force it to be the former of these by intentionally introducing a
    // conflict.
    [$.call_suffix, $.expr_hack_at_ternary_binary_call_suffix],
    // Patterns, man
    [$._navigable_type_expression, $._case_pattern],
    [$._no_expr_pattern_already_bound, $._binding_pattern_no_expr],
  ],
  extras: ($) => [
    $.comment,
    $.multiline_comment,
    /\s+/, // Whitespace
  ],
  externals: ($) => [
    // Comments and raw strings are parsed in a custom scanner because they require us to carry forward state to
    // maintain symmetry. For instance, parsing a multiline comment requires us to increment a counter whenever we see
    // `/*`, and decrement it whenever we see `*/`. A standard grammar would only be able to exit the comment at the
    // first `*/` (like C does). Similarly, when you start a string with `##"`, you're required to include the same
    // number of `#` symbols to end it.
    $.multiline_comment,
    $.raw_str_part,
    $.raw_str_continuing_indicator,
    $.raw_str_end_part,
    // Because Swift doesn't have explicit semicolons, we also do some whitespace handling in a custom scanner. Line
    // breaks are _sometimes_ meaningful as the end of a statement: try to write `let foo: Foo let bar: Bar`, for
    // instance and the compiler will complain, but add either a newline or a semicolon and it's fine. We borrow the
    // idea from the Kotlin grammar that a newline is sometimes a "semicolon". By including `\n` in both `_semi` and
    // an anonymous `whitespace` extras, we _should_ be able to let the parser decide if a newline is meaningful. If the
    // parser sees something like `foo.bar(1\n)`, it knows that a "semicolon" would not be valid there, so it parses
    // that as whitespace. On the other hand, `let foo: Foo\n let bar: Bar` has a meaningful newline.
    // Unfortunately, we can't simply stop at that. There are some expressions and statements that remain valid if you
    // end them early, but are expected to be parsed across multiple lines. One particular nefarious example is a
    // function declaration, where you might have something like `func foo<A>(args: A) -> Foo throws where A: Hashable`.
    // This would still be a valid declaration even if it ended after the `)`, the `Foo`, or the `throws`, so a grammar
    // that simply interprets a newline as "sometimes a semi" would parse those incorrectly.
    // To solve that case, our custom scanner must do a bit of extra lookahead itself. If we're about to generate a
    // `_semi`, we advance a bit further to see if the next non-whitespace token would be one of these other operators.
    // If so, we ignore the `_semi` and just produce the operator; if not, we produce the `_semi` and let the rest of
    // the grammar sort it out. This isn't perfect, but it works well enough most of the time.
    $._implicit_semi,
    $._explicit_semi,
    // Every one of the below operators will suppress a `_semi` if we encounter it after a newline.
    $._arrow_operator_custom,
    $._dot_custom,
    $._conjunction_operator_custom,
    $._disjunction_operator_custom,
    $._nil_coalescing_operator_custom,
    $._eq_custom,
    $._eq_eq_custom,
    $._plus_then_ws,
    $._minus_then_ws,
    $._bang_custom,
    $._throws_keyword,
    $._rethrows_keyword,
    $.default_keyword,
    $.where_keyword,
    $["else"],
    $.catch_keyword,
    $._as_custom,
    $._as_quest_custom,
    $._as_bang_custom,
    $._async_keyword_custom,
    $._custom_operator,

    // Fake operator that will never get triggered, but follows the sequence of characters for `try!`. Tracked by the
    // custom scanner so that it can avoid triggering `$.bang` for that case.
    $._fake_try_bang,
  ],
  inline: ($) => [$._locally_permitted_modifiers],
  rules: {
    ////////////////////////////////
    // File Structure
    ////////////////////////////////
    source_file: ($) =>
      seq(
        optional($.shebang_line),
        optional($.module_declaration),
        optional(
          seq(
            $._top_level_statement,
            repeat(seq($._semi, $._top_level_statement)),
            optional($._semi),
          ),
        ),
      ),
    _semi: ($) => choice($._implicit_semi, $._explicit_semi, ";"),
    shebang_line: ($) => seq("#!", /[^\r\n]*/),
    ////////////////////////////////
    // Lexical Structure - https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html
    ////////////////////////////////
    comment: ($) => token(prec(PRECS.comment, seq("//", /.*/))),
    // Identifiers
    simple_identifier: ($) =>
      choice(
        LEXICAL_IDENTIFIER,
        /`[^\r\n` ]*`/,
        /\$[0-9]+/,
        token(seq("$", LEXICAL_IDENTIFIER)),
      ),
    identifier: ($) => sep1($.simple_identifier, $._dot),
    // Literals
    _basic_literal: ($) =>
      choice(
        $.integer_literal,
        $.hex_literal,
        $.oct_literal,
        $.bin_literal,
        $.real_literal,
        $.boolean_literal,
        $._string_literal,
        $.regex_literal,
        "null",
      ),
    real_literal: ($) =>
      token(
        choice(
          seq(DEC_DIGITS, REAL_EXPONENT),
          seq(optional(DEC_DIGITS), ".", DEC_DIGITS, optional(REAL_EXPONENT)),
          seq(
            "0x",
            HEX_DIGITS,
            optional(seq(".", HEX_DIGITS)),
            HEX_REAL_EXPONENT,
          ),
        ),
      ),
    integer_literal: ($) => token(seq(optional(/[1-9]/), DEC_DIGITS)),
    hex_literal: ($) => token(seq("0", /[xX]/, HEX_DIGITS)),
    oct_literal: ($) => token(seq("0", /[oO]/, OCT_DIGITS)),
    bin_literal: ($) => token(seq("0", /[bB]/, BIN_DIGITS)),
    boolean_literal: ($) => choice("true", "false"),
    // String literals
    _string_literal: ($) =>
      choice(
        $.line_string_literal,
        $.multi_line_string_literal,
        $.raw_string_literal,
      ),
    line_string_literal: ($) =>
      seq(
        '"',
        repeat(choice(field("text", $._line_string_content), $._interpolation)),
        '"',
      ),
    _line_string_content: ($) => choice($.line_str_text, $.str_escaped_char),
    line_str_text: ($) => /[^\\"]+/,
    str_escaped_char: ($) =>
      choice($._escaped_identifier, $._uni_character_literal),
    _uni_character_literal: ($) => seq("\\", "u", /\{[0-9a-fA-F]+\}/),
    multi_line_string_literal: ($) =>
      seq(
        '"""',
        repeat(
          choice(field("text", $._multi_line_string_content), $._interpolation),
        ),
        '"""',
      ),
    raw_string_literal: ($) =>
      seq(
        repeat(
          seq(
            field("text", $.raw_str_part),
            field("interpolation", $.raw_str_interpolation),
            optional($.raw_str_continuing_indicator),
          ),
        ),
        field("text", $.raw_str_end_part),
      ),
    raw_str_interpolation: ($) =>
      seq($.raw_str_interpolation_start, $._interpolation_contents, ")"),
    raw_str_interpolation_start: ($) => /\\#*\(/,
    _multi_line_string_content: ($) =>
      choice($.multi_line_str_text, $.str_escaped_char, '"'),
    _interpolation: ($) => seq("\\(", $._interpolation_contents, ")"),
    _interpolation_contents: ($) =>
      sep1(
        field(
          "interpolation",
          alias($.value_argument, $.interpolated_expression),
        ),
        ",",
      ),
    _escaped_identifier: ($) => /\\[0\\tnr"'\n]/,
    multi_line_str_text: ($) => /[^\\"]+/,
    // Based on https://gitlab.com/woolsweater/tree-sitter-swifter/-/blob/3d47c85bd47ce54cdf2023a9c0e01eb90adfcc1d/grammar.js#L1019
    // But required modifications to hit all of the cases in SE-354
    regex_literal: ($) =>
      choice(
        $._extended_regex_literal,
        $._multiline_regex_literal,
        $._oneline_regex_literal,
      ),

    _extended_regex_literal: ($) => /#\/((\/[^#])|[^\n])+\/#/,

    _multiline_regex_literal: ($) => seq(/#\/\n/, /(\/[^#]|[^/])*?\n\/#/),

    _oneline_regex_literal: ($) =>
      token(
        prec(
          PRECS.regex,
          seq(
            "/",
            token.immediate(/[^ \t\n]?[^/\n]*[^ \t\n/]/),
            token.immediate("/"),
          ),
        ),
      ),
    ////////////////////////////////
    // Types - https://docs.swift.org/swift-book/ReferenceManual/Types.html
    ////////////////////////////////
    type_annotation: ($) =>
      seq(":", field("type", $._possibly_implicitly_unwrapped_type)),
    _possibly_implicitly_unwrapped_type: ($) =>
      seq($._type, optional(token.immediate("!"))),
    _type: ($) =>
      prec.right(
        PRECS.ty,
        seq(optional($.type_modifiers), field("name", $._unannotated_type)),
      ),
    _unannotated_type: ($) =>
      prec.right(
        PRECS.ty,
        choice(
          $.user_type,
          $.function_type,
          $.array_type,
          $.optional_type,
          $.metatype,
        ),
      ),
    // The grammar just calls this whole thing a `type-identifier` but that's a bit confusing.
    user_type: ($) => sep1($._simple_user_type, $._dot),
    _simple_user_type: ($) =>
      prec.right(
        PRECS.ty,
        seq(
          alias($.simple_identifier, $.type_identifier),
          optional($.type_arguments),
        ),
      ),
    function_type: ($) =>
      seq(
        field("params", choice($._unannotated_type)),
        $._arrow_operator,
        field("return_type", $._type),
      ),
    array_type: ($) => seq("[", field("element", $._type), "]"),
    optional_type: ($) =>
      prec.left(
        seq(
          field("wrapped", choice($.user_type, $.array_type)),
          repeat1(alias($._immediate_quest, "?")),
        ),
      ),
    metatype: ($) => seq($._unannotated_type, ".", choice("Type", "Protocol")),
    _quest: ($) => "?",
    _immediate_quest: ($) => token.immediate("?"),
    ////////////////////////////////
    // Expressions - https://docs.swift.org/swift-book/ReferenceManual/Expressions.html
    ////////////////////////////////
    _expression: ($) =>
      prec(
        PRECS.expr,
        choice(
          $.simple_identifier,
          $._unary_expression,
          $._binary_expression,
          $.ternary_expression,
          $._primary_expression,
          $.if_statement,
          $.switch_statement,
          $.assignment,
          $.value_parameter_pack,
          $.value_pack_expansion,
          seq($._expression, alias($._immediate_quest, "?")),
        ),
      ),
    // Unary expressions
    _unary_expression: ($) =>
      choice(
        $.call_expression,
        $.postfix_expression,
        $.constructor_expression,
        $.navigation_expression,
        $.prefix_expression,
        $.as_expression,
      ),
    postfix_expression: ($) =>
      prec.left(
        PRECS.postfix_operations,
        seq(
          field("target", $._expression),
          field("operation", $._postfix_unary_operator),
        ),
      ),
    constructor_expression: ($) =>
      prec(
        PRECS.call,
        seq(
          "new",
          field("constructed_type", choice($.array_type, $.user_type)),
          $.constructor_suffix,
        ),
      ),
    navigation_expression: ($) =>
      prec.left(
        PRECS.navigation,
        seq(
          field("target", choice($._navigable_type_expression, $._expression)),
          field("suffix", $.navigation_suffix),
        ),
      ),
    _navigable_type_expression: ($) => choice($.user_type, $.array_type),
    prefix_expression: ($) =>
      prec.left(
        PRECS.prefix_operations,
        seq(
          field("operation", $._prefix_unary_operator),
          field("target", $._expression),
        ),
      ),
    as_expression: ($) =>
      prec.left(
        PRECS.as,
        seq(
          field("expr", $._expression),
          $.as_operator,
          field("type", $._type),
        ),
      ),
    // Binary expressions
    _binary_expression: ($) =>
      choice(
        $.multiplicative_expression,
        $.additive_expression,
        $.infix_expression,
        $.nil_coalescing_expression,
        $.check_expression,
        $.equality_expression,
        $.comparison_expression,
        $.conjunction_expression,
        $.disjunction_expression,
        $.bitwise_operation,
      ),
    multiplicative_expression: ($) =>
      prec.left(
        PRECS.multiplication,
        seq(
          field("lhs", $._expression),
          field("op", $._multiplicative_operator),
          field("rhs", $._expression),
        ),
      ),
    additive_expression: ($) =>
      prec.left(
        PRECS.addition,
        seq(
          field("lhs", $._expression),
          field("op", $._additive_operator),
          field("rhs", $._expression),
        ),
      ),
    infix_expression: ($) =>
      prec.left(
        PRECS.infix_operations,
        seq(
          field("lhs", $._expression),
          field("op", $.custom_operator),
          field("rhs", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    nil_coalescing_expression: ($) =>
      prec.right(
        PRECS.nil_coalescing,
        seq(
          field("value", $._expression),
          $._nil_coalescing_operator,
          field("if_nil", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    check_expression: ($) =>
      prec.left(
        PRECS.check,
        seq(
          field("target", $._expression),
          field("op", $._is_operator),
          field("type", $._type),
        ),
      ),
    comparison_expression: ($) =>
      prec.left(
        seq(
          field("lhs", $._expression),
          field("op", $._comparison_operator),
          field("rhs", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    equality_expression: ($) =>
      prec.left(
        PRECS.equality,
        seq(
          field("lhs", $._expression),
          field("op", $._equality_operator),
          field("rhs", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    conjunction_expression: ($) =>
      prec.left(
        PRECS.conjunction,
        seq(
          field("lhs", $._expression),
          field("op", $._conjunction_operator),
          field("rhs", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    disjunction_expression: ($) =>
      prec.left(
        PRECS.disjunction,
        seq(
          field("lhs", $._expression),
          field("op", $._disjunction_operator),
          field("rhs", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    bitwise_operation: ($) =>
      prec.left(
        seq(
          field("lhs", $._expression),
          field("op", $._bitwise_binary_operator),
          field("rhs", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    custom_operator: ($) => choice(token(/[\/]+[*]+/), $._custom_operator),
    // Suffixes
    navigation_suffix: ($) =>
      seq(
        $._dot,
        field("suffix", choice($.simple_identifier, $.integer_literal)),
      ),
    call_suffix: ($) => prec(PRECS.call_suffix, $.value_arguments),
    constructor_suffix: ($) =>
      prec(
        PRECS.call_suffix,
        alias($._constructor_value_arguments, $.value_arguments),
      ),
    _constructor_value_arguments: ($) =>
      seq("(", optional(sep1($.value_argument, ",")), ")"),
    type_arguments: ($) => prec.left(seq("<", sep1($._type, ","), ">")),
    value_arguments: ($) =>
      seq(
        choice(
          seq("(", optional(sep1($.value_argument, ",")), ")"),
          seq("[", optional(sep1($.value_argument, ",")), "]"),
        ),
      ),
    value_argument_label: ($) =>
      prec.left(
        choice(
          $.simple_identifier,
          alias("if", $.simple_identifier),
          alias("switch", $.simple_identifier),
        ),
      ),
    value_argument: ($) =>
      prec.left(
        seq(
          optional($.type_modifiers),
          choice(
            repeat1(
              seq(field("reference_specifier", $.value_argument_label), ":"),
            ),
            seq(
              optional(seq(field("name", $.value_argument_label), ":")),
              field("value", $._expression),
            ),
          ),
        ),
      ),
    ternary_expression: ($) =>
      prec.right(
        PRECS.ternary,
        seq(
          field("condition", $._expression),
          $._quest,
          field("if_true", $._expression),
          ":",
          field("if_false", $._expr_hack_at_ternary_binary_suffix),
        ),
      ),
    _expr_hack_at_ternary_binary_suffix: ($) =>
      prec.left(
        PRECS.ternary_binary_suffix,
        choice(
          $._expression,
          alias($.expr_hack_at_ternary_binary_call, $.call_expression),
        ),
      ),
    expr_hack_at_ternary_binary_call: ($) =>
      seq(
        $._expression,
        alias($.expr_hack_at_ternary_binary_call_suffix, $.call_suffix),
      ),
    expr_hack_at_ternary_binary_call_suffix: ($) =>
      prec(PRECS.call_suffix, $.value_arguments),
    call_expression: ($) =>
      prec(
        PRECS.call,
        prec.dynamic(
          DYNAMIC_PRECS.call,
          choice(
            seq($._expression, $.call_suffix),
            seq($.identifier, optional($.type_arguments), $.call_suffix),
          ),
        ),
      ),
    _primary_expression: ($) =>
      choice(
        $._basic_literal,
        $._special_literal,
        $._playground_literal,
        $.array_literal,
        $.self_expression,
        $.super_expression,
        $._referenceable_operator,
      ),
    array_literal: ($) =>
      seq(
        "[",
        optional(sep1(field("element", $._expression), ",")),
        optional(","),
        "]",
      ),
    _special_literal: ($) =>
      choice(
        "#file",
        "#fileID",
        "#filePath",
        "#line",
        "#column",
        "#function",
        "#dsohandle",
      ),
    _playground_literal: ($) =>
      seq(
        choice("#colorLiteral", "#fileLiteral", "#imageLiteral"),
        "(",
        sep1(seq($.simple_identifier, ":", $._expression), ","),
        ")",
      ),
    self_expression: ($) => "this",
    super_expression: ($) => "super",
    _else_options: ($) => choice($._block, $.if_statement),
    if_statement: ($) =>
      prec.right(
        PRECS["if"],
        seq(
          "if",
          field("condition", $._expression),
          $._block,
          optional(seq($["else"], $._else_options)),
        ),
      ),
    switch_statement: ($) =>
      prec.right(
        PRECS["switch"],
        seq(
          "switch",
          field("expr", $._expression),
          "{",
          repeat($.switch_entry),
          "}",
        ),
      ),
    switch_entry: ($) =>
      seq(
        optional($.modifiers),
        choice(
          seq(
            "case",
            seq(
              $.switch_pattern,
              optional(seq($.where_keyword, $._expression)),
            ),
            repeat(seq(",", $.switch_pattern)),
          ),
          $.default_keyword,
        ),
        ":",
        $.statements,
        optional("fallthrough"),
      ),
    switch_pattern: ($) => alias($._binding_pattern_with_expr, $.pattern),
    _assignment_and_operator: ($) =>
      choice("+=", "-=", "*=", "/=", "%=", $._equal_sign),
    _equality_operator: ($) => choice("!=", $._eq_eq),
    _comparison_operator: ($) => choice("<", ">", "<=", ">="),
    _is_operator: ($) => "is",
    _additive_operator: ($) =>
      choice(
        alias($._plus_then_ws, "+"),
        alias($._minus_then_ws, "-"),
        "+",
        "-",
      ),
    // The `/` operator conflicts with a regex literal (which itself appears to conflict with a
    // comment, for some reason), so we must give it equivalent token precedence.
    _multiplicative_operator: ($) =>
      choice("*", alias(token(prec(PRECS.regex, "/")), "/"), "%"),
    as_operator: ($) => choice($._as, $._as_quest, $._as_bang),
    _prefix_unary_operator: ($) =>
      prec.right(choice("-", "+", $.bang, "&", "~", $._dot, $.custom_operator)),
    _bitwise_binary_operator: ($) => choice("&", "|", "^", "<<", ">>"),
    _postfix_unary_operator: ($) => choice($.bang),
    directly_assignable_expression: ($) => $._expression,

    ////////////////////////////////
    // Statements - https://docs.swift.org/swift-book/ReferenceManual/Statements.html
    ////////////////////////////////
    statements: ($) =>
      prec.left(
        // Left precedence is required in switch statements
        seq(
          $._local_statement,
          repeat(seq($._semi, $._local_statement)),
          optional($._semi),
        ),
      ),
    _local_statement: ($) =>
      choice(
        $._expression,
        $._local_declaration,
        $.control_transfer_statement,
        $.for_statement,
        $.while_statement,
        $.if_statement,
        $.switch_statement,
      ),
    _top_level_statement: ($) =>
      choice(
        // $._expression, // TODO: do we really want to ban expressions at the top level? fixes issue with `module`
        $._global_declaration,
        $.for_statement,
        $.while_statement,
        $.if_statement,
        $.switch_statement,
      ),
    _block: ($) => prec(PRECS.block, seq("{", optional($.statements), "}")),
    for_statement: ($) =>
      prec(
        PRECS.loop,
        seq(
          "for",
          field("item", alias($._binding_pattern_no_expr, $.pattern)),
          "in",
          field("collection", $._expression),
          $._block,
        ),
      ),
    while_statement: ($) =>
      prec(
        PRECS.loop,
        seq(
          "while",
          field("condition", $._expression),
          "{",
          optional($.statements),
          "}",
        ),
      ),
    control_transfer_statement: ($) =>
      choice(
        prec.right(
          PRECS.control_transfer,
          seq(
            $._optionally_valueful_control_keyword,
            field("result", optional($._expression)),
          ),
        ),
      ),
    _optionally_valueful_control_keyword: ($) => choice("return", "break"),
    assignment: ($) =>
      prec.left(
        PRECS.assignment,
        seq(
          field("target", $.directly_assignable_expression),
          field("operator", $._assignment_and_operator),
          field("result", $._expression),
        ),
      ),
    value_parameter_pack: ($) =>
      prec.left(PRECS.parameter_pack, seq("each", $._expression)),
    value_pack_expansion: ($) =>
      prec.left(PRECS.parameter_pack, seq("repeat", $._expression)),
    availability_condition: ($) =>
      seq(
        choice("#available", "#unavailable"),
        "(",
        sep1($._availability_argument, ","),
        ")",
      ),
    _availability_argument: ($) =>
      choice(seq($.identifier, sep1($.integer_literal, ".")), "*"),
    ////////////////////////////////
    // Declarations - https://docs.swift.org/swift-book/ReferenceManual/Declarations.html
    ////////////////////////////////
    _global_declaration: ($) =>
      choice(
        $.import_declaration,
        $.property_declaration,
        $.function_declaration,
        $.class_declaration,
      ),
    _type_level_declaration: ($) =>
      choice(
        $.import_declaration,
        $.property_declaration,
        $.function_declaration,
        $.class_declaration,
      ),
    _local_declaration: ($) =>
      choice(
        alias($._local_property_declaration, $.property_declaration),
        alias($._local_function_declaration, $.function_declaration),
        alias($._local_class_declaration, $.class_declaration),
      ),
    _local_property_declaration: ($) =>
      seq(
        optional($._locally_permitted_modifiers),
        $._modifierless_property_declaration,
      ),
    _local_function_declaration: ($) =>
      seq(
        optional($._locally_permitted_modifiers),
        $._modifierless_function_declaration,
      ),
    _local_class_declaration: ($) =>
      seq(
        optional($._locally_permitted_modifiers),
        $._modifierless_class_declaration,
      ),
    module_declaration: ($) => seq("module", $.identifier),
    import_declaration: ($) => seq("import", $.identifier),
    property_declaration: ($) =>
      seq(optional($.modifiers), $._modifierless_property_declaration),
    _modifierless_property_declaration: ($) =>
      prec.right(
        seq(
          $.value_binding_pattern,
          sep1($._single_modifierless_property_declaration, ","),
        ),
      ),
    _single_modifierless_property_declaration: ($) =>
      prec.left(
        seq(
          field("name", alias($._no_expr_pattern_already_bound, $.pattern)),
          optional($.type_annotation),
          optional(seq($._equal_sign, field("value", $._expression))),
        ),
      ),
    function_declaration: ($) =>
      prec.right(
        seq($._bodyless_function_declaration, field("body", $.function_body)),
      ),
    _modifierless_function_declaration: ($) =>
      prec.right(
        seq(
          $._modifierless_function_declaration_no_body,
          field("body", $.function_body),
        ),
      ),
    _bodyless_function_declaration: ($) =>
      seq(optional($.modifiers), $._modifierless_function_declaration_no_body),
    _modifierless_function_declaration_no_body: ($) =>
      prec.right(
        seq(
          $._non_constructor_function_decl,
          optional($.type_parameters),
          $._function_value_parameters,
          optional(
            seq(
              $._arrow_operator,
              field("return_type", $._possibly_implicitly_unwrapped_type),
            ),
          ),
        ),
      ),
    function_body: ($) => $._block,
    class_declaration: ($) =>
      seq(optional($.modifiers), $._modifierless_class_declaration),
    _modifierless_class_declaration: ($) =>
      prec.right(
        choice(
          seq(
            field("declaration_kind", choice("class", "struct", "enum")),
            field("name", alias($.simple_identifier, $.type_identifier)),
            optional($.type_parameters),
            optional(seq("extends", field("inherits_from", $.user_type))),
            field("body", $.class_body),
          ),
        ),
      ),
    class_body: ($) => seq("{", optional($._class_member_declarations), "}"),
    type_parameters: ($) => seq("<", sep1($.type_parameter, ","), ">"),
    type_parameter: ($) =>
      seq(
        optional($.type_parameter_modifiers),
        $._type_parameter_possibly_packed,
        optional(seq(":", $._type)),
      ),
    _type_parameter_possibly_packed: ($) =>
      alias($.simple_identifier, $.type_identifier),
    _class_member_separator: ($) => choice($._semi, $.multiline_comment),
    _class_member_declarations: ($) =>
      seq(
        sep1($._type_level_declaration, $._class_member_separator),
        optional($._class_member_separator),
      ),
    _function_value_parameters: ($) =>
      repeat1(seq("(", optional(sep1($._function_value_parameter, ",")), ")")),
    _function_value_parameter: ($) =>
      seq(
        optional($.attribute),
        $.parameter,
        optional(seq($._equal_sign, field("default_value", $._expression))),
      ),
    parameter: ($) =>
      seq(
        field("name", $.simple_identifier),
        ":",
        optional($.parameter_modifiers),
        field("type", $._possibly_implicitly_unwrapped_type),
      ),
    _non_constructor_function_decl: ($) =>
      seq("func", field("name", $.simple_identifier)),
    _referenceable_operator: ($) =>
      choice(
        $.custom_operator,
        $._comparison_operator,
        $._additive_operator,
        $._multiplicative_operator,
        $._equality_operator,
        $._comparison_operator,
        $._assignment_and_operator,
        $.bang,
        "~",
        "|",
        "^",
        "<<",
        ">>",
        "&",
      ),
    // Hide the fact that certain symbols come from the custom scanner by aliasing them to their
    // string variants. This keeps us from having to see them in the syntax tree (which would be
    // noisy) but allows callers to refer to them as nodes by their text form like with any
    // operator.
    _equal_sign: ($) => alias($._eq_custom, "="),
    _eq_eq: ($) => alias($._eq_eq_custom, "=="),
    _dot: ($) => alias($._dot_custom, "."),
    _arrow_operator: ($) => alias($._arrow_operator_custom, "->"),
    _conjunction_operator: ($) => alias($._conjunction_operator_custom, "&&"),
    _disjunction_operator: ($) => alias($._disjunction_operator_custom, "||"),
    _nil_coalescing_operator: ($) =>
      alias($._nil_coalescing_operator_custom, "??"),
    _as: ($) => alias($._as_custom, "as"),
    _as_quest: ($) => alias($._as_quest_custom, "as?"),
    _as_bang: ($) => alias($._as_bang_custom, "as!"),
    bang: ($) => choice($._bang_custom, "!"),
    enum_class_body: ($) =>
      seq("{", repeat(choice($.enum_entry, $._type_level_declaration)), "}"),
    enum_entry: ($) =>
      seq(
        sep1(
          seq(
            field("name", $.simple_identifier),
            optional(seq($._equal_sign, field("raw_value", $._expression))),
          ),
          ",",
        ),
        optional(";"),
      ),
    ////////////////////////////////
    // Attributes - https://docs.swift.org/swift-book/ReferenceManual/Attributes.html
    ////////////////////////////////
    attribute: ($) =>
      seq(
        "@",
        $.user_type,
        // attribute arguments are a mess of special cases, maybe this is good enough?
        optional(seq("(", sep1($._attribute_argument, ","), ")")),
      ),
    _attribute_argument: ($) =>
      choice(
        // labeled function parameters, used in custom property wrappers
        seq($.simple_identifier, ":", $._expression),
        // Unlabeled function parameters, simple identifiers, or `*`
        $._expression,
        // References to param names (used in `@objc(foo:bar:)`)
        repeat1(seq($.simple_identifier, ":")),
        // Version restrictions (iOS 3.4.5, Swift 5.0.0)
        seq(repeat1($.simple_identifier), sep1($.integer_literal, ".")),
      ),
    ////////////////////////////////
    // Patterns - https://docs.swift.org/swift-book/ReferenceManual/Patterns.html
    ////////////////////////////////
    _universally_allowed_pattern: ($) =>
      choice($.wildcard_pattern, $._type_casting_pattern, $._case_pattern),
    _bound_identifier: ($) => field("bound_identifier", $.simple_identifier),

    _binding_pattern_no_expr: ($) =>
      seq(
        choice(
          $._universally_allowed_pattern,
          $._binding_pattern,
          $._bound_identifier,
        ),
        optional($._quest),
      ),
    _no_expr_pattern_already_bound: ($) =>
      seq(
        choice($._universally_allowed_pattern, $._bound_identifier),
        optional($._quest),
      ),
    _binding_pattern_with_expr: ($) =>
      seq(
        choice(
          $._universally_allowed_pattern,
          $._binding_pattern,
          $._expression,
        ),
        optional($._quest),
      ),
    _non_binding_pattern_with_expr: ($) =>
      seq(
        choice($._universally_allowed_pattern, $._expression),
        optional($._quest),
      ),
    _direct_or_indirect_binding: ($) =>
      seq(
        choice(
          $._binding_kind_and_pattern,
          seq("case", $._binding_pattern_no_expr),
        ),
        optional($.type_annotation),
      ),
    value_binding_pattern: ($) => field("mutability", choice("var", "let")),
    _binding_kind_and_pattern: ($) =>
      seq($.value_binding_pattern, $._no_expr_pattern_already_bound),
    wildcard_pattern: ($) => "_",
    _case_pattern: ($) =>
      seq(
        optional("case"),
        optional($.user_type), // XXX this should just be _type but that creates ambiguity
        $._dot,
        $.simple_identifier,
      ),
    _type_casting_pattern: ($) =>
      choice(
        seq("is", $._type),
        seq(alias($._binding_pattern_no_expr, $.pattern), $._as, $._type),
      ),
    _binding_pattern: ($) =>
      seq(
        seq(optional("case"), $.value_binding_pattern),
        $._no_expr_pattern_already_bound,
      ),

    // ==========
    // Modifiers
    // ==========
    modifiers: ($) =>
      repeat1(
        prec.left(
          choice($._non_local_scope_modifier, $._locally_permitted_modifiers),
        ),
      ),
    _locally_permitted_modifiers: ($) =>
      repeat1(choice($.attribute, $._locally_permitted_modifier)),
    parameter_modifiers: ($) => repeat1($.parameter_modifier),
    _modifier: ($) =>
      choice($._non_local_scope_modifier, $._locally_permitted_modifier),
    _non_local_scope_modifier: ($) =>
      choice($.visibility_modifier, $.property_modifier, $.parameter_modifier),
    _locally_permitted_modifier: ($) =>
      choice($.inheritance_modifier, $.function_modifier),
    type_modifiers: ($) => repeat1($.attribute),
    visibility_modifier: ($) => choice("public", "private", "protected"),
    type_parameter_modifiers: ($) => repeat1($.attribute),
    property_modifier: ($) => choice("static", "abstract", "persistent"),
    function_modifier: ($) => choice("cb"),
    inheritance_modifier: ($) => choice("final", "native"),
    parameter_modifier: ($) => choice("out"),
  },
});

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

function tree_sitter_version_supports_emoji() {
  try {
    return (
      TREE_SITTER_CLI_VERSION_MAJOR > 0 ||
      TREE_SITTER_CLI_VERSION_MINOR > 20 ||
      TREE_SITTER_CLI_VERSION_PATCH >= 5
    );
  } catch (err) {
    if (err instanceof ReferenceError) {
      return false;
    } else {
      throw err;
    }
  }
}
