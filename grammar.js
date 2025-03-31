"use strict";

module.exports = grammar({
  name: "redscript",

  word: ($) => $.identifier,

  precedences: ($) => [
    [
      "assignment",
      "lambda",
      "binary",
      "primary",
      "parameter",
      "type",
      "expression",
    ],
    ["comment", "source_file", "declaration"],
  ],

  rules: {
    source_file: ($) =>
      seq(
        optional($.module_declaration),
        repeat(choice($.comment, $.item_declaration)),
      ),

    module_declaration: ($) => seq("module", $.module_path),

    module_path: ($) =>
      prec.left(1, seq($.identifier, repeat(seq(".", $.identifier)))),

    item_declaration: ($) =>
      seq(
        repeat($.annotation),
        optional($.visibility),
        repeat($.item_qualifier),
        $.item,
      ),

    item: ($) =>
      choice(
        $.import_declaration,
        $.class_declaration,
        $.struct_declaration,
        $.enum_declaration,
        $.function_declaration,
        $.field_declaration,
      ),

    import_declaration: ($) =>
      seq(
        "import",
        $.module_path,
        optional(seq(".", choice(seq("{", commaSep($.identifier), "}"), "*"))),
        optional(";"),
      ),

    // Expression structure
    expression: ($) =>
      choice(
        $.assignment_expression,
        $.conditional_expression,
        $.binary_expression,
        $.unary_expression,
        $.call_expression,
        $.member_expression,
        $.index_expression,
        $.cast_expression,
        $.lambda_expression,
        $.primary_expression,
      ),

    // Literals and simple expressions
    primary_expression: ($) =>
      prec.left(
        "primary",
        choice(
          $.literal,
          $.identifier,
          $.array_literal,
          $.interpolated_string,
          $.parenthesized_expression,
        ),
      ),

    literal: ($) =>
      choice(
        $.number,
        $.string,
        $.cname,
        $.resref,
        $.tdbid,
        "null",
        "this",
        "super",
        "true",
        "false",
      ),

    number: ($) => {
      const decimal = /[0-9][0-9_]*/;
      const suffix = choice("ul", "u", "l", "d");
      return token(
        seq(
          optional("-"),
          decimal,
          optional(seq(".", decimal)),
          optional(suffix),
        ),
      );
    },

    string: ($) =>
      seq('"', repeat(choice($.string_content, $.escape_sequence)), '"'),

    cname: ($) =>
      seq('n"', repeat(choice($.string_content, $.escape_sequence)), '"'),

    resref: ($) =>
      seq('r"', repeat(choice($.string_content, $.escape_sequence)), '"'),

    tdbid: ($) =>
      seq('t"', repeat(choice($.string_content, $.escape_sequence)), '"'),

    interpolated_string: ($) =>
      seq(
        's"',
        repeat(
          choice(
            $.string_content,
            $.escape_sequence,
            seq("\\(", $.expression, ")"),
          ),
        ),
        '"',
      ),

    string_content: ($) => token.immediate(/[^"\\]+/),

    escape_sequence: ($) =>
      token.immediate(
        seq("\\", choice(/[nrt'"\\]/, seq("u{", /[0-9a-fA-F]{1,6}/, "}"))),
      ),

    // Operator precedence expressions
    binary_expression: ($) => {
      // Define precedence tiers for binary operators
      const PREC = {
        multiplicative: 7, // * / %
        additive: 6, // + -
        bitwise: 5, // & | ^
        relational: 4, // < <= > >=
        equality: 3, // == !=
        logical_and: 2, // &&
        logical_or: 1, // ||
        assignment: 0, // += -= *= /= |= &=
      };

      const binOp = (operator, precedence) =>
        prec.left(
          "binary", // Use named precedence
          seq(
            field("left", $.expression),
            field("operator", operator),
            field("right", $.expression),
          ),
        );

      return choice(
        // Assignment operators
        binOp("+=", PREC.assignment),
        binOp("-=", PREC.assignment),
        binOp("*=", PREC.assignment),
        binOp("/=", PREC.assignment),
        binOp("|=", PREC.assignment),
        binOp("&=", PREC.assignment),

        // Logical operators
        binOp("||", PREC.logical_or),
        binOp("&&", PREC.logical_and),

        // Equality operators
        binOp("==", PREC.equality),
        binOp("!=", PREC.equality),

        // Relational operators
        binOp("<", PREC.relational),
        binOp("<=", PREC.relational),
        binOp(">", PREC.relational),
        binOp(">=", PREC.relational),

        // Bitwise operators
        binOp("|", PREC.bitwise),
        binOp("&", PREC.bitwise),
        binOp("^", PREC.bitwise),

        // Arithmetic operators
        binOp("+", PREC.additive),
        binOp("-", PREC.additive),
        binOp("*", PREC.multiplicative),
        binOp("/", PREC.multiplicative),
        binOp("%", PREC.multiplicative),
      );
    },

    unary_expression: ($) =>
      prec.right(
        8,
        seq(
          field("operator", choice("-", "!", "~")),
          field("argument", $.expression),
        ),
      ),

    assignment_expression: ($) =>
      prec.right(
        0,
        seq(field("left", $.expression), "=", field("right", $.expression)),
      ),

    conditional_expression: ($) =>
      prec.right(
        0,
        seq(
          field("condition", $.expression),
          "?",
          field("consequence", $.expression),
          ":",
          field("alternative", $.expression),
        ),
      ),

    call_expression: ($) =>
      prec.left(
        9,
        seq(
          field("function", $.expression),
          optional($.type_arguments),
          field("arguments", $.argument_list),
        ),
      ),

    member_expression: ($) =>
      prec.left(
        10,
        seq(field("object", $.expression), ".", field("member", $.identifier)),
      ),

    index_expression: ($) =>
      prec.left(
        10,
        seq(
          field("array", $.expression),
          "[",
          field("index", $.expression),
          "]",
        ),
      ),

    cast_expression: ($) =>
      prec.left(
        8,
        seq(field("expression", $.expression), "as", field("type", $.type)),
      ),

    // Helper expressions
    array_literal: ($) => seq("[", commaSep($.expression), "]"),

    parenthesized_expression: ($) => seq("(", $.expression, ")"),

    lambda_expression: ($) =>
      prec.left(
        "lambda", // Use named precedence
        seq(
          "(",
          commaSep($.lambda_parameter),
          ")",
          "->",
          choice(
            field("body", $.block),
            field("expression_body", $.expression),
          ),
        ),
      ),

    lambda_parameter: ($) =>
      prec(
        "parameter",
        seq(
          field("name", $.identifier),
          optional(seq(":", field("type", $.type))),
        ),
      ),

    argument_list: ($) => seq("(", commaSep($.expression), ")"),

    // Type system
    type: ($) => choice($.named_type, $.array_type, $.function_type),

    named_type: ($) =>
      prec.left(
        "type",
        seq(field("name", $.identifier), optional($.type_arguments)),
      ),

    array_type: ($) =>
      choice(
        seq("[", field("element_type", $.type), "]"),
        seq(
          "[",
          field("element_type", $.type),
          ";",
          field("size", $.number),
          "]",
        ),
      ),

    function_type: ($) =>
      seq(
        "(",
        commaSep(field("parameter_type", $.type)),
        ")",
        "->",
        field("return_type", $.type),
      ),

    type_arguments: ($) => seq("<", commaSep($.type), ">"),

    type_parameters: ($) => seq("<", commaSep($.type_parameter), ">"),

    type_parameter: ($) =>
      seq(
        optional(choice("+", "-")),
        $.identifier,
        optional(seq("extends", $.type)),
      ),

    // Annotations and qualifiers
    annotation: ($) =>
      seq(
        "@",
        field("name", $.identifier),
        optional(
          seq(
            "(",
            optional(commaSep($._annotation_expression)), // Use a special rule for annotation expressions
            ")",
          ),
        ),
      ),
    // Special rule for expressions in annotations to handle the specific conflict
    _annotation_expression: ($) =>
      choice(
        $._literal_expression, // Literals (no ambiguity)
        $._identifier_expression, // Identifiers (no ambiguity)
        alias($._array_expr, $.array_literal), // Handle array literals specially
        alias($._parenthesized_expr, $.parenthesized_expression),
        alias($._interp_string_expr, $.interpolated_string),
        alias($._lambda_expr, $.lambda_expression),
      ),

    _array_expr: ($) =>
      seq(
        "[",
        commaSep(
          choice(
            $._literal_expression,
            $._identifier_expression,
            alias($._parenthesized_expr, $.parenthesized_expression),
          ),
        ),
        "]",
      ),

    _parenthesized_expr: ($) =>
      seq(
        "(",
        choice(
          $._literal_expression,
          $._identifier_expression,
          alias($._simple_binary_expr, $.binary_expression),
        ),
        ")",
      ),

    _simple_binary_expr: ($) =>
      choice(
        ...["*", "/", "+", "-", "==", "!="].map((op) =>
          seq(
            choice($._literal_expression, $._identifier_expression),
            op,
            choice($._literal_expression, $._identifier_expression),
          ),
        ),
      ),

    _interp_string_expr: ($) =>
      seq(
        's"',
        repeat(
          choice(
            $.string_content,
            $.escape_sequence,
            seq(
              "\\(",
              choice($._literal_expression, $._identifier_expression),
              ")",
            ),
          ),
        ),
        '"',
      ),

    _lambda_expr: ($) =>
      prec(
        2,
        seq(
          // Higher precedence than identifier
          "(",
          commaSep(
            seq(
              field("name", $.identifier),
              optional(seq(":", field("type", $.type))),
            ),
          ),
          ")",
          "->",
          choice(
            seq("{", repeat($.statement), "}"),
            choice($._literal_expression, $._identifier_expression),
          ),
        ),
      ),

    _literal_expression: ($) => alias($.literal, $.expression),
    _identifier_expression: ($) => prec(1, alias($.identifier, $.expression)),

    // A version of expression that doesn't cause conflicts in annotations
    _safe_expression: ($) =>
      choice(
        $.array_literal,
        alias($._parenthesized_expression, $.expression), // Use a wrapper
        $.interpolated_string,
        $.lambda_expression,
      ),

    _parenthesized_expression: ($) => seq("(", $.expression, ")"),

    comment: ($) => choice($.line_comment, $.block_comment, $.doc_comment),

    line_comment: ($) => /\/\/[^\n]*/,
    block_comment: ($) => /\/\*(.|\n)*?\*\//,
    doc_comment: ($) => /\/\/\/[^\n]*/,

    visibility: ($) => choice("public", "private", "protected"),

    item_qualifier: ($) =>
      choice(
        "abstract",
        "cb",
        "const",
        "exec",
        "final",
        "importonly",
        "native",
        "persistent",
        "quest",
        "static",
      ),

    parameter_qualifier: ($) => choice("opt", "out", "const"),

    // Declarations
    class_declaration: ($) =>
      seq(
        "class",
        field("name", $.identifier),
        optional($.type_parameters),
        optional(seq("extends", field("superclass", $.type))),
        field("body", $.class_body),
      ),

    struct_declaration: ($) =>
      seq(
        "struct",
        field("name", $.identifier),
        optional($.type_parameters),
        optional(seq("extends", field("superclass", $.type))),
        field("body", $.class_body),
      ),

    class_body: ($) => seq("{", repeat($.item_declaration), "}"),

    enum_declaration: ($) =>
      seq(
        "enum",
        field("name", $.identifier),
        "{",
        commaSep($.enum_entry),
        "}",
      ),

    enum_entry: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("=", field("value", $.number))),
      ),

    function_declaration: ($) =>
      seq(
        "func",
        field("name", $.identifier),
        optional($.type_parameters),
        field("parameters", $.parameter_list),
        optional(seq("->", field("return_type", $.type))),
        choice(
          field("body", $.block),
          seq("=", field("expression_body", $.expression), ";"),
          ";",
        ),
      ),

    parameter_list: ($) => seq("(", commaSep($.parameter), ")"),

    parameter: ($) =>
      seq(
        repeat($.parameter_qualifier),
        field("name", $.identifier),
        ":",
        field("type", $.type),
      ),

    field_declaration: ($) =>
      seq(
        "let",
        field("name", $.identifier),
        ":",
        field("type", $.type),
        optional(seq("=", field("initializer", $.expression))),
        ";",
      ),

    // Statements
    statement: ($) =>
      choice(
        $.let_statement,
        $.if_statement,
        $.while_statement,
        $.for_statement,
        $.switch_statement,
        $.return_statement,
        $.break_statement,
        $.continue_statement,
        seq($.expression, ";"),
      ),

    block: ($) => seq("{", repeat($.statement), "}"),

    let_statement: ($) =>
      seq(
        "let",
        field("name", $.identifier),
        optional(seq(":", field("type", $.type))),
        optional(seq("=", field("initializer", $.expression))),
        ";",
      ),

    if_statement: ($) =>
      seq(
        "if",
        field("condition", $.expression),
        field("consequence", $.block),
        optional(
          seq("else", field("alternative", choice($.block, $.if_statement))),
        ),
      ),

    while_statement: ($) =>
      seq("while", field("condition", $.expression), field("body", $.block)),

    for_statement: ($) =>
      seq(
        "for",
        field("name", $.identifier),
        "in",
        field("iterator", $.expression),
        field("body", $.block),
      ),

    switch_statement: ($) =>
      seq(
        "switch",
        field("value", $.expression),
        "{",
        repeat($.case_clause),
        optional($.default_clause),
        "}",
      ),

    case_clause: ($) =>
      seq("case", field("value", $.expression), ":", repeat($.statement)),

    default_clause: ($) => seq("default", ":", repeat($.statement)),

    return_statement: ($) => seq("return", optional($.expression), ";"),

    break_statement: ($) => seq("break", ";"),

    continue_statement: ($) => seq("continue", ";"),

    // Common helpers
    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
  },
});

function commaSep(rule) {
  return optional(seq(rule, repeat(seq(",", rule)), optional(",")));
}
