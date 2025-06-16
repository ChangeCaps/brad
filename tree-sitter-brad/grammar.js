/**
 * @file Brad grammar for tree-sitter
 * @author BRAD Team
 * @license AGPL
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// TODO: Consider when to use token.immediate and how newlines are dealt with.

const PRECEDENCE = {
    TYPE_FUNCTION: 1,
    FUNCTION_RETURN_TYPE: 2,
};

module.exports = grammar({
    name: "brad",

    extras: $ => [/\s/, $.comment,],

    rules: {
        // module
        source_file: $ => seq(repeat($.module_attribute), repeat(seq(repeat($.declaration_attribute), $._declaration))),

        // attributes
        module_attribute: $ => seq('#', '!', '[', $.attribute, ']'),
        declaration_attribute: $ => seq('#', '[', $.attribute, ']'),
        attribute: $ => seq($.identifier, optional(seq('=', $._literal)),),

        // declarations
        _declaration: $ => choice($.import_declaration, $.type_declaration, $.alias_declaration, $.function_declaration,),

        import_declaration: $ => seq('import', $.path0),

        type_declaration: $ => seq('type', $.path0, optional($.specialization_definition), optional(seq('=', $._type)),),

        alias_declaration: $ => seq('alias', $.path0, optional($.specialization_definition), '=', $._type),

        function_argument: $ => choice(
            $.identifier,
            seq('(', $.bind, ')', optional(seq(':', $._type)))
        ),

        function_declaration: $ => seq(
            'fn',
            $.path0,
            optional($.specialization_definition),
            repeat($.function_argument),
            optional(prec.left(PRECEDENCE.FUNCTION_RETURN_TYPE, seq('->', $._type))),
            choice(seq('=>', $._expression),
                $._block_expression
            )),

        // expressions
        _expression: $ => choice(),
        _block_expression: $ => seq('{', repeat($._expression), '}'),

        // initializers

        // types

        _type: $ => choice(
            $.path,
            $.generic,
            $._type_function,
            $._type_record,
            $._type_array,
            $._type_union,
            $._type_tuple,
            seq('(', $._type, ')')
        ),

        _type_function: $ => prec.right(PRECEDENCE.TYPE_FUNCTION, seq($._type, '->', $._type)),

        // TODO;
        _type_record: $ => seq('{', repeat(seq($.identifier, ':', $._type, optional(','),)), '}'),

        _type_array: $ => seq('[', $._type, ']'),

        _type_union: $ => prec.right(seq($._type, '|', $._type,)),

        _type_tuple: $ => prec.right(seq($._type, '*', $._type,)),

        // Defining generic parameters
        // <'a, 'b, 'c>
        specialization_definition: $ => seq('<', $.generic, repeat(seq(',', $.generic)), optional(','), '>'),

        // Using generic parameters
        // <T, U, 'x>
        specialization: $ => seq('<', $._type, repeat(seq(',', $._type)), optional(','), '>'),

        // literals
        _literal: $ => choice($._string_literal, $._float_literal, $._integer_literal, $._negative_literal),

        _string_literal: $ => token(seq('"', repeat(choice(/[^"\\]/, '\\' + /./)), '"')),
        _float_literal: $ => token(/\d+\.\d+/),
        _integer_literal: $ => token(/\d+/),
        // TODO: How did we deal with - (unary vs symbolic) again?
        _negative_literal: $ => seq('-', choice($._float_literal, $._integer_literal)),

        // bind
        bind: $ => choice(
            $._bind_identifier,
            $._bind_multiple,
        ),

        _bind_identifier: $ => seq(optional('mut'), $.identifier),
        _bind_multiple: $ => seq(
            '(',
            repeat1(seq($.bind, optional(','))),
            ')'
        ),

        // misc
        path: $ => seq($.path0, optional($.specialization)),
        path0: $ => seq($.identifier, repeat(seq('::', $.identifier))),

        generic: $ => token(seq("'", /[a-zA-Z_][a-zA-Z0-9_\-]*/)),
        identifier: $ => token(/[a-zA-Z_][a-zA-Z0-9_\-]*/),
        comment: $ => token(seq('//', /.*/)),
    }
});
