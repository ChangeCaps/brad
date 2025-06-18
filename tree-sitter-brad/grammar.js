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
    ACCESS: 14,
    UNARY: 13,
    REF: 2,
    CALL: 1,
};

module.exports = grammar({
    name: "brad",

    extras: $ => [/\s/, $.comment,],

    rules: {
        // module
        source_file: $ => $._expression, // seq(repeat($.module_attribute), repeat(seq(repeat($.declaration_attribute), $._declaration))),

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
        _expression: $ => choice(
            $.match_expression,
            $.let_expression,
            $.loop_expression,
            $.access_expression,
            $.unary_expression,
            $.binary_expression,
            $.term_expression,
            'break'
        ),

        _block_expression: $ => seq('{', repeat($._expression), '}'),

        match_arm: $ => seq(
            '|',
            $._type,
            optional(seq('as', $.bind)),
            '=>',
            $._expression,
        ),

        match_expression: $ => seq(
            'match',
            $._expression,
            '\n',
            repeat(seq(
                $.match_arm,
                repeat1(seq('\n', $.match_arm))
            ))
        ),

        loop_expression: $ => seq(
            'loop',
            $._block_expression,
        ),

        let_expression: $ => seq(
            'let',
            $.bind,
            optional(seq(':', $._type)),
            seq('=', $._expression),
        ),

        call_expression: $ => prec.left(PRECEDENCE.CALL, seq(
            $._expression,
            repeat($._expression),
        )),

        access_expression: $ => choice(
            prec.left(PRECEDENCE.ACCESS, seq($._expression, '.', $.identifier)),
            prec.left(PRECEDENCE.ACCESS, seq($._expression, '[', $._expression, ']')),
        ),

        unary_expression: $ => choice(
            prec.left(PRECEDENCE.UNARY, seq('*', $._expression)),
            prec.left(PRECEDENCE.UNARY, seq('~', $._expression)),
            prec.left(PRECEDENCE.UNARY, seq('-', $._expression)),
            prec.left(PRECEDENCE.UNARY, seq('!', $._expression)),
            prec.left(PRECEDENCE.REF, seq('ref', $._expression)),
        ),

        binary_expression: $ => choice(
            prec.left(12, seq($._expression, '%', $._expression)),
            prec.left(12, seq($._expression, '/', $._expression)),
            prec.left(12, seq($._expression, '*', $._expression)),

            prec.left(11, seq($._expression, '+', $._expression)),
            prec.left(11, seq($._expression, '-', $._expression)),

            prec.left(10, seq($._expression, '>>', $._expression)),
            prec.left(10, seq($._expression, '<<', $._expression)),

            prec.left(9, seq($._expression, '&', $._expression)),
            prec.left(8, seq($._expression, '^', $._expression)),
            prec.left(7, seq($._expression, '|', $._expression)),

            prec.left(6, seq($._expression, '>=', $._expression)),
            prec.left(6, seq($._expression, '<=', $._expression)),
            prec.left(6, seq($._expression, '<', $._expression)),
            prec.left(6, seq($._expression, '>', $._expression)),
            prec.left(5, seq($._expression, '!=', $._expression)),
            prec.left(5, seq($._expression, '==', $._expression)),
            prec.left(4, seq($._expression, '&&', $._expression)),
            prec.left(3, seq($._expression, '||', $._expression)),
            // ref
            // call
            prec.left(0, seq($._expression, '=', $._expression)),
        ),

        // initializers
        term_expression: $ => choice(
            $._string_literal,
            $._float_literal,
            $._integer_literal,
            $.path,
            seq(
                '[',
                repeat('\n'),
                optional(
                    seq(
                        $._expression,
                        repeat(seq(';', repeat('\n'), $._expression)),
                        optional(';'),
                        repeat('\n')
                    )
                ),
                ']'
            ), // list
            seq($._expression, repeat1(seq(',', $._expression))), // tuple
            seq(
                '{',
                repeat('\n'),
                optional(
                    seq(
                        $._field_value,
                        repeat(seq(';', repeat('\n'), $._field_value)),
                        optional(';'),
                        repeat('\n'),
                    )
                ),
                '}'
            ), // record
            seq(
                '(',
                $._expression,
                ')'
            ), // parenthesis
        ),

        _field_value: $ => seq(
            $.identifier,
            ':',
            $._expression,
        ),

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
            $.bind,
            repeat(seq(',', $.bind)),
            optional(','),
            ')',
        ),

        // misc
        path: $ => seq($.path0, optional($.specialization)),
        path0: $ => seq($.identifier, repeat(seq('::', $.identifier))),

        generic: $ => token(seq("'", /[a-zA-Z_][a-zA-Z0-9_\-]*/)),
        identifier: $ => token(/[a-zA-Z_][a-zA-Z0-9_\-]*/),
        comment: $ => token(seq('//', /.*/)),
    }
});
