/**
 * @file Brad grammar for tree-sitter
 * @author BRAD Team
 * @license AGPL
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PRECEDENCE = {
    PIPE_RIGHT: 1,
    PIPE_LEFT: 1,
    OR: 2,
    AND: 3,
    EQUALITY: 4,
    COMPARISON: 5,
    BITWISE_OR: 6,
    BITWISE_XOR: 7,
    BITWISE_AND: 8,
    SHIFT: 9,
    ADDITIVE: 10,
    MULTIPLICATIVE: 11,
    UNARY: 15,
    CALL: 13,
    ACCESS: 14,
    TYPE_FUNCTION: 1,
    TYPE_UNION: 2,
    TYPE_TUPLE: 3,
};

module.exports = grammar({
    name: "brad",

    extras: $ => [/\s/, $.comment],


    rules: {
        // module
        source_file: $ => seq(
            repeat($.module_attribute),
            repeat(seq(repeat($.declaration_attribute), $._declaration))
        ),

        // attributes
        module_attribute: $ => seq('#', '!', '[', $.attribute, ']'),
        declaration_attribute: $ => seq('#', '[', $.attribute, ']'),
        attribute: $ => seq($.identifier, optional(seq('=', $._literal)),),

        // declarations
        _declaration: $ => choice(
            $.import_declaration,
            $.type_declaration,
            $.alias_declaration,
            $.function_declaration,
            $.extern_function_declaration,
            $.extern_type_declaration
        ),

        import_declaration: $ => seq('import', $.path0),

        type_declaration: $ => seq(
            'type',
            $.path0,
            optional($.specialization_definition),
            optional(seq('=', $._type))
        ),

        extern_type_declaration: $ => seq(
            'extern',
            'type',
            $.path0,
            optional($.specialization_definition),
        ),

        alias_declaration: $ => seq('alias', $.path0, optional($.specialization_definition), '=', $._type),

        function_argument: $ => choice(
            $.bind,
            seq('(', $.bind, ':', $._type, ')')
        ),

        function_declaration: $ => seq(
            'fn',
            $.path0,
            optional($.specialization_definition),
            repeat($.function_argument),
            optional(seq('->', $._type)),
            choice(
                seq('=>', $._expression),
                $.block_expression
            )
        ),

        extern_function_declaration: $ => seq(
            'extern',
            'fn',
            $.path0,
            optional($.specialization_definition),
            repeat($.function_argument),
            optional(seq('->', $._type))
        ),

        // expressions
        _expression: $ => $.assignment_expression,

        assignment_expression: $ => choice(
            prec.right(1, seq($.binary_expression, '=', $.assignment_expression)),
            $.binary_expression,
        ),

        binary_expression: $ => $.pipe_expression,

        pipe_expression: $ => choice(
            prec.left(PRECEDENCE.PIPE_RIGHT, seq($.pipe_expression, '|>', $.logical_or_expression)),
            prec.right(PRECEDENCE.PIPE_LEFT, seq($.pipe_expression, '<|', $.logical_or_expression)),
            $.logical_or_expression,
        ),

        logical_or_expression: $ => choice(
            prec.left(PRECEDENCE.OR, seq($.logical_or_expression, '||', $.logical_and_expression)),
            $.logical_and_expression,
        ),

        logical_and_expression: $ => choice(
            prec.left(PRECEDENCE.AND, seq($.logical_and_expression, '&&', $.equality_expression)),
            $.equality_expression,
        ),

        equality_expression: $ => choice(
            prec.left(PRECEDENCE.EQUALITY, seq($.equality_expression, '==', $.comparison_expression)),
            prec.left(PRECEDENCE.EQUALITY, seq($.equality_expression, '!=', $.comparison_expression)),
            $.comparison_expression,
        ),

        comparison_expression: $ => choice(
            prec.left(PRECEDENCE.COMPARISON, seq($.comparison_expression, '<', $.bitwise_or_expression)),
            prec.left(PRECEDENCE.COMPARISON, seq($.comparison_expression, '>', $.bitwise_or_expression)),
            prec.left(PRECEDENCE.COMPARISON, seq($.comparison_expression, '<=', $.bitwise_or_expression)),
            prec.left(PRECEDENCE.COMPARISON, seq($.comparison_expression, '>=', $.bitwise_or_expression)),
            $.bitwise_or_expression,
        ),

        bitwise_or_expression: $ => choice(
            prec.left(PRECEDENCE.BITWISE_OR, seq($.bitwise_or_expression, '|', $.bitwise_xor_expression)),
            $.bitwise_xor_expression,
        ),

        bitwise_xor_expression: $ => choice(
            prec.left(PRECEDENCE.BITWISE_XOR, seq($.bitwise_xor_expression, '^', $.bitwise_and_expression)),
            $.bitwise_and_expression,
        ),

        bitwise_and_expression: $ => choice(
            prec.left(PRECEDENCE.BITWISE_AND, seq($.bitwise_and_expression, '&', $.shift_expression)),
            $.shift_expression,
        ),

        shift_expression: $ => choice(
            prec.left(PRECEDENCE.SHIFT, seq($.shift_expression, '<<', $.additive_expression)),
            prec.left(PRECEDENCE.SHIFT, seq($.shift_expression, '>>', $.additive_expression)),
            $.additive_expression,
        ),

        additive_expression: $ => choice(
            prec.left(PRECEDENCE.ADDITIVE, seq($.additive_expression, '+', $.multiplicative_expression)),
            prec.left(PRECEDENCE.ADDITIVE, seq($.additive_expression, '-', $.multiplicative_expression)),
            $.multiplicative_expression,
        ),

        multiplicative_expression: $ => choice(
            prec.left(PRECEDENCE.MULTIPLICATIVE, seq($.multiplicative_expression, '*', $.unary_expression)),
            prec.left(PRECEDENCE.MULTIPLICATIVE, seq($.multiplicative_expression, '/', $.unary_expression)),
            prec.left(PRECEDENCE.MULTIPLICATIVE, seq($.multiplicative_expression, '%', $.unary_expression)),
            $.unary_expression,
        ),

        unary_expression: $ => choice(
            prec.right(PRECEDENCE.UNARY, seq('!', $.unary_expression)),
            prec.right(PRECEDENCE.UNARY, seq('-', $.unary_expression)),
            prec.right(PRECEDENCE.UNARY, seq('~', $.unary_expression)),
            prec.right(PRECEDENCE.UNARY, seq('*', $.unary_expression)),
            prec.right(PRECEDENCE.UNARY, seq('ref', $.unary_expression)),
            $.call_expression,
        ),

        call_expression: $ => choice(
            prec.left(PRECEDENCE.CALL, seq($.call_expression, $.postfix_expression)),
            $.postfix_expression,
        ),

        postfix_expression: $ => choice(
            prec.left(PRECEDENCE.ACCESS, seq($.postfix_expression, '.', $.identifier)),
            prec.left(PRECEDENCE.ACCESS, seq($.postfix_expression, '[', $.assignment_expression, ']')),
            $.primary_expression,
        ),

        primary_expression: $ => choice(
            $.match_expression,
            $.let_expression,
            $.loop_expression,
            $.if_expression,
            $.block_expression,
            $.return_expression,
            $.break_expression,
            $.continue_expression,
            $.literal_expression,
            $.path,
            $.list_expression,
            $.tuple_expression,
            $.record_expression,
            $.parenthesized_expression,
        ),

        block_expression: $ => prec(1, seq('{', repeat($._expression), '}')),

        match_arm: $ => seq(
            '|',
            $.match_pattern,
            optional(seq('as', $.bind)),
            '=>',
            $._expression,
        ),

        match_pattern: $ => choice(
            $._type,
            '_'
        ),

        match_expression: $ => prec.left(seq(
            'match',
            $._expression,
            optional
            ('\n'),
            repeat1($.match_arm)
        )),

        loop_expression: $ => seq(
            'loop',
            $.block_expression,
        ),

        let_expression: $ => seq(
            'let',
            $.bind,
            optional(seq(':', $._type)),
            '=',
            $._expression,
        ),

        if_expression: $ => seq(
            'if',
            $._expression,
            choice(
                seq('->', $._expression),
                seq($.block_expression, optional(seq('else', choice($.if_expression, $.block_expression))))
            )
        ),

        return_expression: $ => prec.right(seq('return', optional($._expression))),
        break_expression: $ => prec.right(seq('break', optional($._expression))),
        continue_expression: $ => 'continue',

        literal_expression: $ => choice(
            $._string_literal,
            $._float_literal,
            $._integer_literal,
            'true',
            'false',
            'none',
        ),

        list_expression: $ => seq(
            '[',
            optional(seq(
                $._expression,
                repeat(seq(
                    optional(';'),
                    $._expression
                )),
                optional(';')
            )),
            ']'
        ),

        tuple_expression: $ => prec.left(2, seq(
            $._expression,
            ',',
            $._expression,
            repeat(seq(',', $._expression)),
            optional(',')
        )),


        record_expression: $ => seq(
            '{',
            optional(seq(
                $.field_value,
                repeat(seq(
                    optional(';'),
                    $.field_value
                )),
                optional(';')
            )),
            '}'
        ),

        field_value: $ => seq(
            $.identifier,
            ':',
            $._expression,
        ),

        parenthesized_expression: $ => seq('(', $._expression, ')'),

        // types
        _type: $ => choice(
            $.type_function,
        ),

        type_function: $ => choice(
            $.type_union,
            prec.right(PRECEDENCE.TYPE_FUNCTION, seq($.type_function, '->', $.type_union)),
        ),

        type_union: $ => choice(
            $.type_tuple,
            prec.left(PRECEDENCE.TYPE_UNION, seq($.type_union, '|', $.type_tuple)),
        ),

        type_tuple: $ => choice(
            $.type_primary,
            prec.left(PRECEDENCE.TYPE_TUPLE, seq($.type_tuple, '&', $.type_primary)),
        ),

        type_primary: $ => choice(
            $.type_ref,
            $.type_task,
            $.type_atom,
        ),

        type_atom: $ => choice(
            $.path,
            $.generic,
            $.type_record,
            $.type_array,
            $.type_parenthesized,
            '!',
        ),

        type_record: $ => seq(
            '{',
            optional(seq(
                $.type_field,
                repeat(seq(';', $.type_field)),
                optional(';')
            )),
            '}'
        ),

        type_field: $ => seq(
            $.identifier,
            ':',
            $._type
        ),

        type_array: $ => choice(
            seq('[', $._type, ']'),
            seq('[', $._type, ';', $._integer_literal, ']'),
        ),

        type_ref: $ => seq('ref', $.type_atom),
        type_task: $ => seq('task', $.type_atom),
        type_parenthesized: $ => seq('(', $._type, ')'),

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
        path: $ => prec.left(seq($.path0, optional($.specialization))),
        path0: $ => seq($.identifier, repeat(seq('::', $.identifier))),

        generic: $ => token(seq("'", /[a-zA-Z_][a-zA-Z0-9_\-]*/)),
        identifier: $ => token(/[a-zA-Z_][a-zA-Z0-9_\-]*/),
        comment: $ => token(seq('//', /.*/)),
    }
});
