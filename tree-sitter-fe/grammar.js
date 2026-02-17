/// <reference types="tree-sitter-cli/dsl" />

// Operator precedence levels (from Fe compiler's binding powers)
const PREC = {
  ASSIGN: 1,        // = and augmented assignment (bp 11,10)
  RANGE: 2,         // .. (bp 40,41)
  OR: 3,            // || (bp 50,51)
  AND: 4,           // && (bp 60,61)
  COMPARE: 5,       // == != < > <= >= (bp 70,71)
  BITOR: 6,         // | (bp 80,81)
  BITXOR: 7,        // ^ (bp 90,91)
  BITAND: 8,        // & (bp 100,101)
  SHIFT: 9,         // << >> (bp 110,111)
  ADD: 10,          // + - (bp 120,121)
  MUL: 11,          // * / % (bp 130,131)
  EXP: 12,          // ** right-assoc (bp 141,140)
  UNARY: 13,        // ! - ~ + (bp 145)
  CAST: 14,         // as (bp 146)
  POSTFIX: 15,      // call, index, field (bp 147-151)
  PATH: 16,
};

module.exports = grammar({
  name: 'fe',

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  externals: $ => [
    $._automatic_semicolon,
    $._block_comment_content,
    $._block_comment_end,
    $._generic_open,
    $._comparison_lt,
  ],

  word: $ => $.identifier,

  supertypes: $ => [
    $._type,
    $._pattern,
    $._item,
    $._statement,
  ],

  conflicts: $ => [
    // Self type vs self path segment vs expression
    [$.self_type, $.path_segment],
    // [$.self_type, $._expression, $.path_segment], -- resolved by precedence
    // recv arm pattern
    [$.recv_arm_pattern],
    // _condition uses same terminals as _expression, needs same conflicts
    [$._condition, $.path_segment],
    // scoped_path / path ambiguity: identifiers, scoped_path, keywords can be
    // expression, _path (for further ::), or path_segment (for type paths)
    [$._expression, $.path_segment],
    [$._path, $.path_segment],
    // attribute name can be identifier or path (path starts with path_segment which is identifier)
    [$.path_segment, $.attribute],
  ],

  rules: {
    source_file: $ => repeat(choice($._item, $._statement)),

    // Statement terminator: either an explicit ';' or an automatic one
    // inserted by the external scanner at newline boundaries.
    // This mirrors tree-sitter-javascript's _semicolon approach.
    _terminator: $ => choice(';', $._automatic_semicolon),

    // ==================== ITEMS ====================

    _item: $ => choice(
      $.use_statement,
      $.const_definition,
      $.function_definition,
      $.struct_definition,
      $.enum_definition,
      $.contract_definition,
      $.msg_definition,
      $.trait_definition,
      $.impl_block,
      $.impl_trait,
      $.type_alias,
      $.mod_definition,
      $.extern_block,
    ),

    // Use statement: use path::to::{A, B}
    use_statement: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'use',
      $.use_tree,
    ),

    // use_tree handles the entire use path + suffix as one unit
    // to avoid ambiguity at :: boundaries.
    // Possible forms:
    //   Foo::Bar            (simple import)
    //   Foo::Bar as Baz     (renamed import)
    //   Foo::Bar::{A, B}    (grouped imports)
    //   Foo::Bar::*         (glob import)
    //   {A, B}              (bare group)
    //   *                   (bare glob)
    use_tree: $ => prec.right(seq(
      repeat(seq($.use_path_segment, '::')),
      choice(
        seq($.use_path_segment, optional(seq('as', choice($.identifier, '_')))),
        $.use_tree_list,
        '*',
      ),
    )),

    use_tree_list: $ => seq(
      '{',
      sepTrailing($.use_tree, ','),
      '}',
    ),

    use_path_segment: $ => choice(
      $.identifier,
      'self',
      'super',
      'ingot',
    ),

    // Const definition: const NAME: Type = expr
    const_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'const',
      field('name', $.identifier),
      ':',
      field('type', $._type),
      '=',
      field('value', $._expression),
      $._terminator,
    ),

    // Function definition: fn name<T>(params) -> Type uses (...) where ... { body }
    function_definition: $ => prec.right(seq(
      optional($.attribute_list),
      optional($.visibility),
      optional('unsafe'),
      optional('const'),
      'fn',
      field('name', $.identifier),
      optional($.generic_param_list),
      $.parameter_list,
      optional(seq('->', field('return_type', $._type))),
      optional($.uses_clause),
      optional($.where_clause),
      optional(field('body', $.block)),
    )),

    parameter_list: $ => seq(
      '(',
      sepTrailing($.parameter, ','),
      ')',
    ),

    parameter: $ => choice(
      seq(optional('mut'), 'self', optional(seq(':', $._type))),
      seq(
        optional('mut'),
        optional('_'),
        field('name', choice($.identifier, '_')),
        ':',
        field('type', $._type),
      ),
    ),

    // Struct definition
    struct_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'struct',
      field('name', $.identifier),
      optional($.generic_param_list),
      optional($.where_clause),
      field('body', $.record_field_def_list),
    ),

    record_field_def_list: $ => seq(
      '{',
      sepTrailing($._record_field_def_item, ','),
      '}',
    ),

    _record_field_def_item: $ => seq(
      optional($.attribute_list),
      $.record_field_def,
    ),

    record_field_def: $ => seq(
      optional($.visibility),
      optional('mut'),
      field('name', $.identifier),
      ':',
      field('type', $._type),
    ),

    // Enum definition
    enum_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'enum',
      field('name', $.identifier),
      optional($.generic_param_list),
      optional($.where_clause),
      field('body', $.variant_def_list),
    ),

    variant_def_list: $ => seq(
      '{',
      sepTrailing($.variant_def, ','),
      '}',
    ),

    variant_def: $ => seq(
      optional($.attribute_list),
      field('name', $.identifier),
      optional(choice(
        $.tuple_type,
        $.record_field_def_list,
      )),
    ),

    // Contract definition
    contract_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'contract',
      field('name', $.identifier),
      optional($.uses_clause),
      '{',
      optional($.contract_fields),
      optional($.contract_init),
      repeat($.contract_recv),
      '}',
    ),

    contract_fields: $ => repeat1($._contract_field_item),

    _contract_field_item: $ => seq(
      $.record_field_def,
      optional(','),
    ),

    contract_init: $ => seq(
      'init',
      $.parameter_list,
      optional($.uses_clause),
      field('body', $.block),
    ),

    contract_recv: $ => seq(
      'recv',
      optional(field('message_type', $.path)),
      '{',
      repeat($.recv_arm),
      '}',
    ),

    recv_arm: $ => seq(
      $.recv_arm_pattern,
      optional(seq('->', field('return_type', $._type))),
      optional($.uses_clause),
      field('body', $.block),
    ),

    recv_arm_pattern: $ => seq(
      field('name', $.identifier),
      optional(seq(
        '{',
        sepTrailing(choice($.identifier, $.rest_pattern), ','),
        '}',
      )),
    ),

    // Msg definition
    msg_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'msg',
      field('name', $.identifier),
      '{',
      sepTrailing($.msg_variant, ','),
      '}',
    ),

    msg_variant: $ => seq(
      optional($.attribute_list),
      field('name', $.identifier),
      optional($.msg_variant_params),
      optional(seq('->', field('return_type', $._type))),
    ),

    msg_variant_params: $ => seq(
      '{',
      sepTrailing($.record_field_def, ','),
      '}',
    ),

    // Uses clause
    uses_clause: $ => seq(
      'uses',
      choice(
        $.uses_param_list,
        $.uses_param,
      ),
    ),

    uses_param_list: $ => seq(
      '(',
      sepTrailing($.uses_param, ','),
      ')',
    ),

    uses_param: $ => choice(
      // labeled: `name: Type` or `mut name: Type` or `name: mut Type`
      seq(
        optional('mut'),
        field('name', choice($.identifier, '_')),
        ':',
        optional('mut'),
        field('type', $._type),
      ),
      // unlabeled: `Type` or `mut Type`
      seq(
        optional('mut'),
        field('type', $.path),
      ),
    ),

    // Trait definition
    trait_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'trait',
      field('name', $.identifier),
      optional($.generic_param_list),
      optional($.super_trait_list),
      optional($.where_clause),
      field('body', $.trait_item_list),
    ),

    super_trait_list: $ => seq(
      ':',
      sep1($.trait_ref, '+'),
    ),

    trait_ref: $ => seq(
      $.path,
      optional($.generic_arg_list),
    ),

    trait_item_list: $ => seq(
      '{',
      repeat(choice(
        $.function_definition,
        $.trait_type_item,
        $.trait_const_item,
      )),
      '}',
    ),

    trait_type_item: $ => seq(
      optional($.attribute_list),
      'type',
      field('name', $.identifier),
      optional(seq(':', $.type_bound_list)),
      optional(seq('=', $._type)),
    ),

    trait_const_item: $ => seq(
      optional($.attribute_list),
      'const',
      field('name', $.identifier),
      ':',
      field('type', $._type),
      optional(seq('=', field('value', $._expression))),
    ),

    // Impl block
    impl_block: $ => seq(
      optional($.attribute_list),
      'impl',
      optional($.generic_param_list),
      field('type', $._type),
      optional($.where_clause),
      field('body', $.impl_item_list),
    ),

    impl_trait: $ => seq(
      optional($.attribute_list),
      'impl',
      optional($.generic_param_list),
      field('trait', $.trait_ref),
      'for',
      field('type', $._type),
      optional($.where_clause),
      field('body', $.trait_item_list),
    ),

    impl_item_list: $ => seq(
      '{',
      repeat($.function_definition),
      '}',
    ),

    // Type alias
    type_alias: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'type',
      field('name', $.identifier),
      optional($.generic_param_list),
      '=',
      field('type', $._type),
    ),

    // Module definition
    mod_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'mod',
      field('name', $.identifier),
      '{',
      repeat($._item),
      '}',
    ),

    // Extern block
    extern_block: $ => seq(
      optional($.attribute_list),
      'extern',
      '{',
      repeat($.function_definition),
      '}',
    ),

    // ==================== GENERICS ====================

    generic_param_list: $ => seq(
      $._generic_open,
      sepTrailing(choice($.type_generic_param, $.const_generic_param), ','),
      '>',
    ),

    type_generic_param: $ => seq(
      field('name', $.identifier),
      optional(seq(':', $.type_bound_list)),
      optional(seq('=', field('default', $._type))),
    ),

    const_generic_param: $ => seq(
      'const',
      field('name', $.identifier),
      ':',
      field('type', $._type),
    ),

    type_bound_list: $ => prec.left(sep1($.type_bound, '+')),

    type_bound: $ => choice(
      // Trait bound: Path<Args>
      prec.right(PREC.PATH + 5, seq($.path, optional($.generic_arg_list))),
      // Kind bound: * -> * -> *, (* -> *) -> *, etc.
      $.kind_bound,
    ),

    kind_bound: $ => prec.right(choice(
      seq('*', optional(seq('->', $.kind_bound))),
      seq('(', $.kind_bound, ')', optional(seq('->', $.kind_bound))),
    )),

    // Uses external scanner token _generic_open instead of '<' to avoid
    // precedence conflict with binary_expression's '<' operator.
    // The scanner uses lookahead to decide: if a matching '>' is found
    // before any token that can't appear in generics (like '{' or ';'),
    // it's a generic; otherwise it's comparison and the parser's built-in
    // lexer matches '<' for binary_expression.
    generic_arg_list: $ => seq(
      $._generic_open,
      sepTrailing($._generic_arg, ','),
      '>',
    ),

    _generic_arg: $ => choice(
      $.assoc_type_generic_arg,
      $._type,
      // Const generic args: block expressions like {3 + 4} and literals
      $.block,
      $.literal,
    ),

    assoc_type_generic_arg: $ => prec(1, seq(
      field('name', $.identifier),
      '=',
      field('type', $._type),
    )),

    where_clause: $ => prec.right(seq(
      'where',
      sep1($.where_predicate, ','),
      optional(','),
    )),

    where_predicate: $ => seq(
      $._type,
      ':',
      $.type_bound_list,
    ),

    // ==================== TYPES ====================

    _type: $ => choice(
      $.path_type,
      $.tuple_type,
      $.array_type,
      $.pointer_type,
      $.self_type,
      $.never_type,
      $.qualified_path_type,
    ),

    // Qualified path: <Type as Trait>::AssocType
    qualified_path_type: $ => prec.right(seq(
      $._generic_open,
      field('type', $._type),
      'as',
      field('trait', $.trait_ref),
      '>',
      repeat1(seq('::', field('name', $.identifier))),
    )),

    path_type: $ => prec.right(PREC.PATH + 5, seq(
      $.path,
      optional($.generic_arg_list),
    )),

    tuple_type: $ => seq(
      '(',
      sepTrailing($._type, ','),
      ')',
    ),

    array_type: $ => seq(
      '[',
      field('element', $._type),
      ';',
      field('length', $._expression),
      ']',
    ),

    pointer_type: $ => seq(
      '*',
      $._type,
    ),

    self_type: $ => 'Self',

    never_type: $ => '!',

    // ==================== EXPRESSIONS ====================

    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.cast_expression,
      $.call_expression,
      $.method_call_expression,
      $.instantiation_expression,
      $.field_expression,
      $.index_expression,
      // Path expressions: identifier and scoped paths directly in expression
      prec.left($.identifier),
      $.scoped_path,
      'self',
      'Self',
      'super',
      'ingot',
      $.record_expression,
      $.tuple_expression,
      $.array_expression,
      $.array_repeat_expression,
      $.paren_expression,
      $.literal,
      $.if_expression,
      $.match_expression,
      $.with_expression,
      $.block,
      $.assignment_expression,
      $.augmented_assignment_expression,
      $.range_expression,
      $.qualified_path_expression,
    ),

    // Qualified path in expression context: <T as Trait>::method(args)
    qualified_path_expression: $ => prec.right(seq(
      $._generic_open,
      field('type', $._type),
      'as',
      field('trait', $.trait_ref),
      '>',
      repeat1(seq('::', field('name', $.identifier))),
    )),

    binary_expression: $ => {
      const table = [
        [PREC.OR, '||'],
        [PREC.AND, '&&'],
        [PREC.COMPARE, choice('==', '!=', $._comparison_lt, '>', '<=', '>=')],
        [PREC.BITOR, '|'],
        [PREC.BITXOR, '^'],
        [PREC.BITAND, '&'],
        [PREC.SHIFT, choice('<<', '>>')],
        [PREC.ADD, choice('+', '-')],
        [PREC.MUL, choice('*', '/', '%')],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(precedence, seq(
            field('left', $._expression),
            field('operator', operator),
            field('right', $._expression),
          ))
        ),
        // ** is right-associative
        prec.right(PREC.EXP, seq(
          field('left', $._expression),
          field('operator', '**'),
          field('right', $._expression),
        )),
      );
    },

    unary_expression: $ => prec(PREC.UNARY, seq(
      field('operator', choice('!', '-', '~')),
      field('operand', $._expression),
    )),

    cast_expression: $ => prec.left(PREC.CAST, seq(
      field('value', $._expression),
      'as',
      field('type', $._type),
    )),

    call_expression: $ => prec(PREC.POSTFIX, seq(
      field('function', $._expression),
      field('arguments', $.call_arg_list),
    )),

    // Generic instantiation without turbofish: expr<Type>
    // Used for patterns like `evm.create2<Coin>(args)` where:
    // - `evm.create2` is a field_expression
    // - `<Coin>` is the generic_arg_list wrapped in instantiation_expression
    // - `(args)` is the call_expression wrapping the instantiation_expression
    // No precedence conflict with binary_expression's '<' because
    // generic_arg_list uses the external _generic_open token (not '<').
    // The external scanner disambiguates via lookahead.
    instantiation_expression: $ => prec(PREC.POSTFIX, seq(
      field('value', $._expression),
      field('type_arguments', $.generic_arg_list),
    )),

    method_call_expression: $ => choice(
      // With turbofish generics: obj.method::<T>(args)
      prec.left(PREC.POSTFIX + 1, seq(
        field('value', $._expression),
        '.',
        field('method', $.identifier),
        '::',
        $.generic_arg_list,
        field('arguments', $.call_arg_list),
      )),
      // Without generics: obj.method(args)
      prec.left(PREC.POSTFIX, seq(
        field('value', $._expression),
        '.',
        field('method', $.identifier),
        field('arguments', $.call_arg_list),
      )),
    ),

    field_expression: $ => prec.left(PREC.POSTFIX, seq(
      field('value', $._expression),
      '.',
      field('field', choice($.identifier, $.integer_literal)),
    )),

    index_expression: $ => prec(PREC.POSTFIX, seq(
      field('value', $._expression),
      '[',
      field('index', $._expression),
      ']',
    )),

    // scoped_path: used in both expression and type contexts
    // Left side uses _path for recursive qualification
    scoped_path: $ => prec(1, seq(
      field('path', $._path),
      '::',
      field('name', choice($.identifier, 'self', 'Self', 'super', 'ingot')),
    )),

    record_expression: $ => prec.dynamic(-1, seq(
      field('type', $.path),
      field('body', $.record_field_list),
    )),

    record_field_list: $ => seq(
      '{',
      sepTrailing($.record_field, ','),
      '}',
    ),

    record_field: $ => choice(
      seq(
        field('name', $.identifier),
        ':',
        field('value', $._expression),
      ),
      field('value', $.identifier),
    ),

    tuple_expression: $ => seq(
      '(',
      optional(seq(
        $._expression,
        ',',
        sepTrailing($._expression, ','),
      )),
      ')',
    ),

    array_expression: $ => seq(
      '[',
      sepTrailing($._expression, ','),
      ']',
    ),

    array_repeat_expression: $ => seq(
      '[',
      field('value', $._expression),
      ';',
      field('length', $._expression),
      ']',
    ),

    paren_expression: $ => seq(
      '(',
      $._expression,
      ')',
    ),

    // Condition expression: excludes record_expression to avoid ambiguity
    // with the opening { of a block. Same approach as Rust's tree-sitter grammar.
    // "if x < Foo { ... }" would be ambiguous: comparison+block vs record expression.
    _condition: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.cast_expression,
      $.call_expression,
      $.method_call_expression,
      $.instantiation_expression,
      $.field_expression,
      $.index_expression,
      prec.left($.identifier),
      $.scoped_path,
      'self',
      'Self',
      'super',
      'ingot',
      // NOTE: record_expression is deliberately excluded here
      $.tuple_expression,
      $.array_expression,
      $.array_repeat_expression,
      $.paren_expression,
      $.literal,
      $.if_expression,
      $.match_expression,
      $.with_expression,
      $.block,
      $.assignment_expression,
      $.augmented_assignment_expression,
      $.range_expression,
      $.qualified_path_expression,
    ),

    if_expression: $ => prec.right(seq(
      'if',
      field('condition', $._condition),
      prec.dynamic(1, field('consequence', $.block)),
      optional(seq(
        'else',
        field('alternative', choice($.if_expression, $.block)),
      )),
    )),

    match_expression: $ => seq(
      'match',
      field('value', $._condition),
      field('body', $.match_arm_list),
    ),

    match_arm_list: $ => seq(
      '{',
      repeat($.match_arm),
      '}',
    ),

    match_arm: $ => seq(
      field('pattern', $._pattern),
      '=>',
      field('value', $._expression),
      choice(',', $._terminator),
    ),

    with_expression: $ => seq(
      'with',
      field('params', $.with_param_list),
      field('body', $.block),
    ),

    with_param_list: $ => seq(
      '(',
      sepTrailing($.with_param, ','),
      ')',
    ),

    with_param: $ => choice(
      seq(
        field('key', $.path),
        '=',
        field('value', $._expression),
      ),
      field('value', $._expression),
    ),

    assignment_expression: $ => prec.right(PREC.ASSIGN, seq(
      field('left', $._expression),
      '=',
      field('right', $._expression),
    )),

    augmented_assignment_expression: $ => prec.right(PREC.ASSIGN, seq(
      field('left', $._expression),
      field('operator', choice('+=', '-=', '*=', '/=', '%=', '**=', '|=', '&=', '^=', '<<=', '>>=')),
      field('right', $._expression),
    )),

    range_expression: $ => prec.left(PREC.RANGE, seq(
      field('start', $._expression),
      '..',
      field('end', $._expression),
    )),

    call_arg_list: $ => seq(
      '(',
      sepTrailing($.call_arg, ','),
      ')',
    ),

    call_arg: $ => choice(
      seq(
        field('label', $.identifier),
        ':',
        field('value', $._expression),
      ),
      field('value', $._expression),
    ),

    // ==================== STATEMENTS ====================

    _statement: $ => choice(
      $.let_statement,
      $.for_statement,
      $.while_statement,
      $.return_statement,
      $.break_statement,
      $.continue_statement,
      $.expression_statement,
    ),

    block: $ => seq(
      '{',
      repeat(choice($._statement, $._item)),
      '}',
    ),

    let_statement: $ => seq(
      'let',
      optional('mut'),
      field('name', $._pattern),
      optional(seq(':', field('type', $._type))),
      optional(seq('=', field('value', $._expression))),
      $._terminator,
    ),

    for_statement: $ => seq(
      'for',
      field('pattern', $._pattern),
      'in',
      field('iterable', $._condition),
      prec.dynamic(1, field('body', $.block)),
    ),

    while_statement: $ => seq(
      'while',
      field('condition', $._condition),
      prec.dynamic(1, field('body', $.block)),
    ),

    return_statement: $ => prec.right(seq(
      'return',
      optional(field('value', $._expression)),
      $._terminator,
    )),

    break_statement: $ => seq('break', $._terminator),

    continue_statement: $ => seq('continue', $._terminator),

    expression_statement: $ => seq($._expression, $._terminator),

    // ==================== PATTERNS ====================

    _pattern: $ => choice(
      $.wildcard_pattern,
      $.rest_pattern,
      $.literal_pattern,
      $.identifier_pattern,
      $.tuple_pattern,
      $.path_pattern,
      $.path_tuple_pattern,
      $.record_pattern,
      $.or_pattern,
    ),

    wildcard_pattern: $ => '_',

    rest_pattern: $ => '..',

    literal_pattern: $ => choice(
      $.integer_literal,
      $.string_literal,
      $.boolean_literal,
      seq('-', $.integer_literal),
    ),

    identifier_pattern: $ => prec(-1, $.identifier),

    tuple_pattern: $ => seq(
      '(',
      sepTrailing($._pattern, ','),
      ')',
    ),

    path_pattern: $ => prec(1, $.path),

    path_tuple_pattern: $ => prec(2, seq(
      $.path,
      '(',
      sepTrailing($._pattern, ','),
      ')',
    )),

    record_pattern: $ => prec(3, seq(
      $.path,
      '{',
      sepTrailing($.record_pattern_field, ','),
      '}',
    )),

    record_pattern_field: $ => choice(
      seq(
        field('name', $.identifier),
        ':',
        field('pattern', $._pattern),
      ),
      field('name', $.identifier),
      $.rest_pattern,
    ),

    or_pattern: $ => prec.left(seq(
      $._pattern,
      '|',
      $._pattern,
    )),

    // ==================== PATHS ====================

    // _path: shared between expression and type contexts
    // Used as the left side of scoped_path for recursive qualification
    _path: $ => choice(
      $.identifier,
      $.scoped_path,
      'self',
      'Self',
      'super',
      'ingot',
    ),

    // path: used in type/pattern/non-expression contexts (use statements, type refs, etc.)
    path: $ => prec.right(seq(
      $.path_segment,
      repeat(prec.right(PREC.PATH + 1, seq('::', $.path_segment))),
    )),

    path_segment: $ => choice(
      $.identifier,
      'self',
      'Self',
      'super',
      'ingot',
    ),

    // ==================== ATTRIBUTES ====================

    attribute_list: $ => repeat1(choice($.attribute, $.doc_comment)),

    attribute: $ => seq(
      '#',
      '[',
      field('name', choice(prec(2, $.identifier), $.path)),
      optional(choice(
        $.attribute_arg_list,
        // Direct key = value form: #[selector = 0x01]
        seq(choice('=', ':'), field('value', choice(
          $.identifier,
          $.integer_literal,
          $.string_literal,
          $.boolean_literal,
        ))),
      )),
      ']',
    ),

    attribute_arg_list: $ => seq(
      '(',
      sepTrailing($.attribute_arg, ','),
      ')',
    ),

    attribute_arg: $ => seq(
      field('key', $.identifier),
      optional(seq(
        choice('=', ':'),
        field('value', choice(
          $.identifier,
          $.integer_literal,
          $.string_literal,
          $.boolean_literal,
        )),
      )),
    ),

    visibility: $ => 'pub',

    // ==================== LITERALS ====================

    literal: $ => choice(
      $.integer_literal,
      $.string_literal,
      $.boolean_literal,
    ),

    integer_literal: $ => token(choice(
      /[0-9]+(?:_[0-9]+)*/,
      /0[bB][0-1]+(?:_[0-1]+)*/,
      /0[oO][0-7]+(?:_[0-7]+)*/,
      /0[xX][0-9a-fA-F]+(?:_[0-9a-fA-F]+)*/,
    )),

    string_literal: $ => seq(
      '"',
      repeat(choice(
        $._string_content,
        $.escape_sequence,
      )),
      '"',
    ),

    _string_content: $ => token.immediate(prec(1, /[^"\\]+/)),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[\\'"nrt0]/,
        /x[0-9a-fA-F]{2}/,
        /u\{[0-9a-fA-F]+\}/,
      ),
    )),

    boolean_literal: $ => choice('true', 'false'),

    // ==================== COMMENTS ====================

    // Regular comment: // but NOT ///
    // NOTE: Must NOT use prec(-1) here. Negative precedence on extras causes
    // tree-sitter to fail to match line comments in certain parser states
    // (e.g., between recv arms, after method chain suppression), resulting
    // in `//` being consumed as two `/` tokens and breaking the parse.
    line_comment: $ => token(seq('//', /[^\n]*/)),

    // Doc comment: /// (higher priority than line_comment)
    doc_comment: $ => token(prec(1, seq('///', /[^\n]*/))),

    block_comment: $ => seq(
      '/*',
      optional($._block_comment_content),
      $._block_comment_end,
    ),

    // ==================== IDENTIFIERS ====================

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
  },
});

// Helper: separated by separator with optional trailing
function sepTrailing(rule, sep) {
  return optional(seq(rule, repeat(seq(sep, rule)), optional(sep)));
}

// Helper: one or more separated by separator
function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
