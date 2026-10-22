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
    $._quote_keyword,
  ],

  word: $ => $.identifier,

  supertypes: $ => [
    $._type,
    $._pattern,
    $._item,
    $._statement,
  ],

  conflicts: $ => [
    [$.record_expression, $._pattern_path],
    [$._pattern_path, $.path],
    [$.where_clause, $._expression, $._condition_atom_no_let],
    [$.record_field, $.record_pattern_field],
    [$.where_clause, $._condition_atom_no_let],
    [$.tuple_expression, $.tuple_pattern],
    [$._expression, $._pattern],
    [$._expression, $.match_arm_list],
    [$._expression, $.identifier_pattern, $.path_segment],
    [$.match_arm_list, $.block],
    [$.literal_pattern, $.literal],
    [$._expression, $._condition_atom_no_let, $._path],
    [$._expression, $._condition_atom_no_let, $._path, $.path_segment],
    [$.where_clause],
    [$._type, $._expression],
    // `pub (` in a tuple struct field is either a visibility restriction such
    // as `pub(ingot)` or a plain `pub` followed by a tuple type.
    [$.visibility],
    [$.qualified_path_type, $.qualified_path_expression],
    [$.tuple_type, $.tuple_expression],
    // Self type vs self path segment vs expression
    [$.self_type, $.path_segment],
    [$.self_type, $._expression, $.path_segment],
    // `Fn(..)` callable shape vs a path in a where-clause bound
    [$.fn_shape, $.path_segment],
    // `Step<ref> = ..` vs a generic type argument `Step<T>`, until the `=`
    [$.assoc_type_generic_arg, $.path_segment],
    // recv arm pattern
    [$.recv_arm_pattern],
    // _condition variants use the same terminals as expressions.
    [$._condition_atom_no_let, $.path_segment],
    [$._condition_let_value, $.path_segment],
    // scoped_path / path ambiguity: identifiers, scoped_path, keywords can be
    // expression, _path (for further ::), or path_segment (for type paths)
    [$._expression, $.path_segment],
    [$._path, $.path_segment],
    // When seeing `ident <`, could be expression (instantiation), _path (scoped_path
    // with generic args), or path_segment. Need three-way conflict.
    [$._expression, $._path, $.path_segment],
    [$._expression, $._path],
    [$._expression, $._condition_atom_no_let],
    [$._expression, $._condition_let_value],
    [$._condition_atom, $._condition_no_or_no_let],
    // attribute name can be identifier or path (path starts with path_segment which is identifier)
    [$.path_segment, $.attribute],
    // recv arm pattern name can be identifier or path
    [$.recv_arm_pattern, $.path_segment],
    // an attribute argument can be a key or an expression
    [$.attribute_arg, $._expression],
    // an attribute list after a contract field can start another field, the
    // `init` block or a `recv` block
    [$.contract_fields],
  ],

  rules: {
    // A statement at the top level is read as one only when nothing else
    // reads its tokens, so a block after a function reads as the function's
    // body or a braced `where` condition first.
    source_file: $ => repeat(choice($._item, prec.dynamic(-1, $._statement))),

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
      $._function_signature,
      // A block that could be the body or a braced `where` condition after a
      // `,` is the body, as the compiler's parser reads it.
      optional(field('body', prec.dynamic(1, $.block))),
    )),

    // A `uses` right after a signature is its clause, not a trait row item.
    _function_signature: $ => prec.right(seq(
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
    )),

    parameter_list: $ => seq(
      '(',
      sepTrailing($.parameter, ','),
      ')',
    ),

    parameter: $ => choice(
      // spread parameter: `..A`, the parameters a shape lists
      seq('..', field('type', $._type)),
      // self parameter: [mut|mut?|var] [ref|own] self [: Type]
      // e.g., `self`, `mut self`, `mut? self`, `own self`, `var own self`, `ref self`
      seq(optional(choice('mut', 'mut?', 'var')), optional(choice('ref', 'own')), 'self', optional(seq(':', $._type))),
      // labeled parameter: [var|ref|own] label name : Type
      // e.g., `from sender: address`, `_ val: u256`, `var _ p: own Packed`
      seq(
        optional(choice('mut', 'var', 'ref', 'own')),
        field('label', choice($.identifier, '_')),
        field('name', choice($.identifier, '_')),
        ':',
        field('type', $._type),
      ),
      // unlabeled parameter: [var|ref|own] name : Type
      // e.g., `bar: i32`, `var baz: own u256`
      seq(
        optional(choice('mut', 'var', 'ref', 'own')),
        field('name', choice($.identifier, '_')),
        ':',
        field('type', $._type),
      ),
      // destructured parameter: _ pattern : Type
      // e.g., `_ (a, b): (u256, u256)`
      seq(
        '_',
        field('pattern', $._parameter_pattern),
        ':',
        field('type', $._type),
      ),
    ),

    // A pattern a parameter destructures with.
    _parameter_pattern: $ => choice(
      $.tuple_pattern,
      $.path_tuple_pattern,
      $.record_pattern,
    ),

    // Struct definition
    struct_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      'struct',
      field('name', $.identifier),
      optional($.generic_param_list),
      choice(
        seq(
          optional($.where_clause),
          field('body', $.record_field_def_list),
        ),
        seq(
          field('body', $.tuple_field_def_list),
          optional($.where_clause),
        ),
      ),
    ),

    tuple_field_def_list: $ => seq(
      '(',
      sepTrailing($.tuple_field_def, ','),
      ')',
    ),

    tuple_field_def: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      field('type', $._type),
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
      optional('unsafe'),
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
      optional($.attribute_list),
      $.record_field_def,
      optional(','),
    ),

    contract_init: $ => seq(
      optional($.attribute_list),
      'init',
      $.parameter_list,
      optional($.uses_clause),
      field('body', $.block),
    ),

    contract_recv: $ => seq(
      optional($.attribute_list),
      'recv',
      optional(field('message_type', $.path)),
      '{',
      repeat($.recv_arm),
      '}',
    ),

    recv_arm: $ => seq(
      optional($.attribute_list),
      $.recv_arm_pattern,
      optional(seq('->', field('return_type', $._type))),
      optional($.uses_clause),
      field('body', $.block),
    ),

    recv_arm_pattern: $ => choice(
      $.wildcard_pattern,
      seq(
        field('name', choice($.identifier, $.path)),
        optional(seq(
          '{',
          sepTrailing(choice(
            // Labeled binding: `a: x` or `a: (x, y)`
            seq(field('name', $.identifier), ':', field('binding', $._pattern)),
            $.identifier,
            $.rest_pattern,
          ), ','),
          '}',
        )),
      ),
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

    uses_param: $ => prec.left(choice(
      // labeled: `name: Type` or `mut name: Type` or `name: mut Type`
      // (mode prefix on type handled by mode_type in _type rule)
      seq(
        optional(choice('mut', 'mut?', 'ref', 'own')),
        field('name', choice($.identifier, '_')),
        ':',
        field('type', $._type),
      ),
      // unlabeled: `Type` or `mut Type` or `Storage<T>`
      seq(
        optional(choice('mut', 'mut?', 'ref', 'own')),
        field('type', $.path),
        optional($.generic_arg_list),
      ),
    )),

    // Trait definition
    trait_definition: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
      optional('unsafe'),
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

    trait_ref: $ => choice(
      seq(
        $.path,
        optional($.generic_arg_list),
      ),
      $.fn_shape,
    ),

    // `Fn(own A, B) -> R`: the callable trait of a parameter list; `-> _`
    // leaves the result open
    fn_shape: $ => prec.right(seq(
      field('name', $.identifier),
      field('params', $.tuple_type),
      optional(seq('->', field('return_type', choice($._type, '_')))),
    )),

    trait_item_list: $ => seq(
      '{',
      repeat(choice(
        $.function_definition,
        $.trait_type_item,
        $.trait_const_item,
        $.trait_uses_item,
        $.trait_shape_item,
      )),
      '}',
    ),

    // Associated shape: `shape Step`, or `shape Step = (own usize, D::Step)`
    trait_shape_item: $ => seq(
      optional($.attribute_list),
      'shape',
      field('name', $.identifier),
      optional(seq('=', $._type)),
    ),

    // Associated effect row: `uses E`, or `uses E = (store: mut Store)`
    trait_uses_item: $ => seq(
      optional($.attribute_list),
      'uses',
      field('name', $.identifier),
      optional(seq('=', choice($.uses_param_list, $.uses_param))),
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

    // Like trait_const_item, but inherent impl consts may carry a
    // visibility modifier.
    impl_const_item: $ => seq(
      optional($.attribute_list),
      optional($.visibility),
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

    impl_trait: $ => choice(
      seq(
        optional($.attribute_list),
        optional('unsafe'),
        'impl',
        optional($.generic_param_list),
        field('trait', $.trait_ref),
        'for',
        field('type', $._type),
        optional(seq('as', field('selection_name', $.identifier))),
        optional($.where_clause),
        field('body', $.trait_item_list),
      ),
      seq(
        optional($.attribute_list),
        $.visibility,
        optional('unsafe'),
        'impl',
        optional($.generic_param_list),
        field('trait', $.trait_ref),
        'for',
        field('type', $._type),
        'as',
        field('selection_name', $.identifier),
        optional($.where_clause),
        field('body', $.trait_item_list),
      ),
    ),

    impl_item_list: $ => seq(
      '{',
      repeat(choice($.function_definition, $.impl_const_item, $.quote_splice)),
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
      // An extern function has no body, so a block in its `where` clause is
      // always a condition.
      repeat(alias($._function_signature, $.function_definition)),
      '}',
    ),

    // ==================== GENERICS ====================

    generic_param_list: $ => seq(
      $._generic_open,
      sepTrailing(choice($.type_generic_param, $.const_generic_param, $.shape_generic_param), ','),
      '>',
    ),

    // `shape A`: a callable's parameter list
    shape_generic_param: $ => seq(
      'shape',
      field('name', $.identifier),
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
      optional(seq('=', field('default', choice($.block, $.literal)))),
    ),

    type_bound_list: $ => choice(
      $.type_bound,
      prec.right(PREC.UNARY + 1, seq($.type_bound_list, '+', $.type_bound)),
    ),

    type_bound: $ => choice(
      // Trait bound: Path<Args>, with the modes it grants: `mut Index<usize>`
      prec.right(PREC.PATH + 5, seq(optional(choice('mut', 'mut?')), $.path, optional($.generic_arg_list))),
      // Callable shape: Fn(own A, B) -> R
      $.fn_shape,
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
      $.mode_generic_arg,
      $._type,
      // Const generic args: block expressions like {3 + 4} and literals
      $.block,
      $.literal,
    ),

    // `ref` or `mut`: the mode an associated shape is named at, `D::Step<ref>`
    mode_generic_arg: $ => choice('ref', 'mut'),

    // `Item = T`, or an associated shape at a mode, `Step<ref> = ref T`
    assoc_type_generic_arg: $ => seq(
      field('name', $.identifier),
      optional(field('mode', $.generic_arg_list)),
      '=',
      field('type', $._type),
    ),

    where_clause: $ => seq(
      'where',
      sep1(choice($.where_predicate, $.where_const_predicate, $.quote_splice), ','),
      optional(','),
    ),

    where_predicate: $ => seq(
      $._type,
      ':',
      $.type_bound_list,
    ),

    // A const condition reads an expression the way an `if` condition does,
    // without a record literal, whose `{` would be the item's.
    where_const_predicate: $ => $._condition_no_let,

    // ==================== TYPES ====================

    _type: $ => choice(
      $.quote_splice,
      $.path_type,
      $.tuple_type,
      $.array_type,
      $.pointer_type,
      $.mode_type,
      $.self_type,
      $.never_type,
      $.qualified_path_type,
    ),

    // Mode-prefixed type: ref Foo, mut Foo, mut? Foo, own Foo
    mode_type: $ => seq(
      field('mode', choice('ref', 'mut', 'mut?', 'own')),
      field('type', $._type),
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
      $.quote_expression,
      $.quote_splice,
      $.binary_expression,
      $.unary_expression,
      $.cast_expression,
      $.call_expression,
      $.macro_call_expression,
      $.method_call_expression,
      $.instantiation_expression,
      $.field_expression,
      $.index_expression,
      $.try_expression,
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
      $.unsafe_expression,
      $.closure_expression,
      $.block,
      $.assignment_expression,
      $.augmented_assignment_expression,
      $.range_expression,
      $.qualified_path_expression,
      $.mode_expression,
    ),

    // Mode expression: mut expr, ref expr, own expr
    // Creates a borrow/move of the given expression.
    // e.g., `mut x`, `ref s.v`, `let p = mut foo`
    mode_expression: $ => prec(PREC.UNARY, seq(
      field('mode', choice('mut', 'mut?', 'ref', 'own')),
      field('value', $._expression),
    )),

    // Qualified path, including the ordinary impl operand <T as Trait>.
    qualified_path_expression: $ => prec.right(seq(
      $._generic_open,
      field('type', $._type),
      'as',
      field('trait', $.trait_ref),
      '>',
      repeat(seq('::', field('name', $.identifier))),
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

    // Condition binary expressions without `||`.
    // Used by let-chains so unparenthesized `||` is rejected whenever
    // a `let` condition is part of the chain.
    condition_binary_expression_no_or: $ => {
      const table = [
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
            field('left', $._condition_no_or),
            field('operator', operator),
            field('right', $._condition_no_or),
          ))
        ),
        // ** is right-associative
        prec.right(PREC.EXP, seq(
          field('left', $._condition_no_or),
          field('operator', '**'),
          field('right', $._condition_no_or),
        )),
      );
    },

    // Condition binary expressions used in places where `let` conditions
    // are not allowed (`for .. in`, `match` scrutinee, `||` chains).
    condition_binary_expression_no_or_no_let: $ => {
      const table = [
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
            field('left', $._condition_no_or_no_let),
            field('operator', operator),
            field('right', $._condition_no_or_no_let),
          ))
        ),
        // ** is right-associative
        prec.right(PREC.EXP, seq(
          field('left', $._condition_no_or_no_let),
          field('operator', '**'),
          field('right', $._condition_no_or_no_let),
        )),
      );
    },

    // let-condition rhs follows the compiler parser's higher minimum
    // precedence and therefore excludes top-level `&&` / `||`.
    condition_binary_expression_let_value: $ => {
      const table = [
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
            field('left', $._condition_let_value),
            field('operator', operator),
            field('right', $._condition_let_value),
          ))
        ),
        // ** is right-associative
        prec.right(PREC.EXP, seq(
          field('left', $._condition_let_value),
          field('operator', '**'),
          field('right', $._condition_let_value),
        )),
      );
    },

    unary_expression: $ => prec(PREC.UNARY, seq(
      field('operator', choice('!', '-', '~', '+', '*')),
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

    macro_call_expression: $ => prec(PREC.POSTFIX, seq(
      field('function', $._expression),
      '!',
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
      field('field', choice($.identifier, $.integer_literal, $.quote_splice)),
    )),

    try_expression: $ => prec(PREC.POSTFIX, seq(
      field('value', $._expression),
      '?',
    )),

    index_expression: $ => prec(PREC.POSTFIX, seq(
      field('value', $._expression),
      '[',
      field('index', $._expression),
      ']',
    )),

    // scoped_path: used in both expression and type contexts
    // Left side uses _path for recursive qualification.
    // Also supports generic args on intermediate segments: Foo<T>::method
    scoped_path: $ => choice(
      prec(1, seq(
        field('path', $._path),
        '::',
        field('name', choice($.identifier, 'self', 'Self', 'super', 'ingot')),
      )),
      // Generic path segment: Foo<T>::name, Self::Ptr<T>::name
      prec(1, seq(
        field('path', $._path),
        field('type_arguments', $.generic_arg_list),
        '::',
        field('name', choice($.identifier, 'self', 'Self', 'super', 'ingot')),
      )),
    ),

    record_expression: $ => prec.dynamic(-1, seq(
      field('type', $.path),
      optional(field('type_arguments', $.generic_arg_list)),
      field('body', $.record_field_list),
    )),

    record_field_list: $ => seq(
      '{',
      sepTrailing($.record_field, ','),
      '}',
    ),

    record_field: $ => choice(
      seq(
        field('name', choice($.identifier, $.quote_splice)),
        ':',
        field('value', $._expression),
      ),
      field('value', choice($.identifier, $.quote_splice)),
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

    // Condition expression atoms excluding let-conditions and condition-specific
    // binary operator nodes.
    _condition_atom_no_let: $ => choice(
      $.quote_splice,
      $.unary_expression,
      $.cast_expression,
      $.call_expression,
      $.macro_call_expression,
      $.method_call_expression,
      $.instantiation_expression,
      $.field_expression,
      $.index_expression,
      $.try_expression,
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
      $.unsafe_expression,
      $.block,
      $.assignment_expression,
      $.augmented_assignment_expression,
      $.range_expression,
      $.qualified_path_expression,
      $.mode_expression,
    ),

    // Condition expression atoms, including let-conditions.
    _condition_atom: $ => choice(
      $._condition_atom_no_let,
      $.let_condition,
    ),

    // Condition expressions without top-level `||`.
    _condition_no_or: $ => choice(
      $.condition_binary_expression_no_or,
      $._condition_atom,
    ),

    // No-let condition expressions without top-level `||`.
    _condition_no_or_no_let: $ => choice(
      $.condition_binary_expression_no_or_no_let,
      $._condition_atom_no_let,
    ),

    // No-let condition expressions with `||`.
    condition_or_expression_no_let: $ => prec.left(PREC.OR, seq(
      field('left', $._condition_no_let),
      field('operator', '||'),
      field('right', $._condition_no_or_no_let),
    )),

    // Condition expressions where `let` is never allowed.
    _condition_no_let: $ => choice(
      $.condition_or_expression_no_let,
      $._condition_no_or_no_let,
    ),

    // Full condition expression used by `if`/`while`.
    _condition: $ => choice(
      $.condition_or_expression_no_let,
      $._condition_no_or,
    ),

    // let-condition rhs allows tighter operators but excludes top-level `&&`
    // and `||` to match compiler parsing behavior.
    _condition_let_value: $ => choice(
      $.quote_splice,
      $.condition_binary_expression_let_value,
      $.unary_expression,
      $.cast_expression,
      $.call_expression,
      $.macro_call_expression,
      $.method_call_expression,
      $.instantiation_expression,
      $.field_expression,
      $.index_expression,
      $.try_expression,
      prec.left($.identifier),
      $.scoped_path,
      'self',
      'Self',
      'super',
      'ingot',
      $.tuple_expression,
      $.array_expression,
      $.array_repeat_expression,
      $.paren_expression,
      $.literal,
      $.if_expression,
      $.match_expression,
      $.with_expression,
      $.unsafe_expression,
      $.block,
      $.qualified_path_expression,
      $.mode_expression,
    ),

    // Destructuring condition for `if`/`while` condition chains.
    // `let` participates in `&&` chains only. Mixing with unparenthesized
    // `||` is rejected at the grammar level.
    let_condition: $ => prec.left(PREC.AND, seq(
      'let',
      field('pattern', $._pattern),
      '=',
      field('value', $._condition_let_value),
    )),

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
      field('value', $._condition_no_let),
      field('body', $.match_arm_list),
    ),

    match_arm_list: $ => seq(
      '{',
      repeat(choice($.match_arm, seq($.quote_splice, choice(',', $._terminator)))),
      '}',
    ),

    match_arm: $ => seq(
      field('pattern', $._pattern),
      '=>',
      field('value', choice(
        $._expression,
        $.match_arm_return,
        $.match_arm_yield,
        $.match_arm_break,
        $.match_arm_continue,
      )),
      choice(',', $._terminator),
    ),

    // Statement-like keywords allowed in match arm bodies.
    // These mirror return/break/continue_statement but without a trailing
    // terminator (the match_arm rule already handles that).
    match_arm_return: $ => prec.right(seq(
      'return',
      optional(field('value', $._expression)),
    )),

    match_arm_yield: $ => seq('yield', field('value', $._expression)),

    match_arm_break: $ => 'break',

    match_arm_continue: $ => 'continue',

    quote_expression: $ => seq(
      alias($._quote_keyword, 'quote'),
      optional($.quote_open_list),
      field('body', choice($.block, $.quote_where, $.match_arm_list)),
    ),

    quote_where: $ => seq('{', $.where_clause, '}'),

    quote_open_list: $ => seq('(', sepTrailing($.identifier, ','), ')'),

    quote_splice: $ => seq('$', '{', field('value', $._expression), '}'),

    with_expression: $ => seq(
      'with',
      field('params', $.with_param_list),
      field('body', $.block),
    ),

    unsafe_expression: $ => seq(
      'unsafe',
      field('body', $.block),
    ),

    // Closure: |x| x + 1, |acc: own, x: u256| -> u256 { acc + x }, || true
    // The body extends as far as possible.
    closure_expression: $ => prec.right(seq(
      choice('||', seq('|', sepTrailing($.closure_parameter, ','), '|')),
      optional(seq('->', field('return_type', $._type))),
      field('body', $._expression),
    )),

    // A closure parameter's type, or the type after its mode, may be omitted:
    // `x`, `x: own`, `x: own u256`, `x: u256`, `(a, b)`.
    closure_parameter: $ => seq(
      choice(
        seq(optional('mut'), field('name', choice($.identifier, '_'))),
        field('pattern', $._parameter_pattern),
      ),
      optional(seq(':', field('type', choice($._type, $.closure_mode)))),
    ),

    closure_mode: $ => choice('own', 'mut', 'mut?', 'ref'),

    with_param_list: $ => seq(
      '(',
      sepTrailing($.with_param, ','),
      ')',
    ),

    with_param: $ => choice(
      // Key = Value, where Key can be a generic type like Storage<u8>
      seq(
        field('key', $.path),
        optional($.generic_arg_list),
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
      $.yield_statement,
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
      choice('let', 'var'),
      field('name', $._pattern),
      optional(seq(':', field('type', $._type))),
      optional(seq(
        '=',
        field('value', $._expression),
        optional(seq('else', field('alternative', $.block))),
      )),
      $._terminator,
    ),

    for_statement: $ => seq(
      'for',
      field('pattern', $._pattern),
      'in',
      field('iterable', $._condition_no_let),
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

    yield_statement: $ => seq(
      'yield',
      field('value', $._expression),
      $._terminator,
    ),

    break_statement: $ => seq('break', $._terminator),

    continue_statement: $ => seq('continue', $._terminator),

    expression_statement: $ => seq($._expression, $._terminator),

    // ==================== PATTERNS ====================

    _pattern: $ => choice(
      $.quote_splice,
      $.wildcard_pattern,
      $.rest_pattern,
      $.literal_pattern,
      $.identifier_pattern,
      $.mut_pattern,
      $.maybe_mut_pattern,
      $.ref_pattern,
      $.var_pattern,
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

    // Mutable binding in pattern: `Some(mut t)`, `Foo { mut x }`
    mut_pattern: $ => prec(1, seq('mut', $._pattern)),

    // Binding at the item's mode in pattern: `Some(mut? t)`
    maybe_mut_pattern: $ => prec(1, seq('mut?', $._pattern)),

    // Read-access binding in pattern: `Some(ref t)`, `Foo { ref x }`
    ref_pattern: $ => prec(1, seq('ref', $._pattern)),

    // Mutable value binding in pattern: `Some(var t)`, `Foo { var x }`
    var_pattern: $ => prec(1, seq('var', $._pattern)),

    tuple_pattern: $ => seq(
      '(',
      sepTrailing($._pattern, ','),
      ')',
    ),

    path_pattern: $ => prec(1, $._pattern_path),

    // A pattern's path starts as any path does, a qualified type included:
    // `<T as Trait>::Out { x }`.
    _pattern_path: $ => choice($.path, $.qualified_path_type),

    path_tuple_pattern: $ => prec(2, seq(
      $._pattern_path,
      optional(field('type_arguments', $.generic_arg_list)),
      '(',
      sepTrailing($._pattern, ','),
      ')',
    )),

    record_pattern: $ => prec(3, seq(
      $._pattern_path,
      optional(field('type_arguments', $.generic_arg_list)),
      '{',
      sepTrailing($.record_pattern_field, ','),
      '}',
    )),

    record_pattern_field: $ => choice(
      seq(
        field('name', choice($.identifier, $.quote_splice)),
        ':',
        field('pattern', $._pattern),
      ),
      // Nested patterns: record, path_tuple, or scoped path patterns
      $.record_pattern,
      $.path_tuple_pattern,
      // Scoped path in field position: e.g., `Bar::Unit` inside `Foo { x, Bar::Unit }`
      $.scoped_path,
      field('name', choice($.identifier, $.quote_splice)),
      // Marked shorthand bindings: `Foo { mut x, ref y, var z }`
      $.mut_pattern,
      $.maybe_mut_pattern,
      $.ref_pattern,
      $.var_pattern,
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
        // Direct key = value form: #[selector = 0x01] or #[selector = sol("...")]
        seq(choice('=', ':'), field('value', $._attribute_value)),
      )),
      ']',
    ),

    attribute_arg_list: $ => seq(
      '(',
      sepTrailing($.attribute_arg, ','),
      ')',
    ),

    attribute_arg: $ => choice(
      seq(
        field('key', $.identifier),
        optional(seq(
          choice('=', ':'),
          field('value', $._attribute_value),
        )),
      ),
      // An expression argument: #[selector(0x01)], #[slot(BASE + 1)]
      prec(-1, field('value', $._expression)),
    ),

    // Attribute values: literals, identifiers, paths, or call expressions
    // e.g., `0x01`, `sol("transfer(address,uint256)")`, `std::abi::sol("...")`
    _attribute_value: $ => choice(
      $.identifier,
      $.integer_literal,
      $.string_literal,
      $.boolean_literal,
      // Function call in attribute: sol("...") or std::abi::sol("...")
      $.attribute_call_expression,
    ),

    attribute_call_expression: $ => prec(2, seq(
      field('function', choice(prec(3, $.identifier), $.path)),
      '(',
      sepTrailing(choice($.string_literal, $.integer_literal, $.identifier), ','),
      ')',
    )),

    visibility: $ => seq(
      'pub',
      optional(seq(
        '(',
        choice(
          'ingot',
          'super',
          seq('in', $.path),
        ),
        ')',
      )),
    ),

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
      /[\\"nrt]/,
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
