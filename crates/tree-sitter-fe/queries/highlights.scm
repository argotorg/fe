; === Types ===

; === Types ===
; Structural: identifiers in type positions (path_type, generic args, etc.)
(path_type (path (path_segment (identifier) @type)))

; Self type (standalone self_type node or Self as a path_segment keyword)
(self_type) @type.builtin
((path_segment) @type.builtin (#eq? @type.builtin "Self"))

; Fallback: assume uppercase identifiers are types/constructors elsewhere
((identifier) @type
 (#match? @type "^[A-Z]"))

; ALL_CAPS identifiers are constants
((identifier) @constant
 (#match? @constant "^_*[A-Z][A-Z\\d_]*$"))

; === Functions ===

(function_definition name: (identifier) @function.definition)

(call_expression
  function: [
    (identifier) @function
    (scoped_path name: (identifier) @function)
  ])

(method_call_expression
  method: (identifier) @function.method)

; === Traits and Impls ===

(trait_definition name: (identifier) @type.interface)
(impl_trait trait: (trait_ref (path (path_segment (identifier) @type.interface))))
(super_trait_list (trait_ref (path (path_segment (identifier) @type.interface))))
(type_bound (path (path_segment (identifier) @type.interface)))

; === Struct/Enum/Contract/Msg names ===

(struct_definition name: (identifier) @type)
(enum_definition name: (identifier) @type)
(contract_definition name: (identifier) @type)
(msg_definition name: (identifier) @type)

; === Enum/Msg variant names ===

(variant_def name: (identifier) @type.enum.variant)
(msg_variant name: (identifier) @type.enum.variant)

; === Fields ===

(field_expression field: (identifier) @property)
(record_field_def name: (identifier) @property)
(record_field name: (identifier) @property)
(record_pattern_field name: (identifier) @property)

; === Parameters and Local Variables ===

(parameter name: (identifier) @variable.parameter)
(uses_param name: (identifier) @variable.parameter)
(let_statement name: (path_pattern (path (path_segment (identifier) @variable))))
(let_statement name: (mut_pattern (path_pattern (path (path_segment (identifier) @variable)))))

; === Attributes ===

(attribute name: (identifier) @attribute)
(doc_comment) @comment.doc

; === Keywords ===
; Note: break, continue, pub, return, let are named nodes (break_statement, etc.)
; so they need separate patterns

[
  "as"
  "const"
  "contract"
  "else"
  "enum"
  "extern"
  "fn"
  "for"
  "if"
  "impl"
  "in"
  "init"
  "ingot"
  "match"
  "mod"
  "msg"
  "mut"
  "own"
  "recv"
  "self"
  "struct"
  "super"
  "trait"
  "type"
  "unsafe"
  "use"
  "uses"
  "where"
  "while"
  "with"
] @keyword

(break_statement) @keyword
(continue_statement) @keyword
(return_statement "return" @keyword)
(let_statement "let" @keyword)
(visibility) @keyword

; === Literals ===

(string_literal) @string
(escape_sequence) @string.escape
(integer_literal) @number
(boolean_literal) @constant

; === Comments ===

(line_comment) @comment
(block_comment) @comment

; === Operators ===

[
  "!="
  "%"
  "%="
  "&"
  "&="
  "&&"
  "*"
  "*="
  "**"
  "**="
  "+"
  "+="
  "-"
  "-="
  "->"
  ".."
  "/="
  ":"
  "<<"
  "<<="

  "<="
  "="
  "=="
  "=>"
  ">"
  ">="
  ">>"
  ">>="
  "^"
  "^="
  "|"
  "|="
  "||"
  "~"
] @operator

(unary_expression "!" @operator)

; === Punctuation ===

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  "."
  ","
  "::"
] @punctuation.delimiter

[
  "#"
] @punctuation.special
