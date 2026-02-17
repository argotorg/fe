(attribute) @annotation
(doc_comment) @annotation

(struct_definition
    (visibility)? @context
    "struct" @context
    name: (_) @name) @item

(enum_definition
    (visibility)? @context
    "enum" @context
    name: (_) @name) @item

(variant_def
    name: (_) @name) @item

(contract_definition
    (visibility)? @context
    "contract" @context
    name: (_) @name) @item

(msg_definition
    (visibility)? @context
    "msg" @context
    name: (_) @name) @item

(impl_block
    "impl" @context
    type: (_) @name
    body: (_ "{" @open (_)* "}" @close)) @item

(impl_trait
    "impl" @context
    trait: (_) @name
    "for" @context
    type: (_) @name
    body: (_ "{" @open (_)* "}" @close)) @item

(trait_definition
    (visibility)? @context
    "trait" @context
    name: (_) @name) @item

(function_definition
    (visibility)? @context
    "fn" @context
    name: (_) @name) @item

(mod_definition
    (visibility)? @context
    "mod" @context
    name: (_) @name) @item

(type_alias
    (visibility)? @context
    "type" @context
    name: (_) @name) @item

(trait_type_item
    "type" @context
    name: (_) @name) @item

(const_definition
    (visibility)? @context
    "const" @context
    name: (_) @name) @item

(trait_const_item
    "const" @context
    name: (_) @name) @item

(record_field_def
    (visibility)? @context
    name: (_) @name) @item
