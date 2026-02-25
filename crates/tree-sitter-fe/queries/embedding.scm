(
    [(line_comment) (attribute)]* @context
    .
    [
        (struct_definition
            name: (_) @name)

        (enum_definition
            name: (_) @name)

        (impl_block
            type: (_) @name)

        (impl_trait
            trait: (_) @name
            "for" @name
            type: (_) @name)

        (trait_definition
            name: (_) @name)

        (contract_definition
            name: (_) @name)

        (msg_definition
            name: (_) @name)

        (function_definition
            name: (_) @name
            body: (block
                "{" @keep
                "}" @keep) @collapse)
        ] @item
    )

(attribute) @collapse
(use_statement) @collapse
