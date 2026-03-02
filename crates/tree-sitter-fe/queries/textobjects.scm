; functions
(function_definition
    body: (_
        "{"
        (_)* @function.inside
        "}" )) @function.around

; classes / types
(struct_definition
    body: (_
        "{"
        [(_) ","?]* @class.inside
        "}" )) @class.around

(enum_definition
   body: (_
       "{"
       [(_) ","?]* @class.inside
       "}" )) @class.around

(trait_definition
    body: (_
        "{"
        [(_) ","?]* @class.inside
        "}" )) @class.around

(impl_block
    body: (_
        "{"
        [(_) ","?]* @class.inside
        "}" )) @class.around

(impl_trait
    body: (_
        "{"
        [(_) ","?]* @class.inside
        "}" )) @class.around

(contract_definition
    "{"
    (_)* @class.inside
    "}" ) @class.around

(mod_definition
    "{"
    (_)* @class.inside
    "}" ) @class.around

; comments
(line_comment)+ @comment.around
(block_comment) @comment.around
