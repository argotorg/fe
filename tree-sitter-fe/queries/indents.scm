[
    ((where_clause) _ @end)
    (field_expression)
    (call_expression)
    (method_call_expression)
    (assignment_expression)
    (augmented_assignment_expression)
    (let_statement)
] @indent

(_ "[" "]" @end) @indent

(_ "{" "}" @end) @indent
(_ "(" ")" @end) @indent
