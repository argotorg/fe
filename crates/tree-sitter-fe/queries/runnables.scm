; Fe test module
(
    (mod_definition
        name: (_) @run
        (#eq? @run "tests")
    )
    (#set! tag fe-mod-test)
)

; Fe test function (with #[test] attribute)
(
    (
        (attribute
            (identifier) @_attribute
            (#match? @_attribute "test")
        ) @_start
        .
        (attribute) *
        .
        (function_definition
            name: (_) @run
            body: _
        ) @_end
    )
    (#set! tag fe-test)
)
