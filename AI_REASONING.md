I was asked to fix a parse error. I started by running the tests and found that `test_parse_tuple_type` was failing.
The error was `Err("Expected token Equal, got Colon")`.
This was because the parser did not support type annotations in variable declarations.

To fix this, I did the following:
1.  Modified the `VariableStatement` struct in `src/ast.rs` to include an optional `type_annotation`.
2.  Updated the `parse_variable_statement` function in `src/parser.rs` to parse the optional type annotation.
3.  Fixed the `test_parse_tuple_type` test in `src/parser.rs` to have a valid input and a correct expected value.
4.  I also had to fix `test_parse_variable_statement` to include the new `type_annotation` field.
5.  Finally, I updated the `README.md` to reflect the changes in the language syntax.