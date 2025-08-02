# AI Reasoning

## 2025-08-02

I was asked to add BNF comments to the parser methods in `src/parser.rs`.

I have analyzed the file and added BNF comments to the following methods:
- `parse_statement`
- `parse_expression`
- `parse_logical_or`
- `parse_logical_and`
- `parse_equality`
- `parse_comparison`
- `parse_term`
- `parse_factor`
- `parse_unary`
- `parse_primary`
- `parse_if_expression`
- `parse_block`
- `parse_variable_statement`
- `parse_function_statement`
- `parse_value_type_declaration`
- `parse_function_parameters`
- `parse_parameter`
- `parse_value_field`
- `parse_type`

I also added a comment for the `statement` rule, which is not a method but a good entry point to understand the grammar.

I was also asked to create a section in the `README.md` file that summarizes the syntax of Dyego and to make a note in the `GEMINI.md` file that is now required to keep these in sync.

I have updated the `README.md` with a new `Syntax` section and added the synchronization note to `GEMINI.md`.