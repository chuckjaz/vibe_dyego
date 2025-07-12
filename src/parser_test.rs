
#[cfg(test)]
mod tests {
    use pest::Parser;
    use crate::DyegoParser;
    use crate::Rule;

    #[test]
    fn test_value_type_declaration() {
        let dyego_code = "value MyValueType(val immutableField: i32, var mutableField: String) { }";
        let parse_result = DyegoParser::parse(Rule::value_type_declaration, dyego_code);
        assert!(parse_result.is_ok(), "Parsing failed: {:?}", parse_result.err());
    }

    #[test]
    fn test_value_type_declaration_empty_fields() {
        let dyego_code = "value MyValueType() { }";
        let parse_result = DyegoParser::parse(Rule::value_type_declaration, dyego_code);
        assert!(parse_result.is_ok(), "Parsing failed: {:?}", parse_result.err());
    }

    #[test]
    fn test_value_type_declaration_single_field() {
        let dyego_code = "value MyValueType(val field: i32) { }";
        let parse_result = DyegoParser::parse(Rule::value_type_declaration, dyego_code);
        assert!(parse_result.is_ok(), "Parsing failed: {:?}", parse_result.err());
    }
}
