mod TestParser {
    use Parser.{parse, letter}

    @Test
    def letter01 : Bool = 
        let input = "a";
        let actual = parse(input, letter);
        let expected = Some('a'::Nil);
        Assert.eq(expected, actual)

    @Test
    def letter02 : Bool = 
        let input = ";";
        let actual = parse(input, letter);
        let expected = None;
        Assert.eq(expected, actual)
}