mod TestParser {
    use Parser.{parse, empty, symbol, digit, letter}

    /// empty

    @Test
    def empty01 : Bool = 
        let input = "abc";
        let actual = parse(input, empty);
        let expected = Err();
        Assert.eq(expected, actual)

    @Test
    def empty02 : Bool = 
        let input = "";
        let actual = parse(input, empty);
        let expected = Ok(Nil);
        Assert.eq(expected, actual)

    /// digit

    @Test
    def digit01 : Bool = 
        let input = "9";
        let actual = parse(input, digit);
        let expected = Ok('9'::Nil);
        Assert.eq(expected, actual)

    @Test
    def digit02 : Bool = 
        let input = " ";
        let actual = parse(input, digit);
        let expected = Err();
        Assert.eq(expected, actual)

        @Test
    def digit03 : Bool = 
        let input = "123";
        let actual = parse(input, digit);
        let expected = Err();
        Assert.eq(expected, actual)

    @Test
    def digit04 : Bool = 
        let input = "";
        let actual = parse(input, digit);
        let expected = Err();
        Assert.eq(expected, actual)

    @Test
    def digit05 : Bool = 
        let input = "a";
        let actual = parse(input, digit);
        let expected = Err();
        Assert.eq(expected, actual)

    /// letter

    @Test
    def letter01 : Bool = 
        let input = "a";
        let actual = parse(input, letter);
        let expected = Ok('a'::Nil);
        Assert.eq(expected, actual)

    @Test
    def letter02 : Bool = 
        let input = ";";
        let actual = parse(input, letter);
        let expected = Err();
        Assert.eq(expected, actual)

        @Test
    def letter03 : Bool = 
        let input = "zz";
        let actual = parse(input, letter);
        let expected = Err();
        Assert.eq(expected, actual)

    @Test
    def letter04 : Bool = 
        let input = "";
        let actual = parse(input, letter);
        let expected = Err();
        Assert.eq(expected, actual)

    @Test
    def letter05 : Bool = 
        let input = "4";
        let actual = parse(input, letter);
        let expected = Err();
        Assert.eq(expected, actual)

}