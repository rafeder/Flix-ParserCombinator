/*
mod TestLanguages {
    use Parser.{parse, empty, symbol, digit, letter,
                token, choice, many, many1, seq}

    
    // Language 1
    // L1 = (0 | 1)* 01110 (0 | 1)*
    // "Strings of 0 and 1 that contain substring 01110"

    def parseL1() : PResult \ Parsing = 
        seq(
            _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1'))),
            _ -> seq(
                _ -> token("01110"),
                _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1')))
            )
        )
        
    @Test
    def lang101 : Bool =
        let input = "01110001";
        let actual = parse(input, parseL1);
        let expected = PResult.ParseComplete('0'::'1'::'1'::'1'::'0'::'0'::'0'::'1'::Nil);
        Assert.eq(expected, actual)
    
    @Test
    def lang102 : Bool =
        let input = "111100001";
        let actual = parse(input, parseL1);
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def lang103 : Bool =
        let input = "";
        let actual = parse(input, parseL1);
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)
    
    @Test
    def lang104 : Bool =
        let input = "01110";
        let actual = parse(input, parseL1);
        let expected = PResult.ParseComplete('0'::'1'::'1'::'1'::'0'::Nil);
        Assert.eq(expected, actual)
}*/