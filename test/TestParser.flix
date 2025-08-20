/*
mod TestParser {
    use Parser.{parse, empty, symbol, digit, letter,
                token, choice, many, many1, seq}

    /// empty

    @Test
    def empty01 : Bool = 
        let input = "abc";
        let actual = parse(input, empty);
        let expected = PResult.ParseOk(Nil);
        Assert.eq(expected, actual)

    @Test
    def empty02 : Bool = 
        let input = "";
        let actual = parse(input, empty);
        let expected = PResult.ParseComplete(Nil);
        Assert.eq(expected, actual)

    /// symbol

    @Test
    def symbol01 : Bool = 
        let input = "!";
        let actual = parse(input, _ -> symbol('!'));
        let expected = PResult.ParseComplete('!'::Nil);
        Assert.eq(expected, actual)

    @Test
    def symbol02 : Bool = 
        let input = "?";
        let actual = parse(input, _ -> symbol('!'));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)
    
    @Test
    def symbol03 : Bool = 
        let input = "";
        let actual = parse(input, _ -> symbol('!'));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def symbol04 : Bool = 
        let input = "!!";
        let actual = parse(input, _ -> symbol('!'));
        let expected = PResult.ParseOk('!'::Nil);
        Assert.eq(expected, actual)
    
    /// digit

    @Test
    def digit01 : Bool = 
        let input = "3";
        let actual = parse(input, digit);
        let expected = PResult.ParseComplete('3'::Nil);
        Assert.eq(expected, actual)

    @Test
    def digit02 : Bool = 
        let input = "a";
        let actual = parse(input, digit);
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def digit03 : Bool = 
        let input = "";
        let actual = parse(input, digit);
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def digit04 : Bool = 
        let input = "12";
        let actual = parse(input, digit);
        let expected = PResult.ParseOk('1'::Nil);
        Assert.eq(expected, actual)

    /// letter

     @Test
    def letter01 : Bool = 
        let input = "c";
        let actual = parse(input, letter);
        let expected = PResult.ParseComplete('c'::Nil);
        Assert.eq(expected, actual)

    @Test
    def letter02 : Bool = 
        let input = "3";
        let actual = parse(input, letter);
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def letter03 : Bool = 
        let input = "";
        let actual = parse(input, letter);
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def letter04 : Bool = 
        let input = "aa";
        let actual = parse(input, letter);
        let expected = PResult.ParseOk('a'::Nil);
        Assert.eq(expected, actual)


    /// token

    @Test
    def token01 : Bool = 
        let input = "TRUE";
        let actual = parse(input, _ -> token("TRUE"));
        let expected = PResult.ParseComplete('T'::'R'::'U'::'E'::Nil);
        Assert.eq(expected, actual)

    @Test
    def token02 : Bool = 
        let input = "FALSE";
        let actual = parse(input, _ -> token("TRUE"));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def token03 : Bool = 
        let input = "";
        let actual = parse(input, _ -> token("TRUE"));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def token04 : Bool = 
        let input = "TRUE&&TRUE";
        let actual = parse(input, _ -> token("TRUE"));
        let expected = PResult.ParseOk('T'::'R'::'U'::'E'::Nil);
        Assert.eq(expected, actual)

    @Test
    def token05 : Bool = 
        let input = "TRIM";
        let actual = parse(input, _ -> token("TRUE"));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def token06 : Bool = 
        let input = "TRU";
        let actual = parse(input, _ -> token("TRUE"));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    /// choice

    @Test
    def choice01 : Bool = 
        let input = "a";
        let actual = parse(input, _ -> choice(letter, digit));
        let expected = PResult.ParseComplete('a'::Nil);
        Assert.eq(expected, actual)

    @Test
    def choice02 : Bool = 
        let input = "1";
        let actual = parse(input, _ -> choice(letter, digit));
        let expected = PResult.ParseComplete('1'::Nil);
        Assert.eq(expected, actual)

    @Test
    def choice03 : Bool = 
        let input = "c";
        let actual = parse(input, _ -> choice(_ -> symbol('c'), empty));
        let expected = PResult.ParseComplete('c'::Nil);
        Assert.eq(expected, actual)

    @Test
    def choice04 : Bool = 
        let input = "c";
        let actual = parse(input, _ -> choice(empty, _ -> symbol('c')));
        let expected = PResult.ParseOk(Nil);
        Assert.eq(expected, actual)

    @Test
    def choice05 : Bool = 
        let input = "abc";
        let actual = parse(input, _ -> choice(_ -> token("ab"), _ -> token("abc")));
        let expected = PResult.ParseOk('a'::'b'::Nil);
        Assert.eq(expected, actual)

    @Test
    def choice06 : Bool = 
        let input = "a";
        let actual = parse(input, _ -> choice(letter, _ -> symbol('a')));
        let expected = PResult.ParseComplete('a'::Nil);
        Assert.eq(expected, actual)

        @Test
    def choice07 : Bool = 
        let input = "aa";
        let actual = parse(input, _ -> choice(letter, _ -> symbol('a')));
        let expected = PResult.ParseOk('a'::Nil);
        Assert.eq(expected, actual)

    @Test
    def choice08 : Bool = 
        let input = "";
        let actual = parse(input, _ -> choice(letter, digit));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def choice09 : Bool = 
        let input = "";
        let actual = parse(input, _ -> choice(letter, empty));
        let expected = PResult.ParseComplete(Nil);
        Assert.eq(expected, actual)    

    // many 
    def mange(p : Unit -> PResult \ Parsing) : PResult \ Parsing = choice(_ -> many1(p), empty)
    @Test
    def many01 : Bool = 
        let input = "a";
        let actual = parse(input, _ -> mange(letter)); 
        let expected = PResult.ParseComplete('a'::'a'::'a'::Nil);
        Assert.eq(expected, actual)    
    
    @Test
    def many02 : Bool = 
        let input = "aaaz";
        let actual = parse(input, _ -> mange(_ -> symbol('a')));
        let expected = PResult.ParseOk('a'::'a'::'a'::Nil);
        Assert.eq(expected, actual)    
    
    @Test
    def many03 : Bool = 
        let input = "aaa";
        let actual = parse(input, _ -> many(digit));
        let expected = PResult.ParseOk(Nil);
        Assert.eq(expected, actual)  

    @Test
    def many04 : Bool = 
        let input = "";
        let actual = parse(input, _ -> many(letter));
        let expected = PResult.ParseComplete(Nil);
        Assert.eq(expected, actual)    
    
    // many1

    @Test
    def many101 : Bool = 
        let input = "aaa";
        let actual = parse(input, _ -> many1(letter));
        let expected = PResult.ParseComplete('a'::'a'::'a'::Nil);
        Assert.eq(expected, actual)    
    
    @Test
    def many102 : Bool = 
        let input = "aaaz";
        let actual = parse(input, _ -> many1(_ -> symbol('a')));
        let expected = PResult.ParseOk('a'::'a'::'a'::Nil);
        Assert.eq(expected, actual)    
    
    @Test
    def many103 : Bool = 
        let input = "aaa";
        let actual = parse(input, _ -> many1(digit));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def many104 : Bool = 
        let input = "";
        let actual = parse(input, _ -> many1(letter));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)


    /// seq

    @Test
    def seq01 : Bool = 
        let input = "a1";
        let actual = parse(input, _ -> seq(letter, digit));
        let expected = PResult.ParseComplete('a'::'1'::Nil);
        Assert.eq(expected, actual)

    @Test
    def seq02 : Bool = 
        let input = "a1";
        let actual = parse(input, _ -> seq(letter, letter));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def seq03 : Bool = 
        let input = "a1";
        let actual = parse(input, _ -> seq(digit, digit));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def seq04 : Bool = 
        let input = "aaa11";
        let actual = parse(input, _ -> seq(_ -> many(letter), digit));
        let expected = PResult.ParseOk('a'::'a'::'a'::'1'::Nil);
        Assert.eq(expected, actual)

    @Test
    def seq05 : Bool = 
        let input = "aaa";
        let actual = parse(input, _ -> seq(_ -> many1(letter), letter));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def seq06 : Bool = 
        let input = "11";
        let actual = parse(input, _ -> seq(_ -> many(letter), digit));
        let expected = PResult.ParseOk('1'::Nil);
        Assert.eq(expected, actual)


    @Test
    def seq07 : Bool = 
        let input = "1";
        let actual = parse(input, _ -> seq(_ -> many1(letter), digit));
        let expected = PResult.ParseFail;
        Assert.eq(expected, actual)

    @Test
    def seq08 : Bool = 
        let input = "a2";
        let actual = parse(input, _ -> seq(letter, _ -> choice(letter, digit)));
        let expected = PResult.ParseComplete('a'::'2'::Nil);
        Assert.eq(expected, actual)
}*/