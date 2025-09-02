/*
   Copyright 2025 rafeder

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

mod TestLanguages {
    use Parser.{empty, symbol, letter, digit,
                token, choice, seq, many, many1, invokeParser}

    
    /// Language 1
    /// L1 = (0 | 1)* 01110 (0 | 1)*
    /// "words consisting of 0s and 1s and contain substring 010"
    ///

    def parserL1() : PResult \ Parse = 
        seq(
            _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1'))),
            _ -> seq(
                _ -> token("010"),
                _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1')))
            )
        )
    
    def decideL1(input : String) : PResult = invokeParser(input, parserL1, x -> x::Nil, (l,r) -> l ::: r, Nil)
    
    @Test
    def lang1_01 : Bool =
        let input = "110101";
        let actual = decideL1(input);
        let expected = Ok('1'::'1'::'0'::'1'::'0'::'1'::Nil);
        Assert.eq(expected, actual)
    
    @Test
    def lang1_02 : Bool =
        let input = "1111000";
        let actual = decideL1(input);
        let expected = Err(());
        Assert.eq(expected, actual)

    @Test
    def lang1_03 : Bool =
        let input = "";
        let actual = decideL1(input);
        let expected = Err(());
        Assert.eq(expected, actual)
    
    @Test
    def lang1_04 : Bool =
        let input = "010010";
        let actual = decideL1(input);
        let expected = Ok('0'::'1'::'0'::'0'::'1'::'0'::Nil);
        Assert.eq(expected, actual)

    @Test
    def lang1_05 : Bool =
        let input = "01";
        let actual = decideL1(input);
        let expected = Err(());
        Assert.eq(expected, actual)

    /// Language 2
    /// L = L(G)
    /// G = ({0, 1}, {S}, P, S)
    /// P = {S -> eps | 1S0S | 0S1S}
    /// "words that have an equal number of 1 and zero"

    def parserL2() : PResult \ Parse = 
        choice(
            empty,
            _ -> choice(
                _ -> seq(_ -> symbol('1'), _ -> seq(parserL2, _ -> seq( _ -> symbol('0'), parserL2))),
                _ -> seq(_ -> symbol('0'), _ -> seq(parserL2, _ -> seq( _ -> symbol('1'), parserL2)))
            )
        )

    def decideL2(input : String) : PResult = invokeParser(input, parserL2, x -> x::Nil, (l,r) -> l ::: r, Nil)

    @Test
    def lang2_01 : Bool =
        let input = "000111";
        let actual = decideL2(input);
        let expected = Ok('0'::'0'::'0'::'1'::'1'::'1'::Nil);
        Assert.eq(expected, actual)

    @Test
    def lang2_02 : Bool =
        let input = "00101110";
        let actual = decideL2(input);
        let expected = Ok('0'::'0'::'1'::'0'::'1'::'1'::'1'::'0'::Nil);
        Assert.eq(expected, actual)

    @Test
    def lang2_03 : Bool =
        let input = "00011";
        let actual = decideL2(input);
        let expected = Err(());
        Assert.eq(expected, actual)        

    @Test
    def lang2_04 : Bool =
        let input = "";
        let actual = decideL2(input);
        let expected = Ok(Nil);
        Assert.eq(expected, actual)

    @Test
    def lang2_05 : Bool =
        let input = "1110101";
        let actual = decideL2(input);
        let expected = Err(());
        Assert.eq(expected, actual)   

    /// Language 3
    /// L = L(G)
    /// G = ({(, )}, {S}, P, S)
    /// P = {S -> eps | (S)S}
    /// "words are valid parenthese eypressions" -> Dyck-Language D1 
    /// 

    def parserL3() : PResult \ Parse = 
        choice(
            empty,
            _ -> seq(_ -> symbol('('), _ -> seq(parserL3, _ -> seq( _ -> symbol(')'), parserL3)))
        )

    def decideL3(input : String) : PResult = invokeParser(input, parserL3, x -> x::Nil, (l,r) -> l ::: r, Nil)

    @Test
    def lang3_01 : Bool =
        let input = "(())()";
        let actual = decideL3(input);
        let expected = Ok('('::'('::')'::')'::'('::')'::Nil);
        Assert.eq(expected, actual)
    
    @Test
    def lang3_02 : Bool =
        let input = "(())))()";
        let actual = decideL3(input);
        let expected = Err(());
        Assert.eq(expected, actual)

    @Test
    def lang3_03 : Bool =
        let input = "";
        let actual = decideL3(input);
        let expected = Ok(Nil);
        Assert.eq(expected, actual)

    @Test
    def lang3_04 : Bool =
        let input = ")(()";
        let actual = decideL3(input);
        let expected = Err(());
        Assert.eq(expected, actual)

    @Test
    def lang3_05 : Bool =
        let input = "(()())";
        let actual = decideL3(input);
        let expected = Ok('('::'('::')'::'('::')'::')'::Nil);
        Assert.eq(expected, actual)

    /// Language 4
    /// L = (a | b)* | (a | c)* | (b | c)*
    /// "words contain at most 2 distinct symbols from alphabet {a, b, c}"
    ///

    def parserL4() : PResult \ Parse = 
        choice(
            _ -> many(_ -> choice(_ -> symbol('a'), _ -> symbol('b'))),
            _ -> choice(
                _ -> many(_ -> choice(_ -> symbol('a'), _ -> symbol('c'))),
                _ -> many(_ -> choice(_ -> symbol('b'), _ -> symbol('c')))
            )
        )

    def decideL4(input : String) : PResult = invokeParser(input, parserL4, x -> x::Nil, (l,r) -> l ::: r, Nil)
   
    @Test
    def lang4_01 : Bool =
        let input = "a";
        let actual = decideL4(input);
        let expected = Ok('a'::Nil);
        Assert.eq(expected, actual)

    @Test
    def lang4_02 : Bool =
        let input = "bcbb";
        let actual = decideL4(input);
        let expected = Ok('b'::'c'::'b'::'b'::Nil);
        Assert.eq(expected, actual)

    @Test
    def lang4_03 : Bool =
        let input = "";
        let actual = decideL4(input);
        let expected = Ok(Nil);
        Assert.eq(expected, actual)

    @Test
    def lang4_04 : Bool =
        let input = "accb";
        let actual = decideL4(input);
        let expected = Err(());
        Assert.eq(expected, actual)

    @Test
    def lang4_05 : Bool =
        let input = "ccbccbbbca";
        let actual = decideL4(input);
        let expected = Err(());
        Assert.eq(expected, actual)

}