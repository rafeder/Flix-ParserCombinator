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

/// Type of the semantic of the program to be parsed.
type alias Sem = List[Char]

/// Type produced by atmomic and combined parsers.
pub type alias PResult = Result[Unit, Sem]

/// Effect to handle the parsing process
eff Parse {

    /// Is triggered whenever an atomic parser is called. 
    /// A Handler should take care of adjusting the input to the 
    /// postfix left unparsed, as well as constructing semantics from
    /// the symbol that was parsed.
    def sat(p : (Char -> Bool)) : PResult

    /// Special effect for an empty parser (accepting \epsilon), similar to
    /// sat effect, but warrants a different effect, as the input is always left untouched
    /// and a parse of epsilon can never fail.
    def empty() : PResult
    
    /// Effect to handle decisions in the parsing process. Handlers of this effect can 
    /// implement different parsing-strategies like eager and echaustive.
    def flip() : Bool
}

mod Parser{
    use Char.isDigit
    use Char.isLetter

    ///
    /// Constructs an atomic parser that consumes no symbol and leaves the input unchanged.
    ///
    /// Returns a parser that decides L = {\epsilon}.
    ///
    pub def empty() : PResult \ Parse = Parse.empty()

    ///
    /// Constructs aan atomic parser that parses s as a singular symbol.
    ///
    /// Returns a parser that decides L = {s}.
    ///
    pub def symbol(x :  Char) : PResult \ Parse = Parse.sat(c -> x == c)

    ///
    /// Constructs an atomic parser that parses a letter.
    ///
    /// Returns a parser that that decides L = {A, B, ..., Z, a, b, ..., z}.
    ///
    pub def letter() : PResult \ Parse = Parse.sat(isLetter)

    ///
    /// Constructs an atomic parser that parses a digit. 
    ///
    /// Returns a parser that that decides L = {0, 1, ..., 9}.
    ///
    pub def digit() : PResult \ Parse = Parse.sat(isDigit)

    /// Constructs a parser-combinator that parses an entire string in a singel step.
    ///
    /// Returns a parser that decides L = {str}.
    ///
    pub def token(str : String) : PResult \ Parse = 
        let f = x -> x 
                |> List.map(c -> _ -> symbol(c))
                |> List.reduceLeft((a, b) -> (_ -> seq(a, b)));
        match f(String.toList(str)) {
            case Some(p) => p()
            case None    => Err()
        }

    ///
    /// Parser-Combinator that takes two parser "left" and "right" (both wrapped into lambdas to defer effects). 
    /// Constructs from them a parser that applies left and/or right according to the parsing strategy.
    ///
    /// If left decides L1 and right decides L2 then the resulting Parser decides L1 | L2.
    ///
    pub def choice(l : Unit -> PResult \ Parse , r : Unit -> PResult \ Parse) : PResult \ Parse = 
        let heads = Parse.flip(); 
        if(heads) l() else r()

    ///
    /// Parser-combinator that takes two parsers "p1" and "p2" (wrapped into lambdas to defer effects) 
    /// and builds fromt them a parser that applies them consecutively. 
    ///
    /// If p1 decides L1 and p2 decides L2 the resulting Parser decides L1L2
    ///
    pub def seq(a : Unit -> PResult \ Parse, b : Unit -> PResult \ Parse) : PResult \ Parse = 
        {discard(a()); b()}

    ///
    /// Parser-combinator that takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails.
    ///
    /// If p decides L then the resulting Parser decides L*.
    ///
    /// Note that this parser can never fail. 
    ///
    pub def many(a : Unit -> PResult \ Parse) : PResult \ Parse = 
        choice(_ -> many1(a), _ -> empty())

    ///
    /// Parser-combinator that takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails, but at least once. 
    /// 
    /// If p decides L then the resulting parser decides L+.
    ///
    pub def many1(a : Unit -> PResult \ Parse) : PResult \ Parse = 
            seq(a, _ -> many(a))

    ///
    /// This is the main function to apply a parser to an input.
    /// Additionally the parsed string will be converted into some program semantic.
    ///
    /// input : the input String to be parsed
    /// parser : parser function to be applied to the input
    /// transform: unary function to cast a single parsed char into a semantic values
    /// op : operation to combine 2 semantic values (that were parsed in sequence) into one
    /// e : neutral element of the semantic values such that x `op` e = x
    ///
    /// Will return the semantics of the input according to the parser, or
    /// an error if the input is not in the language accepted by the parser
    ///
    pub def invokeParser(input : String, parser : Unit -> PResult \ Parse, transform : Char -> Sem, op : (Sem, Sem) -> Sem, e : Sem): PResult =   
        
        /// acts as the zero element for our fold over the atomic semantic elements 
        def zero(rest : List[Char]) : PResult = if(rest == Nil) Ok(e) else Err();
        
        /// acts as the fold funciton over the atomice semantic elements
        def <<(a : PResult, b : PResult) : PResult = 
            match (a, b) {
                case (Ok(l), Ok(r)) => Ok(l `op` r)
                case _              => Err()
            };

        region exec {
            // src reference will hold the unparsed rest of the input string
            let src = Ref.fresh(exec, String.toList(input));
            run{
                    parser()
                }with handler Parse {

                    /// this effect applies an atomic parser to the referenz holding the unconsumed input
                    def sat(p, k) = 
                        match Ref.get(src) {
                            case Nil     => Err() // parse in empty rest
                            case x::xs   => if(p(x)) 
                                                // sucessful parse
                                                // we combine the semantics of the continuation with the 
                                                // semantics of the symbol that was just parsed.
                                                // This creates a pattern similar to a folgright
                                                {Ref.put(xs, src); (Ok(transform(x)) << k(zero(Ref.get(src))))} 
                                            else 
                                                Err() // parser could not accept next symbol
                        }
                    
                    def empty(k) = (Ok(e) << k(zero(Ref.get(src))))

                    /// This represents an eager parsing strategy were at any decision point we 
                    /// arbitrarily choose one option and only evaluate the second option if the 
                    /// first one fails.
                    def flip(k) = 
                        let stash = Ref.get(src);
                        let left = k(true);
                        if(left != Err()) left else {Ref.put(stash, src); k(false)}
                }
        }
}
