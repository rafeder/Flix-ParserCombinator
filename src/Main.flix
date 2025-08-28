use Parser.{parse, empty, symbol, digit, letter,
           token, choice, many, many1, seq}
def _parseL1() : PResult \ Parsing = 
        seq(
            _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1'))),
            _ -> seq(
                _ -> token("11"),
                _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1')))
            )
        )
def main() : PResult \ IO = 
    let input  = "aaa";
    //parse(input, (_ -> choice(_ -> symbol('b'), _ -> symbol('a'))))
    parse(input, _ -> seq(_ -> many(_ -> choice(letter, letter)), letter))