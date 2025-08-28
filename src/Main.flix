use Parser.{invokeParser, many, choice, symbol, seq, token, letter}
def main() : Bool = 
    let input = "010111a";
    let p2 = _ -> seq(_ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1'))), _ -> seq(_ -> symbol('a'), _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1')))));
    //let p3 = _ -> many(letter);
    invokeParser(input, p2)