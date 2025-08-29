use Parser.{invokeParser, many, choice, symbol, seq, token, letter, digit, empty}
def main() : Result[Unit, List[Char]] \ IO = 
    let input = "a";
    //let p2 = _ -> seq(_ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1'))), _ -> seq(_ -> symbol('a'), _ -> many(_ -> choice(_ -> symbol('0'), _ -> symbol('1')))));
    let p3 = _ -> choice(empty, letter);
    invokeParser(input, p3)