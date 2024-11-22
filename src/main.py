from lexer import lexer
from parser import parser
from symbol_table import display_symbol_table

def test_lexer(input_text):
    print("Lexical Analysis:")
    lexer.input(input_text)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)

def test_parser(input_text):
    print("\nParsing:")
    parser.parse(input_text)
    print("\nSymbol Table:")
    display_symbol_table()

if __name__ == '__main__':
    code = """
    int a;
    char b[5];
    float c = 3.14;
    """
    test_lexer(code)
    test_parser(code)
