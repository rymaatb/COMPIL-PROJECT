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
    code = '''
    %% Variable declarations
    int a;
    float b = 4.2;
    char c = 'X';
    int arr[2];
    const int MAX = 100;

    %% Assignments and expressions
    a = 10;
    b = a + 3.5;
    

    %% Read and write operations
    READ(a);
    WRITE(a, b, c);

    %% Conditional (if-else)
    if (b > 10) {
        WRITE(1);
    } else {
        WRITE(0);
    }

    %% Errors to test handling
             %% Out of bounds
    WRITE(nonDeclaredVar);     %% Writing undeclared variable
    const int PI = 3.14;       %% Redeclaring a constant
   
'''

    test_lexer(code)
    test_parser(code)
