from lexer import lexer  # Import the lexer from lexer.py

# Sample code to test lexer
sample_code = '''
    INTEGER a, b;       %% Simple variable declaration
    FLOAT pi = 3.14;    %% Constant
    CHAR letter = 'a';  %% Single character variable
    CONST INTEGER maxVal = 100;
    CHAR text[10];      %% Array declaration
    %% End of declarations
'''

# Feed the sample code to the lexer
lexer.input(sample_code)

# Tokenize the input and print each token
for tok in lexer:
    print(tok)
