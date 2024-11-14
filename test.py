# test_lexer.py
from lexer import test_lexer


sample_code = '''
    INTEGER a, b;       %% Simple variable declaration
    FLOAT pi = 3.14;    %% Constant
    CHAR letter = 'a';  %% Single character variable
    CONST INTEGER maxVal = 100;
    CHAR text[10];      %% Array declaration
    %% End of declarations
'''
# Exemple de code source pour tester le lexer
input_text = '''
VAR_GLOBAL A;
DECLARATION INTEGER x = 5;
INSTRUCTION x = 10 + 20;
IF (x > 10) { x = 30; }
ELSE { x = 0; }
FOR (i = 0; i < 10; i++) { x = x + 1; }
'''
test_lexer(sample_code)
print("-----------------------------")
# Appel à la fonction de test du lexer
test_lexer(input_text)
