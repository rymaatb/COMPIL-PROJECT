import sys
from pathlib import Path

sys.path.append(str(Path(__file__).resolve().parent / "src"))

from src.main import test_lexer


sample_code = '''
    INTEGER a, b;       %% Simple variable declaration
    FLOAT pi = 3.14;    %% Constant
    CHAR letter = 'a';  %% Single character variable
    
    %% End of declarations
'''
# Exemple de code source pour tester le lexer
input_text = '''
VAR_GLOBAL A;
DECLARATION INTEGER x = 5;
INSTRUCTION x = 10 + 20;

'''
test_lexer(sample_code)
print("-----------------------------")
# Appel à la fonction de test du lexer
test_lexer(input_text)
