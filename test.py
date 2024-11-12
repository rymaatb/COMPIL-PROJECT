# test_lexer.py
from lexer import test_lexer

# Exemple de code source pour tester le lexer
input_text = '''
VAR_GLOBAL A;
DECLARATION INTEGER x = 5;
INSTRUCTION x = 10 + 20;
IF (x > 10) { x = 30; }
ELSE { x = 0; }
FOR (i = 0; i < 10; i++) { x = x + 1; }
'''

# Appel à la fonction de test du lexer
test_lexer(input_text)
