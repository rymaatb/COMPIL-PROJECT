<<<<<<< HEAD
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
=======
from lexer import lexer

# Exemples d'instructions à analyser avec le lexer
test_code = [
    "A = (X + 7 + B) / (5.3 - (-2)) ;",
    "B = 0;",
    "IF (A > B) { C = A + 2.6; } ELSE { C = 0; }",
    "FOR (i = 0 : 2 : n) { i = i + 1; }"
]

# Fonction pour analyser et afficher les tokens générés par le lexer
def test_lexer(code_list):
    for code in code_list:
        print(f"\nAnalyzing: {code}")
        lexer.input(code)  # Fournit l'instruction au lexer
        while True:
            token = lexer.token()  # Récupère le token suivant
            if not token:
                break
            print(token)  # Affiche le token trouvé

# Exécution des tests
test_lexer(test_code)
>>>>>>> fdd88be0598cc567b816804bd977685896b22d54
