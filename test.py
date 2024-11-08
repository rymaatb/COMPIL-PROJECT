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
