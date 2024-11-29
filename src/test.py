from lexer import lexer
from parser import parse_statement, display_symbol_table

def pipeline_test(input_code):
    # Étape 1: Analyse lexicale
    print("Phase 1: Analyse Lexicale")
    lexer.input(input_code)
    tokens = []
    while True:
        tok = lexer.token()
        if not tok:
            break
        tokens.append((tok.type, tok.value))
    print(f"Tokens générés: {tokens}")
    
    # Étape 2: Analyse syntaxique
    print("Phase 2: Analyse Syntaxique")
    try:
        parse_statement(input_code)
    except Exception as e:
        print(f"Erreur syntaxique: {e}")
        return  # Arrête le pipeline si erreur syntaxique
    
    # Étape 3: Validation dans la table des symboles
    print("Phase 3: Table des Symboles")
    display_symbol_table()
    print("-" * 40)











