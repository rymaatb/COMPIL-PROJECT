import ply.lex as lex

# Liste des tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION', 'INSTRUCTION', #mots cles 
    'ID', 'COMMENT',
    'LBRACE', 'RBRACE', 'SEMICOLON', 'COMMA'    # Symboles
)

# Expressions régulières pour les mots-clés
t_VAR_GLOBAL = r'VAR_GLOBAL'
t_DECLARATION = r'DECLARATION'
t_INSTRUCTION = r'INSTRUCTION'

# Expressions régulières pour les symboles
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','

# Expression régulière pour les commentaires
def t_COMMENT(t):
    r'%%.*'
    pass  # Ignorer les commentaires

# Expression régulière pour les identifiants
def t_ID(t):
    r'[A-Z_][a-zA-Z_0-9]*'
     # Vérifier si l'identifiant est un mot-clé
    if t.value == 'VAR_GLOBAL':
        t.type = 'VAR_GLOBAL'
    elif t.value == 'DECLARATION':
        t.type = 'DECLARATION'
    elif t.value == 'INSTRUCTION':
        t.type = 'INSTRUCTION'
    return t

# Gestion des retours à la ligne
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# Ignorer les espaces et tabulations
t_ignore = ' \t'

# Gestion des erreurs
def t_error(t):
    print(f"Caractère illégal '{t.value[0]}' à la ligne {t.lineno}")
    t.lexer.skip(1)


# Construire l'analyseur lexical
lexer = lex.lex()

# Fonction pour tester l'analyseur lexical
def analyser_lexical(input_text):
    lexer.input(input_text)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
