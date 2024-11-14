import ply.lex as lex

# Liste des tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION', 'INSTRUCTION',  # mots-clés
    'ID', 'COMMENT','TYPE', 'CONST',
    'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET',  # LBRACE and RBRACE for curly braces, LBRACKET and RBRACKET for square brackets
    'SEMICOLON', 'COMMA',    # Symboles
    'NUMBER' , 'FLOAT_CONST', 'CHAR_CONST', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',  # Opérateurs
    'EQ_EQ', 'COLON',  # Autres opérateurs et symboles du premier code
    'IF', 'ELSE', 'FOR',  # Mots-clés du premier code
)

# Expressions régulières pour les mots-clés
t_VAR_GLOBAL = r'VAR_GLOBAL'
t_DECLARATION = r'DECLARATION'
t_INSTRUCTION = r'INSTRUCTION'

keywords = {
    'INTEGER': 'TYPE',
    'FLOAT': 'TYPE',
    'CHAR': 'TYPE',
    'CONST': 'CONST'
}

# Expressions régulières pour les symboles
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACKET = r'\['   
t_RBRACKET = r'\]'   
t_SEMICOLON = r';'
t_COMMA = r','

# Expressions régulières pour les opérateurs
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQ = r'='
t_EQ_EQ = r'=='
t_NEQ = r'!='
t_LT = r'<'
t_LTE = r'<='
t_GT = r'>'
t_GTE = r'>='
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'

# Mots-clés réservés
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR'
}

# Expression régulière pour les identifiants (permet d'inclure l'underscore)
def t_ID(t):
    r'[A-Za-z_][a-zA-Z0-9_]{0,7}'  # Permet les lettres, chiffres et underscores
    t.type = reserved.get(t.value, 'ID')  # Vérifie si l'identifiant est un mot-clé
    return t

# Floating-point constant (place before NUMBER to avoid conflict)
def t_FLOAT_CONST(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# Constantes numériques
def t_NUMBER(t):
    r'\d+'  # Représente des nombres entiers
    t.value = int(t.value)  # Pour simplification, on ne prend que des entiers
    return t

def t_CHAR_CONST(t):
    r'\'[a-zA-Z0-9]\''
    t.value = t.value[1]  # Extracts the character without quotes
    return t

# Gestion des commentaires
def t_COMMENT(t):
    r'%%.*'
    pass  # Ignorer les commentaires

# Retour à la ligne
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Ignorer les espaces et tabulations
t_ignore = ' \t'

# Gestion des erreurs
def t_error(t):
    print(f"Caractère illégal '{t.value[0]}' à la ligne {t.lineno}")
    t.lexer.skip(1)

# Construire le lexer
lexer = lex.lex()

# Fonction pour tester l'analyseur lexical
def test_lexer(input_text):
    lexer.input(input_text)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
