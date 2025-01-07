import ply.lex as lex
import ply.yacc as yacc
import sys
from tabulate import tabulate


# List of tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION' ,  'CONST','INSTRUCTION',  # mots-clés
    'ID', 'COMMENT','TRUE','FALSE','STRING',
    'LBRACKET', 'RBRACKET',
    'SEMICOLON', 'COMMA','LBRACE', 'RBRACE',
    'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',
    'EQUALS', 'INTEGER','BOOL', 'FLOAT', 'CHAR', 'INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE', 'CHAR_TYPE',  'COLON',  
    'IF', 'ELSE', 'FOR',  # Mots-clés du premier code
    'READ', 'WRITE' # entrees et sorties

    
)


# Reserved keywords
reserved = {
    'IF': 'IF',
    'ELSE': 'ELSE',
    'FOR': 'FOR',
    'INTEGER': 'INT_TYPE',
    'FLOAT': 'FLOAT_TYPE',
    'BOOL': 'BOOL_TYPE',
    'CHAR': 'CHAR_TYPE',
    'TRUE': 'BOOL',
    'FALSE': 'BOOL',
    'CONST': 'CONST',
    'READ': 'READ',
    'WRITE': 'WRITE',
    'VAR_GLOBAL':'VAR_GLOBAL',
    'DECLARATION':'DECLARATION',
    'INSTRUCTION':'INSTRUCTION'
}

# Regular expressions
# Expressions régulières pour les mots-clés
#t_VAR_GLOBAL = r'VAR_GLOBAL'
#t_DECLARATION = r'DECLARATION'
#t_INSTRUCTION = r'INSTRUCTION'

t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_COLON = r':'
t_SEMICOLON = r';'
t_COMMA = r','
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUALS = r'='
t_EQ = r'=='
t_NEQ = r'!='
t_LT = r'<'
t_LTE = r'<='
t_GT = r'>'
t_GTE = r'>='
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'

# Identifier and constants
def t_ID(t):
    r'[A-Z][a-zA-Z0-9_]*'  # Permet les lettres, chiffres et underscores
    if t.value in reserved:
        t.type = reserved.get(t.value, 'ID')  # Vérifie si l'identifiant est un mot-clé
        return t
    if len(t.value) > 8:
        print(f"Erreur : l'identifiant '{t.value}' dépasse 8 caractères.")
        sys.exit(1)
        t.lexer.skip(1)
    if t.value == 'TRUE':
        t.value = True
    elif t.value == 'FALSE':
        t.value = False
    return t


def t_FLOAT(t):
    r'[+-]?\d+\.\d+'
    t.value = float(t.value)
    
    return t

def t_INTEGER(t):
    r'-?\d+'  # Cela permet de capturer les entiers avec un signe négatif optionnel
    t.value = int(t.value)
    return t


def t_CHAR(t):
    r"\'(.)\'" 
    t.value = t.value[1]
    return t

def t_STRING(t):
    r'".*?"'  # Matches anything between double quotes
    t.value = list(t.value[1:-1])  # Convert the string (without quotes) to a list of characters
    return t

# Ignore comments
def t_COMMENT(t):
    r'%%.*'
    pass


t_ignore = ' \t'
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
# Handle errors
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    sys.exit(1)
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()