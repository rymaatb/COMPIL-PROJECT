import ply.lex as lex

# List of tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION', 'CONST', 'INSTRUCTION',
    'ID', 'COMMENT',
    'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET',
    'SEMICOLON', 'COMMA',
    'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',
    'EQUALS', 'INT', 'FLOAT', 'CHAR', 'BOOL', 'INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE', 'CHAR_TYPE', 'COLON',
    'IF', 'ELSE', 'FOR',
    'READ', 'WRITE',
)

# Reserved keywords
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR',
    'int': 'INT_TYPE',
    'float': 'FLOAT_TYPE',
    'bool': 'BOOL_TYPE',
    'char': 'CHAR_TYPE',
    'true': 'BOOL',
    'false': 'BOOL',
    'const': 'CONST',
    'READ': 'READ',
    'WRITE': 'WRITE',
}

# Regular expressions
t_VAR_GLOBAL = r'VAR_GLOBAL'
t_DECLARATION = r'DECLARATION'
t_INSTRUCTION = r'INSTRUCTION'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
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
    r'[A-Za-z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_CHAR(t):
    r"'.'"
    t.value = t.value[1]
    return t

# Ignore comments
def t_COMMENT(t):
    r'%%.*'
    pass

# Ignore spaces and tabs
t_ignore = ' \t'

# Handle errors
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()
