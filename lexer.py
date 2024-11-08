import ply.lex as lex

# Define token names
tokens = [
    'ID', 'TYPE', 'NUMBER', 'FLOAT_CONST', 'CHAR_CONST', 
    'EQUALS', 'SEMICOLON', 'COMMA', 'LBRACKET', 'RBRACKET', 
    'CONST', 'COMMENT'
]

# Keywords dictionary
keywords = {
    'INTEGER': 'TYPE',
    'FLOAT': 'TYPE',
    'CHAR': 'TYPE',
    'CONST': 'CONST'
}

# Regular expression patterns for simple tokens
t_EQUALS = r'='
t_SEMICOLON = r';'
t_COMMA = r','
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_ignore = ' \t'  # Ignore spaces and tabs

# Handle newline and keep track of line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Regular expressions for tokens

# Floating-point constant (place before NUMBER to avoid conflict)
def t_FLOAT_CONST(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# Identifier (variable names)
def t_ID(t):
    r'[A-Za-z][a-zA-Z0-9]{0,7}'
    t.type = keywords.get(t.value, 'ID')  # Check if the identifier is a keyword
    return t

# Integer number (handles signed integers in parentheses)
def t_NUMBER(t):
    r'(\(\+?\d+\)|\(-\d+\)|\d+)'
    t.value = int(t.value.replace('(', '').replace(')', ''))
    return t

# Single character constant
def t_CHAR_CONST(t):
    r'\'[a-zA-Z0-9]\''
    t.value = t.value[1]  # Extracts the character without quotes
    return t

# Comment
def t_COMMENT(t):
    r'%%[^\n]*'
    pass  # Ignore comments

# Error handling
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

