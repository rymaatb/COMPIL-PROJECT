import ply.lex as lex

# Token definitions
tokens = [
    'IDF',
    'NUMBER',
    'PLUS',
    'MINUS',
    'MULTIPLY',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'EQ',
    'NEQ',
    'LT',
    'LTE',
    'GT',
    'GTE',
    'AND',
    'OR',
    'NOT'
]

# Arithmetic , Comparison  and logical operators rules
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQ = r'=='
t_NEQ = r'!='
t_LT = r'<'
t_LTE = r'<='
t_GT = r'>'
t_GTE = r'>='
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'

# IDf rule 
def t_IDF(t):
    r'\b[A-Z][a-z0-9]{0,7}\b'
    return t

# Rule for numbers (temporary till the merge of code)
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Ignored characters (spaces and tabs)
t_ignore = ' \t'

# Error handling rule
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Test 
expression_test = "(3 + Xvar1)  * Yvar123 - 10 / Zvar2 && Xvar3 == 1 || !(Yvar4 > 2)"
lexer.input(expression_test)

# Tokenize 
print(f"Expression: {expression_test}")
for tok in lexer:
    print(tok)
