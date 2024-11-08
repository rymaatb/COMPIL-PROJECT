import ply.lex as lex
import ply.yacc as yacc

# Reserved keywords
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR'
}

# Token list
tokens = ['NUMBER', 'ID', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQ',
          'LPAREN', 'RPAREN', 'GT', 'LT', 'GE', 'LE', 'NE', 'SEMI',
          'LBRACE', 'RBRACE', 'EQ_EQ', 'COLON'] + list(reserved.values())

# Token regex rules
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQ = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='
t_NE = r'!='
t_SEMI = r';'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_EQ_EQ = r'=='
t_COLON = r':'

# Symbol table for demonstration
symbol_table = {
    'A': 10,
    'B': 20,
    'C': 0
}

# Lexing rule for identifiers and reserved words
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

# Lexing rule for numbers
def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

# Ignore whitespace and newline
t_ignore = ' \t\n'

# Error handling for invalid characters
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing precedence rules
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE')
)

# Grammar rule for assignments
def p_statement_assign(p):
    'statement : ID EQ expression SEMI'
    symbol_table[p[1]] = p[3]
    print(f"{p[1]} assigned {p[3]}")

# Grammar rule for if-else conditions
def p_statement_if(p):
    '''statement : IF LPAREN condition RPAREN LBRACE statement RBRACE
                 | IF LPAREN condition RPAREN LBRACE statement RBRACE ELSE LBRACE statement RBRACE'''
    condition = p[3]
    if condition:
        p[0] = p[6]
    elif len(p) > 9:
        p[0] = p[10]

# Grammar rule for for loop
def p_statement_for(p):
    '''statement : FOR LPAREN statement COLON expression COLON condition RBRACE statement RBRACE'''
    init_stmt = p[3]
    step_expr = p[5]
    condition = p[7]
    
    while condition:
        p[9]
        symbol_table[init_stmt[0]] += step_expr

# Grammar rule for conditions
def p_condition(p):
    '''condition : expression GT expression
                 | expression LT expression
                 | expression GE expression
                 | expression LE expression
                 | expression EQ_EQ expression
                 | expression NE expression'''
    if p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]
    elif p[2] == '==':
        p[0] = p[1] == p[3]
    elif p[2] == '!=':
        p[0] = p[1] != p[3]

# Grammar rule for expressions
def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    elif p[2] == '*':
        p[0] = p[1] * p[3]
    elif p[2] == '/':
        p[0] = p[1] / p[3]

# Grammar rule for parentheses
def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

# Grammar rule for numbers
def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

# Grammar rule for identifiers
def p_expression_id(p):
    'expression : ID'
    p[0] = symbol_table.get(p[1], 0)

# Error handling
def p_error(p):
    print("Syntax error at '%s'" % p.value if p else "Syntax error at EOF")

# Build the parser
parser = yacc.yacc()
