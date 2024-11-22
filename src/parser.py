import ply.yacc as yacc
from lexer import tokens  # Import tokens from the lexer module
from symbol_table import add_to_symbol_table, find_in_symbol_table, display_symbol_table

# Grammar rules for syntactic analysis

# Variable and array declaration
def p_statement_declaration(t):
    '''statement : type declaration_list SEMICOLON
                 | CONST type ID EQUALS expression SEMICOLON'''
    scope = "global"
    if t[1] == 'const':
        var_type, var_name, value = t[2], t[3], t[5]
        add_to_symbol_table(var_name, var_type, scope, value, "constant")
    else:
        var_type = t[1]
        for var_name, value in t[2]:
            add_to_symbol_table(var_name, var_type, scope, value, "variable")

def p_declaration_list(t):
    '''declaration_list : declaration
                        | declaration COMMA declaration_list'''
    if len(t) == 2:
        t[0] = [t[1]]
    else:
        t[0] = [t[1]] + t[3]

def p_declaration_array(t):
    '''declaration : ID LBRACKET INT RBRACKET'''
    size = t[3]
    t[0] = (t[1], [None] * size)

def p_declaration(t):
    '''declaration : ID
                   | ID EQUALS expression'''
    if len(t) == 2:
        t[0] = (t[1], None)
    elif len(t) == 4:
        t[0] = (t[1], t[3])

def p_expression(t):
    '''expression : ID
                  | FLOAT
                  | INT
                  | CHAR'''
    # Check if the identifier exists in the symbol table
    if isinstance(t[1], str):
        var_name = t[1]
        if not find_in_symbol_table(var_name, "global"):
            print(f"Error: Variable '{var_name}' not declared.")
            t[0] = None
    else:
        t[0] = t[1]

def p_type(t):
    '''type : INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | CHAR_TYPE'''
    t[0] = t[1]

def p_read_statement(t):
    '''statement : READ LPAREN ID RPAREN SEMICOLON'''
    var_name = t[3]
    if not find_in_symbol_table(var_name, "global"):
        print(f"Error: Variable '{var_name}' not declared.")

def p_write_statement(t):
    '''statement : WRITE LPAREN write_content RPAREN SEMICOLON'''
    pass  # Logic is part of semantic actions

def p_write_content_single(t):
    '''write_content : write_item'''
    t[0] = [t[1]]

def p_write_content_multiple(t):
    '''write_content : write_content COMMA write_item'''
    t[0] = t[1] + [t[3]]

def p_write_item(t):
    '''write_item : ID
                  | ID LBRACKET NUMBER RBRACKET'''
    pass  # Logic is part of semantic actions

# Error handling
def p_error(t):
    if t:
        print(f"Syntax error at token '{t.value}' (line {t.lineno})")
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()
