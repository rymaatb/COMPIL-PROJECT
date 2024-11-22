import ply.yacc as yacc
from symbol_table import add_to_symbol_table, update_symbol_table, display_symbol_table

# Import tokens
from lexer import tokens

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
            add_to_symbol_table(var_name, var_type, scope, value)

def p_declaration_list(t):
    '''declaration_list : declaration
                        | declaration COMMA declaration_list'''
    if len(t) == 2:
        t[0] = [t[1]]
    else:
        t[0] = [t[1]] + t[3]

def p_declaration(t):
    '''declaration : ID
                   | ID EQUALS expression
                   | ID LBRACKET NUMBER RBRACKET'''
    if len(t) == 2:
        t[0] = (t[1], None)
    elif len(t) == 4:
        t[0] = (t[1], t[3])
    else:
        size = t[3]
        t[0] = (t[1], [None] * size)

def p_expression(t):
    '''expression : ID
                  | FLOAT
                  | INT
                  | CHAR'''
    t[0] = t[1]

def p_type(t):
    '''type : INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | CHAR_TYPE'''
    t[0] = t[1]

def p_error(t):
    print(f"Syntax error at '{t.value}'" if t else "Syntax error at EOF")

parser = yacc.yacc()
