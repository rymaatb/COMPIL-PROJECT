def p_declaration(t):
    '''declaration :  ID
                   | ID EQUALS expression
                   | ID LBRACKET NUMBER RBRACKET'''
    if len(t) == 2:
        t[0] = (t[1], None)  # Variable without initialization
    else:
        t[0] = (t[1], t[3])  # Variable with initialization and array declaration