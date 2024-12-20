def p_statementIf(t):
    '''statement : IFTHEN 
                 | IFTHENELSE'''
    if len(t) == 2:  # If there is no else block
        t[0] = ('statement', t[1])
    elif len(t) == 3:  # If there is an else block
        t[0] = ('statement', t[1])

def p_IFTHENELSE(t):
   # '''IFTHENELSE : IF LPAREN condition RIF1 RPAREN block RIF2 ELSE block RIF3'''
    '''IFTHENELSE : IFTHEN ELSE block'''
    if len(t) == 4:  # If there is an else block
        t[0] = ('IFTHENELSE', t[1], t[3])
        RIF3(t)

def p_IFTHEN(t):# If there is no else block
    #IFTHEN --> IF LPAREN condition RIF1 RPAREN block RIF2
    '''IFTHEN : conditionIF RPAREN block'''
    if len(t) == 4:
        t[0] = ('IFTHEN', t[1], t[3])
        RIF2(t)

def p_conditionIF(t):
    #conditionIf --> IF LPAREN condition RIF1
    '''conditionIF : IF LPAREN condition'''
    if len(t) == 4:
        t[0] = t[3]
        RIF1(t)

    '''statement : IF LPAREN condition RPAREN block
                   | IF LPAREN condition RPAREN block ELSE block'''
    if len(t) == 6:  # If there is no else block
        t[0] = ('statement', t[3], t[5])
    elif len(t) == 8:  # If there is an else block
        t[0] = ('statement', t[3], t[5], t[7])

def p_condition(t):
    '''condition : ID EQUALS ID
                 | ID LT ID
                 | ID GT ID
                 | ID EQUALS factor
                 | ID LT factor
                 | ID GT factor'''
    t[0] = ('condition', t[2], t[1], t[3])  # ('operator', left, right)
#***********Routine ofIF ******************************
def RIF1(t):
    global quadruples ,sauv_bz
    conditionTemp = new_temp()
    else_etiq = new_label('else')
    quadruples.append(('BZ',else_etiq,conditionTemp,None))
    sauv_bz = get_counter()
    increment_quad_counter(1)
def RIF2(t):
    global quadruples
    fin = new_label('fin')
    quadruples.append(('BR',fin,None,None))
    sauv_br = get_counter()
    increment_quad_counter(1)
    #quadruples[sauv_bz,2] = get_counter()
def RIF3(t):
    global quadruples , sauv_br
    #quadruples[sauv_br,2] = get_counter()