import ply.yacc as yacc

from tabulate import tabulate

# Import tokens
from lexer import tokens

# Symbol table as an array of dictionaries
symbol_table = []

def find_in_symbol_table(name, scope):
    """Search for an entry by name and scope in the symbol table."""
    return any(entry[0] == name and entry[2] == scope for entry in symbol_table)

def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
    # Check for existing entry with the same name and scope
    if find_in_symbol_table(name, scope):
        print(f"Warning: Variable '{name}' already declared in scope '{scope}'.")
    else:
        memory_address = hex(id(name))  # Generate a unique "memory address" using id
        entry = [name, var_type, scope, memory_address, value, additional_info]
        symbol_table.append(entry)

# a function to update the symbol table
def update_symbol_table(name, value):
    for entry in symbol_table:
        if entry[0] == name:  # entry[0] is the 'Name' field
            entry[4] = value  # entry[4] is the 'Value' field
            break




def p_array_declaration(t):
    'declaration : type ID LBRACKET NUMBER RBRACKET SEMICOLON'
    var_type, var_name, size = t[1], t[2], t[4]
    if size <= 0:
        print(f"Erreur : Taille invalide pour le tableau '{var_name}'.")
    else:
        value = [None] * size  # Initialiser avec des valeurs par défaut
        add_to_symbol_table(var_name, var_type, "global", value, f"Tableau de taille {size}")        


def p_statement_declaration(t):
    '''statement : type declaration_list SEMICOLON
                 | CONST type ID EQUALS expression SEMICOLON
                 | type ID LBRACKET NUMBER RBRACKET SEMICOLON'''  # <-- Ajout pour les tableaux'''
    
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
def p_array_assignment(t):
    'statement : ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON'
    var_name, index, value = t[1], t[3], t[6]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                entry[4][index] = value
                print(f"Tableau '{var_name}' mis à jour : {entry[4]}")
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
# updating variable , ID= exp(arth or logic or comparison);
def p_statement_assignment(t):
    'statement : ID EQUALS expression SEMICOLON'
    var_name = t[1]
    update_symbol_table(var_name, t[3])
# END of var declaration


def p_array_access(t):
    'factor : ID LBRACKET expression RBRACKET'
    var_name, index = t[1], t[3]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                t[0] = entry[4][index]
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                t[0] = None
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
    t[0] = None





def p_type(t):
    '''type : INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | CHAR_TYPE'''
    t[0] = t[1] 

# E → T
def p_expression_term(t):
    'expression : term'
    t[0] = t[1]

# T → F
def p_term_factor(t):
    'term : factor'
    t[0] = t[1]

# F → num
def p_factor_num(t):
    'factor : NUMBER'
    t[0] = t[1]
def p_expression_or(t):
    'expression : expression OR expression'
    t[0] = t[1] or t[3]

# Logical AND
def p_expression_and(t):
    'expression :  expression  AND expression  '
    t[0] = t[1] and t[3]

def p_expression_comparison(t):
    '''expression : expression EQ expression
                  | expression NEQ expression
                  | expression LT expression
                  | expression GT expression
                  | expression LTE expression
                  | expression GTE expression'''
    if t[2] == '==':
        t[0] = t[1] == t[3]
    elif t[2] == '!=':
        t[0] = t[1] != t[3]
    elif t[2] == '<':
        t[0] = t[1] < t[3]
    elif t[2] == '>':
        t[0] = t[1] > t[3]
    elif t[2] == '<=':
        t[0] = t[1] <= t[3]
    elif t[2] == '>=':
        t[0] = t[1] >= t[3]
# NOT operator
def p_expression_not(t):
    'expression : NOT expression'
    t[0] = not t[2]
# Grouped expression
def p_comparison_expr(t):
    'comparison : LPAREN expression RPAREN'
    t[0] = t[2]

# Base case: a number
def p_comparison_number(t):
    'comparison : NUMBER'
    t[0] = t[1]


def p_factor_comparison(t):
    'factor : comparison'
    t[0] = t[1]
def p_error(t):
    print(f"Syntax error at '{t.value}'" if t else "Syntax error at EOF")



#  when an identifier (ID) appears as a factor in an expression it'll looks it's value in symbol table
def p_factor_id(t):
    'factor : ID'
    var_name = t[1]
    for entry in symbol_table:
        if entry[0] == var_name:  # entry[0] is the 'Name' field
            if entry[4] is not None:  # entry[4] is the 'Value' field
                t[0] = entry[4]
            else:
                print(f"Error: Variable '{var_name}' not initialized.")
                t[0] = 0
            return
    print(f"Error: Variable '{var_name}' not declared.")
    t[0] = 0

parser = yacc.yacc()


# Test the parser
def parse_statement(statement):
    parser.parse(statement)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))
