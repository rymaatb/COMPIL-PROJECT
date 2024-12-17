import ply.lex as lex
import ply.yacc as yacc

from tabulate import tabulate


# List of tokens
tokens = (
    'CONST',
    'ID', 
    'LBRACKET', 'RBRACKET',
    'SEMICOLON', 'COMMA',
    'NUMBER',  'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',
    'EQUALS', 'INTEGER', 'FLOAT', 'CHAR', 'INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE', 'CHAR_TYPE', 
    
    
)

# Reserved keywords
reserved = {
    'IF': 'IF',
    'ELSE': 'ELSE',
    'FOR': 'FOR',
    'INTEGER': 'INT_TYPE',
    'FLOAT': 'FLOAT_TYPE',
    
    'CHAR': 'CHAR_TYPE',
    
    'CONST': 'CONST',
    'READ': 'READ',
    'WRITE': 'WRITE',
}

# Regular expressions


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
    r'[A-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    
    return t

def t_INTEGER(t):
    r'-?\d+'  # Cela permet de capturer les entiers avec un signe négatif optionnel
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

start = 'statements'

quadruples_list = []  # Liste globale pour stocker les quadruplets

# Symbol table as an array of dictionaries
symbol_table = []

def find_in_symbol_table(name, scope):
    """Search for an entry by name and scope in the symbol table."""
    return any(entry[0] == name and entry[2] == scope for entry in symbol_table)

def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
   # Vérifiez que l'ajout du tableau se fait bien
    print(f"Ajout à la table des symboles: {name}, {var_type}, {scope}, {value}, {additional_info}")
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




def p_statements(t):
    '''statements : statement
                  | statement statements '''
    if len(t) == 2 :
        t[0] = [t[1]]
    else:
        t[0] = [t[1]] + t[2]
def p_statement(t):
    '''statement : simple_assignment
                 | array_declaration
                  | declaration_assignment 
                 | array_assignment
                 | type declaration_list SEMICOLON
                 | const_declaration'''





def p_const_declaration(t):
    '''const_declaration : CONST type ID EQUALS expression SEMICOLON'''
    var_type, var_name, value = t[2], t[3], t[5]
    print(f"Déclaration constante : {var_name} = {value} (type : {var_type})")
    add_to_symbol_table(var_name, var_type, "global", value, "constant")
  

def p_array_declaration(t):
    'array_declaration : type ID LBRACKET INTEGER RBRACKET SEMICOLON'
    var_type, var_name, size = t[1], t[2], t[4]
    if size <= 0:
        print(f"Erreur : Taille invalide pour le tableau '{var_name}'.")
    else:
        value = [None] * size
        add_to_symbol_table(var_name, var_type, "global", value, f"Tableau de taille {size}")
           # Génération du quadruplet ADEC
    quadruplet = ("ADEC", var_name, size, None)  # Aucun résultat spécifique dans la 4e colonne
    quadruples_list.append(quadruplet)
     
    # Affichage du quadruplet généré
    print(f"Quadruplet pour le tableau '{var_name}':")
    print(quadruplet)





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
                   | ID LBRACKET INTEGER RBRACKET'''
    if len(t) == 2:
        t[0] = (t[1], None)
    elif len(t) == 4:
        t[0] = (t[1], t[3])    
    else:
        size = t[3]
        if size <= 0:
            print(f"Erreur : Taille invalide pour '{t[1]}'.")
            t[0] = (t[1], None)
        else:
            t[0] = (t[1], [None] * size)  # Tableau





def p_expression(t):
    '''expression : ID
                  | FLOAT
                  | INTEGER
                  | CHAR'''
    
    print(f"Expression analysée : {t[1]}")
    t[0] = t[1]




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
    'factor : INTEGER'
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
    'comparison : INTEGER'
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

def p_array_access(t):
    'factor : ID LBRACKET expression RBRACKET'
    var_name, index = t[1], t[3]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie si c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                if entry[4][index] is None:
                    print(f"Avertissement : Accès à une valeur non initialisée dans '{var_name}[{index}]'.")
                t[0] = ('ARRAY_ACCESS', var_name, index)  # Retourne les informations nécessaires pour l'affectation
                print(f"Accès tableau détecté : {t[0]}")
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                t[0] = None
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
    t[0] = None


def p_variable_assignment(t):
    '''simple_assignment : ID EQUALS expression SEMICOLON'''
    var_name = t[1]
    expr_value = t[3]  # Récupération de la partie droite de l'affectation

    # Vérifie si expr_value est une case de tableau
    if isinstance(expr_value, tuple) and len(expr_value) == 3 and expr_value[0] == 'ARRAY_ACCESS':
        array_name, index = expr_value[1], expr_value[2]
        expr_value = f"{array_name}[{index}]"  # Formater comme accès tableau
        print(f"Accès tableau détecté : {expr_value}")
    
    # Génération du quadruplet
    quadruple = ('=', expr_value, 'NONE', var_name)
    quadruples_list.append(quadruple)
    print(f"Quadruplet généré : {quadruple}")

    # Mise à jour de la table des symboles
    for entry in symbol_table:
        if entry[0] == var_name:
            entry[4] = expr_value
            print(f"Variable '{var_name}' mise à jour avec : {expr_value}")
            return
    print(f"Erreur : Variable '{var_name}' non déclarée.")



def p_array_assignment(t):
    'array_assignment : ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON'
    var_name, index, value = t[1], t[3], t[6]

    # Rechercher le tableau dans la table des symboles
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                # Vérifie la compatibilité des types avant de procéder
                expected_type = entry[1]  # Type attendu du tableau
                if expected_type == 'INTEGER' and not isinstance(value, int):
                    print(f"Erreur : Type incorrect pour '{var_name}[{index}]'. Attendu : INTEGER, reçu : {type(value).__name__}.")
                    return
                elif expected_type == 'FLOAT' and not isinstance(value, float):
                    print(f"Erreur : Type incorrect pour '{var_name}[{index}]'. Attendu : FLOAT, reçu : {type(value).__name__}.")
                    return
                elif expected_type == 'CHAR' and not isinstance(value, str):
                    print(f"Erreur : Type incorrect pour '{var_name}[{index}]'. Attendu : CHAR, reçu : {type(value).__name__}.")
                    return

                # Mise à jour de la valeur dans le tableau
                entry[4][index] = value

                # Générer un quadruplet pour l'affectation
                quadruple = ('=', value, 'NONE', f'{var_name}[{index}]')
                quadruples_list.append(quadruple)
                print(f"Quadruplet généré : {quadruple}")
                print(f"Tableau '{var_name}' mis à jour : {entry[4]}")
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                return

    print(f"Erreur : Tableau '{var_name}' non déclaré.")

def p_declaration_assignment(t):
    '''declaration_assignment : type ID EQUALS expression SEMICOLON'''
    var_type, var_name, value = t[1], t[2], t[4]

    # Vérifiez si la valeur est un accès à un tableau
    if isinstance(value, tuple) and value[0] == 'ARRAY_ACCESS':  # Cas d'accès tableau
        array_name, index = value[1], value[2]
        value = f"{array_name}[{index}]"
        print(f"Accès tableau détecté : {value}")

    # Ajout à la table des symboles
    add_to_symbol_table(var_name, var_type, "global", value, "Déclaration et affectation")
    print(f"Variable déclarée : {var_name} avec valeur : {value}")

    # Génération du quadruplet
    quadruple = ('=', value, 'NONE', var_name)
    quadruples_list.append(quadruple)
    print(f"Quadruplet généré : {quadruple}")



parser = yacc.yacc()






# Test the parser
def parse_statement(statement):
    parser.parse(statement)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))


expressions = [
 "INTEGER Matrix[3];",
 "Matrix[2] = 2;",
 " INTEGER X = Matrix[2];"
 
 ]
for stmt in expressions:
    print(f"Parsing statement: {stmt}")
    parse_statement(stmt)
    print("-" * 40)


# Affichage des quadruplets
print("\nQuadruplets générés :")
for quad in quadruples_list:
    print(quad)
