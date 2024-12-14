import ply.lex as lex
import ply.yacc as yacc
# to print a table 
from tabulate import tabulate


# Liste des tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION' ,  'CONST','INSTRUCTION',  # mots-clés
    'ID', 'COMMENT',
    'LBRACE', 'RBRACE', 'SEMICOLON', 'COMMA',    # Symboles
    'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',  # Opérateurs
    'EQUALS','INT', 'FLOAT', 'CHAR', 'BOOL','INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE','CHAR_TYPE',  'COLON',  # Autres opérateurs et type du symbol
    'IF', 'ELSE', 'FOR',  # Mots-clés du premier code
)

# Expressions régulières pour les mots-clés
t_VAR_GLOBAL = r'VAR_GLOBAL'
t_DECLARATION = r'DECLARATION'
t_INSTRUCTION = r'INSTRUCTION'

# Expressions régulières pour les symboles
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','

# Expressions régulières pour les opérateurs
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

# Mots-clés réservés
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR',
    'int': 'INT_TYPE',
    'float': 'FLOAT_TYPE',
    'bool': 'BOOL_TYPE',
    'char': 'CHAR_TYPE',
    'true': 'BOOL',
    'const': 'CONST',
    'false': 'BOOL'
}

# Expression régulière pour les identifiants (permet d'inclure l'underscore)
def t_ID(t):
    r'[A-Za-z_][a-zA-Z0-9_]*'  # Permet les lettres, chiffres et underscores
    t.type = reserved.get(t.value, 'ID')  # Vérifie si l'identifiant est un mot-clé
    if t.value == 'true':
        t.value = True
    elif t.value == 'false':
        t.value = False
    return t

# Constantes numériques
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# Integer literals
def t_INT(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

# Character literals
def t_CHAR(t):
    r"'.'"
    t.value = t.value[1]  # Extract the character between single quotes
    return t
# Gestion des commentaires
def t_COMMENT(t):
    r'%%.*'
    pass  # Ignorer les commentaires


# Retour à la ligne
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Ignorer les espaces et tabulations
t_ignore = ' \t'

# Gestion des erreurs
def t_error(t):
    print(f"Caractère illégal '{t.value[0]}' à la ligne {t.lineno}")
    t.lexer.skip(1)

# Construire le lexer
lexer = lex.lex()

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

# START of var declartion
# the expression will be TYPE multiple var; OR const id = value;
def p_statement_declaration(t):
    '''statement : type declaration_list SEMICOLON
                 | CONST type ID EQUALS expression SEMICOLON'''
    scope = "global"  # Set scope; change as needed for local scope support
    
    if t[1] == 'const':
        # Constant declaration
        var_type = t[2]
        var_name = t[3]
        value = t[5]
        add_to_symbol_table(var_name, var_type, scope, value, "constant")
    else:
        # Multiple variable declarations
        var_type = t[1]
        for var_name, value in t[2]:
            add_to_symbol_table(var_name, var_type, scope, value)

# int a, b = value, c;
def p_declaration_list(t):
    '''declaration_list : declaration
                        | declaration COMMA declaration_list'''
    if len(t) == 2:
        # Single variable
        t[0] = [t[1]]
    else:
        # Multiple variables
        t[0] = [t[1]] + t[3]

#  declare var type id=value ; or  type id = expr;
def p_declaration(t):
    '''declaration : ID
                   | ID EQUALS expression'''
    if len(t) == 2:
        t[0] = (t[1], None)  # Variable without initialization
    else:
        t[0] = (t[1], t[3])  # Variable with initialization


# updating variable , ID= exp(arth or logic or comparison);
def p_statement_assignment(t):
    'statement : ID EQUALS expression SEMICOLON'
    var_name = t[1]
    update_symbol_table(var_name, t[3])
# END of var declaration

# ID type 
def p_type(t):
    '''type : INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | CHAR_TYPE'''
    t[0] = t[1]



def p_term_factor(t):
    'term : factor'
    t[0] = t[1]

def p_factor_number(t):
    '''factor : INT
              | FLOAT
              | CHAR
              | BOOL'''
    t[0] = t[1]


# our grammar
# E → E + T
# E → E - T
# E → T
# T → T * F
# T → T / F
# T → F
# F → ( E )
# F → num

quadruplets = []  #list to stock quadruplets
temp_counter = 0   # compteur pour les var temporaires
def new_temp():
    """Génère un nouveau nom de var temp"""
    global temp_counter
    temp_counter += 1
    return f"T{temp_counter}"

# Routine pour vérifier la compatibilité des types
def check_type_compatibility(type1, type2, operator):
    if type1 == type2:
        return type1  # Compatible, retourne le type
    if {type1, type2} == {"int", "float"}:
        return "float"  # Conversion implicite vers float
    print(f"Erreur : Types incompatibles pour l'opération '{operator}' : {type1} et {type2}")
    return None

def p_binary_operators(p):
    '''expression : expression PLUS term
                  | expression MINUS term
       term       : term MULTIPLY factor
                  | term DIVIDE factor'''
    global quadruplets  # Liste globale pour stocker les quadruplets

    # Vérification des types des opérandes
    type1 = get_variable_type(p[1])
    type2 = get_variable_type(p[3])

    # Vérifier si les types sont compatibles
    result_type = check_type_compatibility(type1, type2, p[2])
    if result_type is None:
        p[0] = None  # Erreur de type
        return

    # Générer une nouvelle variable temporaire pour le résultat
    temp_var = new_temp()

    # Ajouter le quadruplet
    quadruplets.append((p[2], p[1], p[3], temp_var))

    # Associer la variable temporaire au résultat
    p[0] = temp_var


# Fonction pour récupérer le type d'une variable ou d'une constante
def get_variable_type(value):
    if isinstance(value, int):
        return "int"
    elif isinstance(value, float):
        return "float"
    elif isinstance(value, str):  # Si c'est un ID, chercher dans la table des symboles
        for entry in symbol_table:
            if entry[0] == value:  # entry[0] = Name
                return entry[1]  # entry[1] = Type
    return None  # Si le type n'est pas trouvé

#affectation
def p_statement_assignment(t):
    'statement : ID EQUALS expression SEMICOLON'
    var_name = t[1]
    # Récupérer le type de la variable
    var_type = get_variable_type(var_name)  
    expr_type = get_variable_type(t[3])  # Type de l'expression à affecter
    # Vérifier la compatibilité des types
    result_type = check_type_compatibility(var_type, expr_type, "=")
    if result_type is None:
        print(f"Erreur d'affectation : incompatible entre {var_type} et {expr_type}")
        return
    quadruplets.append(("=", t[3], None, var_name))  # Affectation simple
    update_symbol_table(var_name, t[3])  # Mettre à jour la table des symboles avec la nouvelle valeur


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


# logical grammar and comparaison
# E → E || T
# E → T
# T → T && F
# T → F
# F → F == G | F != G | F < G | F > G | F <= G | F >= G
# F → G
# G → ! G
# G → ( E )
# G → num
# Step 2: Define the parser with grammar rules
# Logical OR
def p_expression_or(t):
    'expression : expression OR expression'
    t[0] = t[1] or t[3]

# Logical AND
def p_expression_and(t):
    'expression :  expression  AND expression  '
    t[0] = t[1] and t[3]

# Comparison operators (==, !=, <, >, <=, >=)
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
    print("Syntax error at '%s'" % t.value if t else "Syntax error at EOF")

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

# Build the parser
parser = yacc.yacc()

# Test the parser
def parse_statement(statement):
    parser.parse(statement)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

# Examples of usage
if __name__ == '__main__':
    expressions = [
        "int n = 5 == 5 || 3 != 3;",
        "int a =8;",
        "int m =(9<10) && (12<15);",
        "int b =8;",
        "float b = 5.5;",
        "bool c = true;",
        "char d = 'x';",
        "a = 10;",
        "c = false;",
        "c = (a > 5) && (b < 10);",
        "d = 'y';",
        "b = b + a * 2;" ,
        "const int g =10;",
        "int h=10 ,  f;"
    ]
    for stmt in expressions:
        print(f"Parsing statement: {stmt}")
        parse_statement(stmt)
        print("\nSymbol Table:")
        display_symbol_table()
        print("\nQuadruplets:")
        display_quadruplets()
        print("-" * 40)


def get_variable_value(variable_name):
    for entry in symbol_table:
        name, var_type, scope, address, value, additional_info = entry
        if name == variable_name:
            return value  # Correct index for value is 4
    print(f"Erreur : La variable '{variable_name}' n'est pas déclarée ou initialisée.")
    return None  # Si la variable n'est pas trouvée, retourner None (erreur)




# Fonction de vérification de la sémantique 
#Vérification des variables non déclarées ou non initialisées
#Vérification des types d'expressions pour les opérateurs arithmétiques
def semantic_analysis():
    # Fonction qui analyse la table des symboles
    print("\n*** Début de l'analyse sémantique pour les opérations arithmétiques ***")
    
    # Vérification des opérations arithmétiques +, -, *, /
    for quad in quadruplets:
        operator, operand1, operand2, result = quad
        
        # Se concentrer uniquement sur les opérations arithmétiques
        if operator in ['+', '-', '*', '/']:
            # Vérification des types des opérandes
            type1 = get_variable_type(operand1)
            type2 = get_variable_type(operand2)
            
            # Gestion de la division par zéro
            if operator == '/' and get_variable_value(operand2) == 0:
                print(f"Erreur : Division par zéro dans l'opération '{operand1} {operator} {operand2}'")
            
            # Vérification de la compatibilité des types
            if type1 == type2:
                print(f"Opération '{operator}' entre {operand1} ({type1}) et {operand2} ({type2}) est correcte.")
            elif type1 == 'float' or type2 == 'float':
                print(f"Opération '{operator}' entre {operand1} ({type1}) et {operand2} ({type2}) est correcte (conversion implicite vers 'float').")
            else:
                print(f"Erreur de type : Les opérandes '{operand1}' ({type1}) et '{operand2}' ({type2}) sont incompatibles pour l'opération '{operator}'")
    
    print("\n*** Fin de l'analyse sémantique pour les opérations arithmétiques ***")


quadruplets = [
    ('+', 'a', 'b', 'result1'),  # a + b
    ('-', 'x', 'y', 'result2'),  # x - y
    ('*', 'a', '2.0', 'result3'),  # a * 2.0
    ('/', 'b', '0', 'result4'),  # Division par zéro
    ('/', 'x', 'z', 'result5')   # x / z (vérification des types)
]
