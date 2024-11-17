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
    r'\d+'
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
    '''statement : ID EQUALS expression SEMICOLON'''
    var_name = t[1]  # Le nom de la variable
    value = t[3]     # La valeur à assigner à la variable
    update_symbol_table(var_name, value)  # Mettre à jour la table des symboles





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
def p_binary_operators(p):
    '''expression : expression PLUS term
                  | expression MINUS term
       term       : term MULTIPLY factor
                  | term DIVIDE factor'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    elif p[2] == '*':
        p[0] = p[1] * p[3]
    elif p[2] == '/':
        p[0] = p[1] / p[3]
        

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


# Bloc de code entre accolades
def p_block(t):
    'block : LBRACE statement RBRACE'
    t[0] = t[2]  # Le bloc contient une ou plusieurs instructions

# Nouvelle règle pour gérer les structures if/else avec les blocs
def p_statement_if(t):
    '''statement : IF LPAREN expression RPAREN block ELSE block
                 | IF LPAREN expression RPAREN block ELSE IF LPAREN expression RPAREN block ELSE block
                 | IF LPAREN expression RPAREN block'''
    
    condition = t[3]  # Expression dans le IF
    print(f"Condition value: {condition}")  # Affiche la valeur de la condition

    # Si la condition du premier IF est vraie
    if condition == True:
        print("Condition is true, executing IF block")
       # t[0] = t[6]  # Exécute le bloc IF (instructions à l'intérieur)
    
    # Si ELSE existe
    elif len(t) == 8:
        print("Condition is false, executing ELSE block")
       # t[0] = t[7]  # Exécute le bloc ELSE (instructions à l'intérieur)

    # Si ELSE IF existe
    elif len(t) == 12:
        condition_else_if = t[9]  # Condition du ELSE IF
        print(f"Else if condition value: {condition_else_if}")  # Affiche la valeur de la condition ELSE IF
        if condition_else_if == True:
            print("Else if condition is true, executing ELSE IF block")
           # t[0] = t[11]  # Exécute le bloc ELSE IF (instructions à l'intérieur)
        else:
            print("Else if condition is false, executing ELSE block")
           # t[0] = t[13]  # Exécute le bloc ELSE (instructions à l'intérieur)

# Build the parser
parser = yacc.yacc()

# Test the parser
def parse_statement(statement):
    parser.parse(statement)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

expressions = [
    "int a = 5;",
    "if (a > 3) {a = a + 2;} else {a = a - 1;}", 
    "int b = 10;",
    "if (b < 15) {b = b + 5;} else {b = b - 5;}",  
    "bool isEven = true;",
    "if (isEven) {isEven = false;} else {isEven = true;}",  
    "bool isGreaterThanTen = false;",
    "if (isGreaterThanTen) {isGreaterThanTen = true;} else {isGreaterThanTen = false;}", 
    "int x = 10;",
    "if (x > 5) {x = x + 5;} else if (x == 10) {x = x * 2;} else {x = x - 5;}",  # Si x > 5, additionne 5 ; sinon si x == 10, multiplie par 2 ; sinon, soustrait 5
    "int y = 3;",
    "if (y > 5) {y = y + 1;} else if (y == 3) {y = y + 10;} else {y = y - 1;}",  # Si y > 5, ajoute 1 ; sinon si y == 3, ajoute 10 ; sinon, soustrait 1
    ]

for stmt in expressions:
    print(f"Parsing statement: {stmt}")
    parse_statement(stmt)
    print("-" * 40)
