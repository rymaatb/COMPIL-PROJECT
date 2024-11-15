import ply.lex as lex
import ply.yacc as yacc
# to print a table 
from tabulate import tabulate


# Liste des tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION' ,  'CONST','INSTRUCTION',  # mots-clés
    'ID', 'COMMENT',
     'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET',  # LBRACE and RBRACE for curly braces, LBRACKET and RBRACKET for square brackets
    'SEMICOLON', 'COMMA',    # Symboles
    'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',  # Opérateurs
    'EQUALS','INT', 'FLOAT', 'CHAR', 'BOOL','INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE','CHAR_TYPE',  'COLON',  # Autres opérateurs et type du symbol
    'IF', 'ELSE', 'FOR',  # Mots-clés du premier code
    'READ', 'WRITE',    # Entrées et sorties
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
t_LBRACKET = r'\['   
t_RBRACKET = r'\]' 

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

# Expressions régulières pour les entrées et sorties
t_READ = r'READ'
t_WRITE = r'WRITE'

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
    'false': 'BOOL',
    'read': 'READ',
    'write': 'WRITE'
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
    if find_in_symbol_table(name, scope):
        print(f"Warning: Variable '{name}' already declared in scope '{scope}'.")
    else:
        memory_address = hex(id(name))  # Generate a unique "memory address" using id
        is_array = isinstance(value, list)  # Detect if it's an array
        entry = [name, var_type, scope, memory_address, value, additional_info, is_array]
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
    '''declaration :  ID
                   | ID EQUALS expression
                   | ID LBRACKET NUMBER RBRACKET'''
    if len(t) == 2:
        t[0] = (t[1], None)  # Variable without initialization
    else:
        t[0] = (t[1], t[3])  # Variable with initialization and array declaration


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

# READ statement
def p_read_statement(t):
    'statement : READ LPAREN ID RPAREN SEMICOLON'
    # t[3] is the ID of the variable being read.
    var_name = t[3]

    # Look up the variable in the symbol table
    for entry in symbol_table:
        if entry[0] == var_name:  # entry[0] is the 'Name' field in the symbol table
            # Ask the user for input and update the value in the symbol table
            value = input(f"Enter a value for '{var_name}': ")
            # Convert the input based on the variable type (could be int, float, etc.)
            if entry[1] == "int":
                entry[4] = int(value)
            elif entry[1] == "float":
                entry[4] = float(value)
            elif entry[1] == "bool":
                entry[4] = value.lower() == 'true'  # Convert input to boolean
            elif entry[1] == "char":
                entry[4] = value[0]  # Assume the first character is valid

            print(f"Value for '{var_name}' set to {entry[4]}")
            break
    else:
        print(f"Error: Variable '{var_name}' not declared.")

def p_write_item_char_array(t):
    'write_item : ID LBRACKET NUMBER RBRACKET'
    var_name = t[1]
    size = t[3]
    # Find the array in the symbol table and prepare it for printing
    for entry in symbol_table:
        if entry[0] == var_name and entry[6]:  # entry[6] is the is_array field
            t[0] = ("ARRAY", var_name, size)
            return
    print(f"Error: Array '{var_name}' not declared.")
    t[0] = ("ARRAY", var_name, size)

def p_write_item_id(t):
    'write_item : ID'
    t[0] = ("ID", t[1])

def p_write_statement(t):
    'statement : WRITE LPAREN write_content RPAREN SEMICOLON'
    t[0] = ("WRITE", t[3])

# Handling content inside WRITE (text, variables, commas)
def p_write_content_multiple(t):
    'write_content : write_content COMMA write_item'
    t[0] = t[1] + [t[3]]

def p_write_content_single(t):
    'write_content : write_item'
    t[0] = [t[1]]



# Build the parser
parser = yacc.yacc()

# Test the parser
def parse_statement(statement):
    parser.parse(statement)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info", "Is Array"]
    for entry in symbol_table:
        if entry[6]:  # Check if it's an array
            entry[4] = f"Array of {entry[4]} elements"  # Display array size or similar
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))


# Examples of usage
if __name__ == '__main__':
    expressions = [
        "int n = 5 == 5 || 3 != 3;",
        "int a =8;",
        "int m =(9<10) && (12<15);",
        "int b =8;",
        "char d, tc[5];",
        "float b = 5.5;",
        "bool c = true;",
        "char d = 'x';",
        "a = 10;",
        "c = false;",
        "c = (a > 5) && (b < 10);",
        "d = 'y';",
        "b = b + a * 2;" ,
        "const int g =10;",
        "int h=10 ,  f;",
        "WRITE(tc);",  # Writing the array
        "WRITE(d);",   # Writing a single character

        # "write(\"Donner la valeur de A :\");"
        # "read(A);"
        # "write(\"La Valeur de A est \", A, \".\");"

    ]
    for stmt in expressions:
        print(f"Parsing statement: {stmt}")
        parse_statement(stmt)
        print("\nSymbol Table:")
        display_symbol_table()
        print("-" * 40)