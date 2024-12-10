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
    'IF', 'ELSE', 'FOR', 'READ', 'WRITE',
    
    
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
# regular expressions for read and write
t_READ = r'READ'
t_WRITE = r'WRITE'

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


quadruplets = []
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
                 | array_assignment 
                 | type declaration_list SEMICOLON
                 | const_declaration'''
    

def p_simple_assignment(t):
    '''simple_assignment : ID EQUALS expression SEMICOLON'''
    var_name = t[1]
    expr_value=t[3]
    
    expr_type = type(expr_value).__name__.upper()
  # Déterminer le type de l'expression
    add_to_symbol_table(var_name, expr_type, "global", expr_value, "Affectation simple")
    update_symbol_table(var_name, expr_value)
   


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
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                t[0] = entry[4][index]
                print(f"Valeur récupérée : {t[0]} de {var_name}[{index}]")
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                t[0] = None
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
    t[0] = None


def p_array_assignment(t):
    'array_assignment : ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON'
    var_name, index, value = t[1], t[3], t[6]
    print(f"Affectation de tableau : {var_name}[{index}] = {value}")
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                entry[4][index] = value
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")


# READ statement
def p_read_statement(t):
    'statement : READ LPAREN ID RPAREN SEMICOLON'
    var_name = t[3]
    found = False

    for entry in symbol_table:
        if entry[0] == var_name:  # Check if variable exists
            found = True
            var_type = entry[1]  # Get type from symbol table

            # Check if it's an array of CHAR
            if isinstance(entry[4], list) and var_type == 'CHAR':
                input_string = input(f"Enter a string for '{var_name}' (max {len(entry[4])} chars): ").strip()

                # Update array values
                for i in range(len(entry[4])):
                    if i < len(input_string):
                        entry[4][i] = input_string[i]
                    else:
                        entry[4][i] = None  # Clear remaining slots

                # Add quadruplets for each character read
                for i, char in enumerate(entry[4]):
                    if char is not None:
                        quadruplets.append(('READ', None, None, f"{var_name}[{i}]"))

            # Handle scalar CHAR
            elif var_type == 'CHAR':
                value = input(f"Enter a single char value for '{var_name}': ")
                entry[4] = value[0] if value else None
                quadruplets.append(('READ', None, None, var_name))  # Add quadruplet

            # Handle other types like INTEGER or FLOAT
            elif var_type in ('INTEGER', 'FLOAT'):
                value = input(f"Enter {var_type.lower()} value for '{var_name}': ")
                try:
                    entry[4] = int(value) if var_type == 'INTEGER' else float(value)
                    quadruplets.append(('READ', None, None, var_name))  # Add quadruplet
                except ValueError:
                    print(f"Error: Invalid {var_type.lower()} input.")
            else:
                print(f"Unsupported type '{var_type}' for READ.")
            break

    if not found:
        print(f"Error: Variable '{var_name}' not declared.")



# WRITE statement
def p_write_statement(t):
    'statement : WRITE LPAREN write_content RPAREN SEMICOLON'
    for content in t[3]:
        # Print the value
        print(content, end=' ')
        # Add a quadruplet for each write operation
        quadruplets.append(('WRITE', None, None, content))
    print()  # For a newline after writing


def p_write_content_single(t):
    'write_content : write_item'
    t[0] = [t[1]]

def p_write_content_multiple(t):
    'write_content : write_content COMMA write_item'
    t[0] = t[1] + [t[3]]

def p_write_item(t):
    '''write_item : ID
                  | ID LBRACKET INTEGER RBRACKET'''
    var_name = t[1]

    if len(t) == 2:  # Simple variable (not an array)
        value = get_variable_value(var_name)
        if value is not None:
            t[0] = value
        else:
            print(f"Error: Variable '{var_name}' not declared.")
    else:  # Array access: ID[INTEGER]
        index = t[3]
        value = get_array_value(var_name, index)
        if value is not None:
            t[0] = value

def get_variable_value(var_name):
    for entry in symbol_table:
        if entry[0] == var_name:
            # Check if it's an array
            if "Tableau de taille" in entry[5]:
                print(f"Error: '{var_name}' is an array, not a scalar variable.")
                return None
            return entry[4]  # Return the scalar value
    print(f"Error: Variable '{var_name}' not declared.")
    return None

def get_array_value(var_name, index):
    for entry in symbol_table:
        if entry[0] == var_name and "Tableau de taille" in entry[5]:
            if isinstance(entry[4], list):  # Ensure entry[4] is a list
                if 0 <= index < len(entry[4]):
                    return entry[4][index]
                else:
                    print(f"Error: Index {index} out of bounds for '{var_name}'.")
                    return None
    print(f"Error: Array '{var_name}' not declared.")
    return None





parser = yacc.yacc()


# Test the parser
def parse_statement(statement):
    parser.parse(statement)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))


expressions = [
    "FLOAT Scores[2];",           # Declare FLOAT array
    "Scores[0] = 98.5;",          # Write FLOAT value to index 0
    "Scores[1] = 87.6;",          # Write FLOAT value to index 1

    "CHAR Characters[4];",        # Declare CHAR array
    "Characters[0] = 'A';",       # Write CHAR value to index 0
    "Characters[1] = 'B';",       # Write CHAR value to index 1
    "Characters[2] = 'C';",       # Write CHAR value to index 2
    "Characters[3] = 'D';",       # Write CHAR value to index 3

    # --- Read Operations (Valid) ---
    "FLOAT A;",
    "A = Scores[0];",            # Read FLOAT value from index 0
    "READ(A);",
    "WRITE(A);",
    "WRITE(Characters[2]);",
    "CHAR Name[10];",
    "READ(Name);",
    "WRITE(B);",

]
for stmt in expressions:
    print(f"Parsing statement: {stmt}")
    parse_statement(stmt)
    display_symbol_table()
    print("-" * 40)
