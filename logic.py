import ply.lex as lex
import ply.yacc as yacc
# to print a table 
from tabulate import tabulate

# Liste des tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION' ,  'CONST','INSTRUCTION',  # mots-clés
    'ID', 'COMMENT','TRUE','FALSE',
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

# *********************************************Quadruplets *********************************
quadruples = []
temp_count = 0
label_counter = 0 
# Function to generate a new temporary variable
def new_temp():
    global temp_count
    temp_name = f"t{temp_count}"
    temp_count += 1
    return temp_name

def new_label():
    """Generate a new label."""
    global label_counter
    label_counter += 1
    return f"L{label_counter}"

# *********************************************Symbol table *********************************
symbol_table = []

def find_in_symbol_table(name, scope):
    """Search for an entry by name and scope in the symbol table."""
    return any(entry[0] == name and entry[2] == scope for entry in symbol_table)

def findType_in_symbol_table(name ):
    for entry in symbol_table:
        # Check if the name and scope match
        if entry[0] == name :
            return entry[1]
    # If variable not found
    print(f"Warning: Variable '{name}'.")
    return None
       
def get_variable_value(name ):
    for entry in symbol_table:
        # Check if the name and scope match
        if entry[0] == name :
            # Return the value (which is at index 4 in the entry)
            return entry[4]
    
    # If variable not found
    print(f"Warning: Variable '{name}'.")
    return None

def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
    # Check for existing entry with the same name and scope
    if find_in_symbol_table(name, scope):
       print("")
        # print(f"Warning: Variable '{name}' already declared in scope '{scope}'.")
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
        
# ****************************************START of var declartion****************************************
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
    quadruples.append(('=', var_name, None, t[1]))  # Affectation quadruplet
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



# ******************************LOGIC RULE****************************

# EL rule: EL → EL or TL {R7} | TL{R8}
def p_expression_el(t):
    '''expression : expression OR term
                  | term'''
    if len(t) == 4:
        R7(t)   
        value_1 = get_variable_value(t[1]);
        value_2 = get_variable_value(t[3]);
        t[0] = value_1 or value_2
    else:
        t[0] = t[1]


# TL rule: TL → TL and FL{R5} | FL{R6}
def p_term_tl(t):
    '''term : term AND factor
            | factor'''
    if len(t) == 4:
        R5(t)

        value_1 = get_variable_value(t[1]);
        value_2 = get_variable_value(t[3]);
        t[0] = value_1 and value_2
    else:
        t[0] = t[1]


# FL rule: FL → id{R1} | true {R2}| false{R3} | not id{R4}
def p_factor_fl(t):
    '''factor : ID
              | TRUE
              | FALSE
              | NOT ID'''
    if len(t) == 2:
        t[0] = t[1]
    else:
        R4(t)
        true_label = new_label()
        end_label = new_label()
        tem = new_temp()
        quadruples.append(('BNZ',true_label, t[2],None ))
        quadruples.append(('=',1,None ,tem))
        quadruples.append(('BR',end_label, None,None ))
        quadruples.append(('=',0, None,tem ))
        value_2 = get_variable_value(t[2]);
        print(get_variable_value(t[2]));
        t[0] =not value_2


#************************************************** Routine****************************
def raise_semantic_error(var_name, message):
    print(f"{var_name} {message}")
    raise SyntaxError(f"Semantic Error: Variable '{var_name}' {message}")

def check_boolean(var_name):
    """Check that a variable is boolean and declared."""
    if find_in_symbol_table(var_name,"global")==False:
        raise_semantic_error(var_name, "is not declared.")
    if  (findType_in_symbol_table(var_name))!="bool":
        print(findType_in_symbol_table(var_name))
        raise_semantic_error(var_name, "must be of type boolean.")

def R7(t):
    if t[1] !=False and t[1] !=True:
        check_boolean(t[1])  # Check the first operand 
    if t[3] !=False and t[3] !=True:
        check_boolean(t[3])  # Check the second operand
    true_label = new_label()
    true2_label = new_label()
    end_label = new_label()
    tem = new_temp()
    quadruples.append(('BNZ',true_label, t[1],None ))
    quadruples.append(('BNZ',true2_label, t[3],None ))
    quadruples.append(('=',0,None ,tem))
    quadruples.append(('BR',end_label, None,None ))
    quadruples.append(('=',1, None,tem ))
    
def R4(t):
    print ( f"this is t[1] '{t[2] }'")
    check_boolean(t[2])  
    
def R5(t):
    if t[1] !=False and t[1] !=True:
        check_boolean(t[1])  # Check the first operand 
    if t[3] !=False and t[3] !=True:
        check_boolean(t[3])  # Check the second operand
    false_label = new_label()
    false2_label = new_label()
    end_label = new_label()
    tem = new_temp()
    quadruples.append(('BN',false_label, t[1],None ))
    quadruples.append(('BN',false2_label, t[3],None ))
    quadruples.append(('=',1,None ,tem))
    quadruples.append(('BR',end_label, None,None ))
    quadruples.append(('=',0, None,tem ))
    

     
# Build the parser
parser = yacc.yacc()

# Test the parser
def parse_statement(statement):
    global quadruples, temp_count
    quadruples = []  # Reset quadruples for each statement
    temp_count = 0  
    parser.parse(statement)
    return quadruples
    

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

# Examples of usage
if __name__ == '__main__':
    expressions = [
        # "bool n = 2 ;",
        "bool j = true  ;",
        # "bool k = true  ;",
        "bool b = false  ;",
        "bool a = true  ;",
        # "bool c = j || k ;",
        "bool n = b && a || j ;"
        # "bool m = false;",
        # " m = ! j;",
    ]
    for stmt in expressions:
        print(f"Parsing statement: {stmt}")
        parse_statement(stmt)
        print("\nSymbol Table:")
        display_symbol_table()
        print("-" * 40)
        quads = parse_statement(stmt)
        for q in quads:
            print(q)
        print("-" * 30)
