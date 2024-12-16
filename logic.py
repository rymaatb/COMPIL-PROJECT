import ply.lex as lex
import ply.yacc as yacc

from tabulate import tabulate


# List of tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION' ,  'CONST','INSTRUCTION',  # mots-clés
    'ID', 'COMMENT','TRUE','FALSE',
    'LBRACKET', 'RBRACKET',
    'SEMICOLON', 'COMMA',
    'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',
    'EQUALS', 'INTEGER','BOOL', 'FLOAT', 'CHAR', 'INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE', 'CHAR_TYPE',  'COLON',  
    'IF', 'ELSE', 'FOR',  # Mots-clés du premier code

    
)

# Reserved keywords
reserved = {
    'IF': 'IF',
    'ELSE': 'ELSE',
    'FOR': 'FOR',
    'INTEGER': 'INT_TYPE',
    'FLOAT': 'FLOAT_TYPE',
    'bool': 'BOOL_TYPE',
    'CHAR': 'CHAR_TYPE',
    'true': 'BOOL',
    'false': 'BOOL',
    'READ': 'READ',
    'WRITE': 'WRITE',
}

# Regular expressions
# Expressions régulières pour les mots-clés
t_VAR_GLOBAL = r'VAR_GLOBAL'
t_DECLARATION = r'DECLARATION'
t_INSTRUCTION = r'INSTRUCTION'

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
    r'[A-Za-z_][a-zA-Z0-9_]*'  # Permet les lettres, chiffres et underscores
    t.type = reserved.get(t.value, 'ID')  # Vérifie si l'identifiant est un mot-clé
    if t.value == 'true':
        t.value = True
    elif t.value == 'false':
        t.value = False
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
    scope = "global"  
    
    try:
        if t[1] == 'const':
            # Constant declaration
            var_type = t[2]
            var_name = t[3]
            value = t[5]
             # Convert integer to float if the type is FLOAT because Py consider -nb as integer
            if var_type == 'FLOAT' and isinstance(value, int):
                value = float(value)
                
            # Type checking
            if not R0_dec(var_type, value):
                raise SyntaxError(f"Type mismatch: Cannot assign value '{value}' to constant of type '{var_type}'.")

            add_to_symbol_table(var_name, var_type, scope, value, "constant")
        else:
            # Multiple variable declarations
            var_type = t[1]
            for var_name, value in t[2]:
                # Convert integer to float if the type is FLOAT
                if var_type == 'FLOAT' and isinstance(value, int):
                    value = float(value)
                    
                if value is not None and not R0_dec(var_type, value):
                    raise SyntaxError(f"Type mismatch: Cannot assign value '{value}' to variable '{var_name}' of type '{var_type}'.")
                add_to_symbol_table(var_name, var_type, scope, value)
    except SyntaxError as e:
        print(f"Error: {e}")

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
    # quadruples.append(('=', var_name, None, t[1]))  
    update_symbol_table(var_name, t[3])



# ID type 
def p_type(t):
    '''type : INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | CHAR_TYPE'''
    t[0] = t[1]

def p_factor_number(t):
    '''factor : INTEGER
              | FLOAT
              | CHAR
              | BOOL
              | MINUS factor
              | PLUS factor
              | LPAREN INTEGER RPAREN
              | LPAREN FLOAT RPAREN
              | LPAREN MINUS FLOAT RPAREN
              '''
    if len(t) == 2:
        # Direct numeric/character/boolean value
        t[0] = t[1]
    elif t[1] == '-':
        # Negative value
        t[0] = -t[2]
    elif t[1] == '+':
        # Positive value (explicit sign)
        t[0] = t[2]
    elif len(t) == 4 and t[1] == '(':
        # Parenthesized float (positive)
        t[0] = t[2]
    elif len(t) == 5 and t[1] == '(' and t[2] == '-':
        # Parenthesized float (negative)
        t[0] = -t[3]  
          

     
# END of var declaration

# ****************************Routine to verify compatiblity ************
def R0_dec(var_type, value):

    type_checks = {
        'INTEGER': lambda v: isinstance(v, int),
        'FLOAT': lambda v: isinstance(v, float),
        'bool': lambda v: isinstance(v, bool),
        'CHAR': lambda v: isinstance(v, str) and len(v) == 1,
    }
    
    return type_checks.get(var_type, lambda v: False)(value)


# ******************************LOGIC RULE****************************

# EL rule: EL → EL or TL {R7} | TL{R8}
def p_expression_el(t):
    '''expression : expression OR term
                  | term'''
    if len(t) == 4:
        R7(t)   

    else:
        t[0] = t[1]


# TL rule: TL → TL and FL{R5} | FL{R6}
def p_term_tl(t):
    '''term : term AND factor 
            | factor'''
    if len(t) == 4:
        R5(t)
        
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
        "FLOAT n = (+5.63) ;",
        # "CHAR j = 'i' ;",
        # "INTEGER c = -4  ;",
        # "bool b = false ;",
        "bool a = true  ;",
        "bool m = false;",
        # "bool n = b && a  ;",
        # "bool f = false;",
        # "bool z = f || a   ;",
        " m = ! a;",
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
