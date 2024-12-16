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
          

#  *********************TABLE*****************************
def p_statements(t):
    '''statements : statement
                  | statement statements '''
    if len(t) == 2 :
        t[0] = [t[1]]
    else:
        t[0] = [t[1]] + t[2]

def p_statement(t):
    '''statement : simple_assignment
                 | array_declarationTab
                 | array_assignment
                 | type declarationTab_listTab SEMICOLON
                 | const_declarationTab'''

def p_simple_assignment(t):
    '''simple_assignment : ID EQUALS expressionTab SEMICOLON'''
    var_name = t[1]
    expr_value = t[3]
    expr_type = type(expr_value).__name__  # Déterminer le type de l'expressionTab

    # Ajout à la table des symboles
    add_to_symbol_table(var_name, expr_type, "global", expr_value, "Affectation simple")
    update_symbol_table(var_name, expr_value)

    # Génère un quadruplet pour l'affectation
    quadruples.append(('ASSIGN', expr_value, '-', var_name))

    t[0] = var_name  # Retourne le nom de la variable affectée

def p_const_declarationTab(t):
    '''const_declarationTab : CONST type ID EQUALS expressionTab SEMICOLON'''
    var_type, var_name, value = t[2], t[3], t[5]
    print(f"Déclaration constante : {var_name} = {value} (type : {var_type})")
    add_to_symbol_table(var_name, var_type, "global", value, "constant")
  
def p_array_declarationTab(t):
    'array_declarationTab : type ID LBRACKET INTEGER RBRACKET SEMICOLON'
    var_type, var_name, size = t[1], t[2], t[4]
    if size <= 0:
        print(f"Erreur : Taille invalide pour le tableau '{var_name}'.")
    else:
        value = [None] * size
        add_to_symbol_table(var_name, var_type, "global", value, f"Tableau de taille {size}")
           # Génération du quadruplet ADEC
    quadruplet = ("ADEC", var_name, size, None)  # Aucun résultat spécifique dans la 4e colonne
    quadruples.append(quadruplet)
     

def p_declarationTab_listTab(t):
    '''declarationTab_listTab : declarationTab
                        | declarationTab COMMA declarationTab_listTab'''
    if len(t) == 2:
        t[0] = [t[1]]
    else:
        t[0] = [t[1]] + t[3]
        
def p_declarationTab(t):
    '''declarationTab : ID
                   | ID EQUALS expressionTab
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
def p_expressionTab(t):
    '''expressionTab : ID
                  | FLOAT
                  | INTEGER
                  | CHAR'''
    
    t[0] = t[1]

def p_array_access(t):
    'factor : ID LBRACKET expressionTab RBRACKET'
    var_name, index = t[1], t[3]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                if entry[4][index] is None:  # Vérifie si la valeur est non initialisée
                    print(f"Avertissement : Accès à une valeur non initialisée dans '{var_name}[{index}]'.")
                t[0] = entry[4][index]
                value = get_variable_value(var_name)[index]
                 # Génère un quadruplet
                temp_var = new_temp();
                quadruples.append(('=', f'{var_name}[{index}]', value, temp_var))
                t[0] = temp_var  # Retourne la variable temporaire
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                t[0] = None
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
    t[0] = None

def p_array_assignment(t):
    'array_assignment : ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLON'
    var_name, index, value = t[1], t[3], t[6]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                # Vérifie la compatibilité des types
                expected_type = entry[1]  # Type attendu
                if expected_type == 'INTEGER' and not isinstance(value, int):
                    print(f"Erreur : Type incorrect pour '{var_name}[{index}]'. Attendu : INTEGER, reçu : {type(value).__name__}.")
                    return
                elif expected_type == 'FLOAT' and not isinstance(value, float):
                    print(f"Erreur : Type incorrect pour '{var_name}[{index}]'. Attendu : FLOAT, reçu : {type(value).__name__}.")
                    return
                elif expected_type == 'CHAR' and not isinstance(value, str):
                    print(f"Erreur : Type incorrect pour '{var_name}[{index}]'. Attendu : CHAR, reçu : {type(value).__name__}.")
                    return
                # Met à jour la valeur si tout est correct
                entry[4][index] = value
                quadruples.append(('=',value, None, f'{var_name}[{index}]' ))

                
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")

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
        # "FLOAT n = (+5.63) ;",
        # "CHAR j = 'i' ;",
        # "INTEGER c = -4  ;",
        # "bool b = false ;",
        # "bool a = true  ;",
        # "bool m = false;",
        # "bool n = b && a  ;",
        # "bool f = false;",
        # "bool z = f || a   ;",
        # " m = ! a;",
        "INTEGER Matrix[3];"    ,
        "FLOAT Ab[1] ;",
        "Matrix[3]=5;",
        "Matrix[2]=5;",
        "INTEGER x =Matrix[2];"
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