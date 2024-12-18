import ply.lex as lex
import ply.yacc as yacc
import sys
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
    'READ', 'WRITE' # entrees et sorties

    
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

# regular expressions for read and write
t_READ = r'READ'
t_WRITE = r'WRITE'

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
    r'[+-]?\d+\.\d+'
    t.value = float(t.value)
    
    return t

def t_INTEGER(t):
    r'-?\d+'  # Cela permet de capturer les entiers avec un signe négatif optionnel
    t.value = int(t.value)
    return t


def t_CHAR(t):
    r"\'(.)\'" 
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
    sys.exit(1)
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()
# *********************************************Quadruplets *********************************
quadruples = []
temp_count = 0
label_counter = 0 
quad_counter = 0
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
def increment_quad_counter(value):
    global quad_counter  
    quad_counter += value

def get_counter():
    """Returns the current value of the counter."""
    return quad_counter  
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
    scope = "global"  # Par défaut, le scope est global

    try:
        if t[1] == 'const':
            # Déclaration de constante
            var_type, var_name, value = t[2], t[3], t[5]

            # Type checking
            if not R0_dec(var_type, value):
                raise SyntaxError(f"Type mismatch: Cannot assign value '{value}' to constant of type '{var_type}' at line {get_line()}.")

            add_to_symbol_table(var_name, var_type, scope, value, "constant")
            print(f"Constante déclarée : {var_name} avec valeur : {value}")

        else:
            # Déclaration de variables multiples
            var_type = t[1]
            for var_name, value in t[2]:
                if isinstance(value, tuple) and value[0] == 'ARRAY_ACCESS':  # Vérifie l'accès tableau
                    array_name, index = value[1], value[2]
                    source = f"{array_name}[{index}]"

                    # Obtenir la valeur réelle de la case tableau
                    for entry in symbol_table:
                        if entry[0] == array_name and isinstance(entry[4], list):  # Vérifie que c'est bien un tableau
                            if isinstance(index, int) and 0 <= index < len(entry[4]):
                                value = entry[4][index]  # Récupère la valeur réelle
                                break
                    else:
                        raise SyntaxError(f"Index '{index}' out of bounds for array '{array_name}' at line {get_line()}.")

                    print(f"Accès tableau détecté : {source} avec valeur réelle : {value}")
                else:
                    source = value  # Si ce n'est pas un tableau, on traite l'expression comme source directe

                # Vérification du type
                if value is not None and not R0_dec(var_type, value):
                    raise SyntaxError(f"Type mismatch: Cannot assign value '{value}' to variable '{var_name}' of type '{var_type}' at line {get_line()}.")

                # Ajout à la table des symboles
                add_to_symbol_table(var_name, var_type, scope, value)
                print(f"Variable déclarée : {var_name} avec valeur : {value}")

                # Génération du quadruplet
                quadruple = ('=', source, 'NONE', var_name)
                quadruples.append(quadruple)
                print(f"Quadruplet généré : {quadruple}")

    except SyntaxError as e:
        print(f"Erreur : {e}")
        sys.exit(1)  # Arrête l'exécution en cas d'erreur

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
    expression_value = t[3]  # La valeur ou l'expression du côté droit

    # Cas 1: L'expression est un simple identifiant (variable)
    if not isinstance(expression_value, tuple):
        # Si l'expression droite est une variable simple (non un accès tableau), on l'affecte directement
        update_symbol_table(var_name, expression_value)
        print(f"Variable '{var_name}' affectée avec valeur : {expression_value}")

        # Génération du quadruplet pour une variable simple
        quadruple = ('=', expression_value, None, var_name)  # Ici on garde la forme du quadruplet pour une variable simple
        quadruples.append(quadruple)
        print(f"Quadruplet généré : {quadruple}")

    # Cas 2: L'expression est un accès à une case de tableau
    elif isinstance(expression_value, tuple) and expression_value[0] == 'ARRAY_ACCESS': 
        # Vérifie si l'expression droite est un accès tableau
        array_name, index = expression_value[1], expression_value[2]
        source = f"{array_name}[{index}]"

        # Obtenir la valeur réelle de la case tableau
        for entry in symbol_table:
            if entry[0] == array_name and isinstance(entry[4], list):  # Vérifie que c'est bien un tableau
                if isinstance(index, int) and 0 <= index < len(entry[4]):
                    expression_value = entry[4][index]  # Récupère la valeur réelle
                    break
        else:
            raise SyntaxError(f"Index '{index}' out of bounds for array '{array_name}' at line {get_line()}.")

        print(f"Accès tableau détecté : {source} avec valeur réelle : {expression_value}")

        # Mise à jour de la table des symboles avec la valeur obtenue de l'accès au tableau
        update_symbol_table(var_name, expression_value)
        print(f"Variable '{var_name}' affectée avec valeur de tableau : {expression_value}")
         # Génération du quadruplet pour un accès à une case de tableau
        quadruple = ('=', source, 'NONE', var_name)  # Ici on garde la forme du quadruplet pour l'accès au tableau
        quadruples.append(quadruple)
        print(f"Quadruplet généré : {quadruple}")



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
    '''statement :  array_declarationTab
               
                 | array_assignment
                 | type declarationTab_listTab SEMICOLON
                 | const_declarationTab'''

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
    'factor : ID LBRACKET expression RBRACKET'
    var_name, index = t[1], t[3]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                if entry[4][index] is None:
                    print(f"Avertissement : Accès à une valeur non initialisée dans '{var_name}[{index}]'.")
                t[0] = ('ARRAY_ACCESS', var_name, index)  # Retourne les informations nécessaires
                print(f"Accès tableau détecté : {t[0]}")
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
FL = {
    "name": "",
    "type": "",  
}
TL = {
    "name": "",
    "type": "",  
}
EL = {
    "name": "",
    "type": "",  
}
# EL rule: EL → EL or TL {R7} | TL{R8}
def p_expression_el(t):
    '''expression : expression OR term
                  | term'''
    if len(t) == 4:
        if t[1]==None:
            t[1]=TL['name']
        if t[3]==None:
            t[3]=TL['name']
        R7(t)   
        if t[0]==None:
            t[0]=EL['name']

 


    else:
        R8(t)
        t[0] = t[1]


# TL rule: TL → TL and FL{R5} | FL{R6}
def p_term_tl(t):
    '''term : term AND factor 
            | factor'''
    if len(t) == 4:
        if t[1]==None:
            t[1]=TL['name']

        R5(t)
        
    else:
        if t[1]=='None':
            t[1]=FL['name']
        R6(t)
        t[0] = t[1]


# FL rule: FL → id{R1} | true {R2}| false{R3} | not id{R4}
def p_factor_fl(t):
    '''factor : ID
              | TRUE
              | FALSE
              | NOT ID'''
    if len(t) == 2:
        if t[1] == True :
            R2(t)
        elif t[1] ==False:
            R3(t)
        else:

            R1(t)
            t[0] = t[1]
        
    else:
        R4(t)
        if t[0]==None:
            t[0]=FL['name']

        



#************************************************** Routine****************************

def raise_semantic_error(var_name, message):
    print(f"{var_name} {message}")
    raise SyntaxError(f"Semantic Error: Variable '{var_name}' {message}")

def check_boolean(var_name):
    """Check that a variable is boolean and declared."""
    if find_in_symbol_table(var_name,"global")==False:
        raise_semantic_error(var_name, "is not declared.")
    if  (findType_in_symbol_table(var_name))!="bool":
        raise_semantic_error(var_name, "must be of type boolean.")

def R1(t):
        check_boolean(t[1])  # Check the first operand
        FL['name']=t[1]
        FL['type']=findType_in_symbol_table(t[1])


def R2(t):
    FL['name']=False
    FL['type']='bool'
def R3(t):
    FL['name']=True
    FL['type']='bool'


def R4(t):
    check_boolean(t[2])
    tem = new_temp()
    quadruples.append(('BNZ',get_counter() +3, t[2],None ))
    increment_quad_counter(1) 
    quadruples.append(('=',1,None ,tem))
    increment_quad_counter(1) 
    quadruples.append(('BR',get_counter() +2, None,None ))
    increment_quad_counter(1) 
    quadruples.append(('=',0, None,tem ))  
    increment_quad_counter(1)
    FL['name']=tem
    FL['type']='bool'



def R5(t):

    if( TL['type']=='' or FL['type']=='' or TL['type']=='bool' and FL['type']=='bool'):
        tem = new_temp()
        quadruples.append(('BZ',get_counter() +4, t[1],None ))
        increment_quad_counter(1)   
        quadruples.append(('BZ',get_counter() +3, t[3],None ))
        increment_quad_counter(1)   
        quadruples.append(('=',1,None ,tem))
        increment_quad_counter(1)   
        quadruples.append(('BR',get_counter() +2, None,None ))
        increment_quad_counter(1)   
        quadruples.append(('=',0, None,tem ))
        increment_quad_counter(1)   
        TL['name']=tem
        TL['type']='bool'
    else:
        raise_semantic_error( TL['type'],"Incompatiblity of type.")


  
def R6(t):
        FL['name']=FL['name']
        FL['type']=FL['type']

    


def R7(t):

    tem = new_temp()
    quadruples.append(('BNZ', get_counter() +4 , t[1],None ))
    increment_quad_counter(1)   
    quadruples.append(('BNZ', get_counter() + 3, t[3],None ))
    increment_quad_counter(1)   
    quadruples.append(('=',0,None ,tem))
    increment_quad_counter(1)   
    quadruples.append(('BR',get_counter() + 2, None,None ))
    increment_quad_counter(1)   
    quadruples.append(('=',1, None,tem ))
    increment_quad_counter(1)  
    EL['name']=tem
    EL['type']='bool'

def R8(t):
        EL['name']=TL['name']
        EL['type']=TL['type']


# READ 
def p_read_statement(t):
    '''statement : READ LPAREN ID RPAREN SEMICOLON
                 | READ LPAREN ID LBRACKET INTEGER RBRACKET RPAREN SEMICOLON'''
    var_name = t[3]

    if len(t) == 6:  # Case: READ(ID)
        # Normal ID (simple variable read)
        process_read_var(var_name, None)
    elif len(t) == 9:  # Case: READ(ID[INTEGER])
        index = int(t[5])  # Extract the index from t[6]
        process_read_array(var_name, index)
    else:
        print("Unexpected read format.")
        
def process_read_var(var_name, index=None):
    found = False
    for entry in symbol_table:
        if entry[0] == var_name:  # Check if variable exists
            found = True
            var_type = entry[1]  # Get type from symbol table
            
            if var_type == 'CHAR':
               
                #entry[4] = value[0] if value else None
                quadruples.append(('READ', None, None, var_name if index is None else f"{var_name}[{index}]"))
            
            elif var_type in ('INTEGER', 'FLOAT'):
               
                try:
                    #entry[4] = int(value) if var_type == 'INTEGER' else float(value)
                    quadruples.append(('READ', None, None, var_name if index is None else f"{var_name}[{index}]"))
                except ValueError:
                    print(f"Error: Invalid {var_type.lower()} input.")
            else:
                print(f"Unsupported type '{var_type}' for READ.")
            break

    if not found:
        print(f"Error: Variable '{var_name}' not declared.")

def process_read_array(var_name, index):
    found = False
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Ensure it's an array
            found = True
            if 0 <= index < len(entry[4]):
                var_type = entry[1]
                if var_type == 'CHAR':
                  
                    quadruples.append(('READ', None, None, f"{var_name}[{index}]"))
                elif var_type in ('INTEGER', 'FLOAT'):
                   
                    try:
                      
                        quadruples.append(('READ', None, None, f"{var_name}[{index}]"))
                    except ValueError:
                        print(f"Error: Invalid {var_type.lower()} input.")
                else:
                    print(f"Unsupported type '{var_type}' for READ.")
            else:
                print(f"Error: Index {index} out of bounds for array '{var_name}'.")
            break
    if not found:
        print(f"Error: Variable '{var_name}' not declared as an array.")






# WRITE statement
def p_write_statement(t):
    'statement : WRITE LPAREN write_content RPAREN SEMICOLON'
   


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
            if isinstance(entry[4], list):
                return None  # Indicates an error
            return entry[4]  # Return the scalar value
    return None  # Indicates variable not found

def get_array_value(var_name, index):
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):
            if 0 <= index < len(entry[4]):
                return entry[4][index]
            else:
                return None  # Indicates index out of bounds
    return None  # Indicates array not found



     
# Build the parser
parser = yacc.yacc()

line_co =0
def increment_line_counter():
    global line_co  
    line_co += 1

def get_line():
    return line_co  
# Test the parser
def parse_statement(statement):
    global quadruples, temp_count
    quadruples = []  
    temp_count = 0  
    increment_line_counter()
    parser.parse(statement)
    return quadruples
    

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

# Examples of usage
if __name__ == '__main__':
    expressions = [
        "CHAR g ;" ,
         " g = '5'  ;",
       
        # "INTEGER n = (+5.63) ;",
        # "INTEGER c = 43  ;",
        # "INTEGER d = 5  ;",
        # "CHAR d = '5'  ;",
        # "bool b = false ;",
        # "bool a = true  ;",
        # "bool m = false;",
        # "bool n = b && a  ;",
        # "bool f = false;",
        # "bool z = f || a   ;",
        # " m = ! a;",
         "INTEGER Matrix[3];",
        "Matrix[2] = 2;",
        " INTEGER X;",
         "X = Matrix[2];" 
        
 
    ]
    for stmt in expressions:
        print(f"Parsing statement: {stmt}")
        parse_statement(stmt)
        print("\nSymbol Table:")
        display_symbol_table()
        print("-" * 40)
        # quads = parse_statement(stmt)
        for q in quadruples:
            print(q)
        print("-" * 30)