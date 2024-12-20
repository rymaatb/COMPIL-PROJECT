import ply.lex as lex
import ply.yacc as yacc
import sys
from tabulate import tabulate
from lexer import tokens
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
# for constant
def get_variable_AddInfo(name ):
    try:
        for entry in symbol_table:
        # Check if the name and scope match
            if entry[0] == name :
            # Return the value (which is at index 4 in the entry)
                return entry[5]
        
        raise SyntaxError(f"  '{name}' not found   at lign'{get_line()}'.")

    # If variable not found
    except SyntaxError as e:
        print(f"Error: {e}")
        sys.exit(1)  # Stops execution of the program on error

def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
    # Check for existing entry with the same name and scope
    try:
        if find_in_symbol_table(name, scope):
            print("hejoif")
            # raise SyntaxError(f" at lign'{get_line()}' '{name}' already declared   .")
            print("")
        else:
            memory_address = hex(id(name))  # Generate a unique "memory address" using id
            entry = [name, var_type, scope, memory_address, value, additional_info]
            symbol_table.append(entry)
    except SyntaxError as e:
        print(f"Error: {e}")
        sys.exit(1) 
# def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
#     # Check for existing entry with the same name and scope
#     if find_in_symbol_table(name, scope):
#        print(f"Warning: Variable '{name}' already declared in scope '{scope}'.")
#     else:
#         memory_address = hex(id(name))  # Generate a unique "memory address" using id
#         entry = [name, var_type, scope, memory_address, value, additional_info]
#         symbol_table.append(entry)

# a function to update the symbol table
def update_symbol_table(name, value):
    for entry in symbol_table:
        if entry[0] == name:  # entry[0] is the 'Name' field
            entry[4] = value  # entry[4] is the 'Value' field
            break
        
class SemanticError(Exception):
    """Custom exception for semantic errors."""
    pass
# ****************************************general structure****************************************
def p_PROGRAMME(t):
    '''PROGRAMME : varGlobal declaration_ instruction'''
    t[0] = [t[1]]+ [t[2]] + [t[3]]
def p_varGlobal(t):
    '''varGlobal : VAR_GLOBAL LBRACE declarations RBRACE '''
    t[0] = [t[3]]
def p_declaration_(t):
    '''declaration_ : DECLARATION LBRACE declarations RBRACE'''
    t[0] = [t[3]]
def p_instruction(t):
    '''instruction : INSTRUCTION LBRACE statements RBRACE'''
    t[0] = [t[3]]
def p_declarations(t):
    '''declarations : declaration declarations
                    | declaration
                    | empty'''
    if len(t) == 2:
        t[0] = [t[1]]
    elif len(t) == 3:
        t[0] = [t[1]] + t[2]
def p_empty(t):
    '''empty :'''
    t[0] = None
# ****************************************START of var declartion****************************************
# the expression will be TYPE multiple var; OR const id = value;
def p_statement_declaration(t):
    '''declaration : type declaration_list SEMICOLON
                   | CONST type ID EQUALS expression SEMICOLON'''
    scope = "global"  
    global quadruples
    try:
        if t[1] == 'CONST':
            # Constant declaration
            var_type = t[2]
            var_name = t[3]
            value = t[5]
             # Convert integer to float if the type is FLOAT because Py consider -nb as integer
            if var_type == 'FLOAT' and isinstance(value, int):
                value = float(value)
                
            # Type checking
            if not R0_dec(var_type, value):

                raise SyntaxError(f" Type mismatch: Cannot assign value '{value}' to constant of type '{var_type}' at lign'{get_line()}'.")
            add_to_symbol_table(var_name, var_type, scope, value, "constant")
            quadruples.append((('=',value, None,var_name )))
            increment_quad_counter(1)
        else:
            # Multiple variable declarations
            var_type = t[1]
            for var_name, value in t[2]:
                # Convert integer to float if the type is FLOAT
                if var_type == 'FLOAT' and isinstance(value, int):
                    value = float(value)
                if var_type=='bool' and isinstance(value, str) and value.startswith("t"):
                    print("") # si on a un temporaire on fait l'affectation son verifier 
                elif value is not None and not R0_dec(var_type, value):

                    raise SyntaxError(f"Type mismatch: Cannot assign value '{value}' to variable '{var_name}' of type '{var_type}' at lign'{get_line()}.")
                add_to_symbol_table(var_name, var_type, scope, value)
                quadruples.append((('=',value, None,var_name )))
                increment_quad_counter(1)
    except SyntaxError as e:
        print(f"Error: {e}")
        sys.exit(1)  # Stops execution of the program on error

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
    try:
        # error handling of constant
        if get_variable_AddInfo(t[1]) == 'constant':
            raise SyntaxError(f"  can't assign '{t[3]}' to '{t[1]}' because '{t[1]}' is a constant   at lign'{get_line()}'.")
        else:
            var_name = t[1]
            update_symbol_table(var_name, t[3])
    except SyntaxError as e:
        print(f"Error: {e}")
        sys.exit(1)  # Stops execution of the program on error




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
                  | statement statements
                  | empty '''
    if len(t) == 2 :
        t[0] = [t[1]]
    else:
        t[0] = [t[1]] + t[2]

def p_statement(t):
    '''statement : simple_assignment
                 | array_assignment
                 '''
def p_declaration_t(t):
    '''declaration : array_declarationTab
                 | type declarationTab_listTab SEMICOLON
                 | const_declarationTab'''

def p_simple_assignment(t):
    '''simple_assignment : ID EQUALS expressionTab SEMICOLON'''
    global quadruples
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
    global quadruples
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
    global quadruples
    var_name, index = t[1], t[3]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                if entry[4][index] is None:  # Vérifie si la valeur est non initialisée
                    print(f"Avertissement : Accès à une valeur non initialisée dans '{var_name}[{index}]'.")
                t[0] = entry[4][index]
                value = get_variable_value(var_name)[index]
                if value is None:
                    print(f"Erreur : La variable '{var_name}' n'est pas initialisée.")
                    return
                 # Génère un quadruplet
                print(t[1])
                if  var_name!= t[1]:
                    quadruples.append((('=',value, None,var_name )))
                    increment_quad_counter(1)
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                t[0] = None
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
    t[0] = None

def p_array_assignment(t):
    'array_assignment : ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLON'
    global quadruples
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
        if t[0]==None:
            t[0]=TL['name']
        
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
    print(var_name)
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
    global quadruples
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
    global quadruples
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
        print(TL['name'])

        
    else:
        raise_semantic_error( TL['type'],"Incompatiblity of type.")


  
def R6(t):
        FL['name']=FL['name']
        FL['type']=FL['type']

    


def R7(t):
    global quadruples
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
        
#************************Arithmetic*********************************
# our grammar
# E → E + T
# E → E - T
# E → T
# T → T * F
# T → T / F
# T → F
# F → ( E )
# F → num
# ************************* Grammaire et Routines Sémantiques *************************
# E → E + T (R6)     : Addition de deux termes et génération d'un quadruplet
# E → T (R5)         : Expression réduite à un terme
# T → T * F (R4)     : Multiplication d'un terme par un facteur et génération d'un quadruplet
# T → F (R3)         : Terme réduit à un facteur
# F → id (R1)        : Facteur est un identifiant (récupération de sa valeur/type)
# F → (E) (R2)       : Parenthèses autour d'une expression pour changer la priorité
# ************************************************************************************
def p_statement_assignmentArth(t):
    '''statement : type ID EQUALS expression_arithmetique SEMICOLON'''
    print(f"Assigning {t[4]} to variable {t[2]} of type {t[1]}")
    # Example: Add variable to symbol table
    add_to_symbol_table(t[2], t[1], "global", t[4])
def p_declarationArth(t):
    '''statement :  ID EQUALS expression_arithmetique SEMICOLON'''
    print(f"Assigning {t[3]} to variable {t[1]} of type {t[1]}")
    # Example: Add variable to symbol table
    update_symbol_table(t[1], t[3])

def p_expression_plus(t):
    '''expression_arithmetique : expression_arithmetique PLUS term_arithmetique  
    '''
    RA6(t)

def p_expression_minus(t):
    'expression_arithmetique : expression_arithmetique MINUS term_arithmetique'
    RA7(t)

def p_expression_term(t):
    'expression_arithmetique : term_arithmetique'
    RA5(t)

def p_term_multiply(t):
    'term_arithmetique : term_arithmetique MULTIPLY term_arithmetique'
    RA4(t)


def p_term_divide(t):
    'term_arithmetique : term_arithmetique DIVIDE factor_arithmetique'
    RA8(t)

def p_term_factor(t):
    'term_arithmetique : factor_arithmetique'
    RA3(t)

def p_factor_id(t):
    '''factor_arithmetique : ID
                        | INTEGER
                        | FLOAT
                        '''
    RA1(t)

def p_factor_parens(t):
    'factor_arithmetique : LPAREN expression_arithmetique RPAREN'
    RA2(t)

# *************************** Routines Sémantiques pour Arithmétique ***************************

def check_numeric(var_name):
    """Vérifie que la variable est déclarée et est de type numérique (int ou float)."""
    if not find_in_symbol_table(var_name, "global"):
        raise_semantic_error(var_name, "is not declared.")
    if findType_in_symbol_table(var_name) not in ["INTEGER", "FLOAT"]:
        raise_semantic_error(var_name, "must be of type int or float.")

def check_variable_declared(var_name):
    """Vérifie que la variable est déclarée dans la table des symboles."""
    try:
        if  isinstance(var_name, str) and var_name.startswith("t"):
                        print("")
        elif isinstance(var_name, int) or isinstance(var_name, float):
            print("")
        elif not find_in_symbol_table(var_name, "global"):
            raise SemanticError(f"  '{var_name}' is not declared '{get_line()}'.")
    except SemanticError as e:
        print(f"Error: {e}")
        sys.exit(1)  # Stops execution of the program on error

def check_variable_type(var_name, expected_type=None):
    """Vérifie que la variable est de type numérique (int ou float)."""
    check_variable_declared(var_name)  # Vérifie si la variable est déclarée
    if  isinstance(var_name, str) and var_name.startswith("t"):
                    print("")
    elif isinstance(var_name, int) or isinstance(var_name, float):
        print("")
    else:
        if expected_type:
            if findType_in_symbol_table(var_name) != expected_type:
                raise_semantic_error(var_name, f"must be of type  {expected_type}.")
        elif findType_in_symbol_table(var_name) not in ["INTEGER", "FLOAT"]:
            raise_semantic_error(var_name, "must be of type int or float.")

def check_notDivZERO(var_name):
    if isinstance(var_name, int):
        if var_name ==0:
                        raise_semantic_error(var_name, "can't divide by zero .")

    
def RA1(t):
    #"""F → id : Facteur est un identifiant."""
    check_variable_type(t[1])  # Vérifie que la variable est déclarée et numérique
    t[0] = t[1]  # Retourne l'identifiant comme facteur

def RA2(t):
    #"""F → ( E ) : Parenthèses autour de l'expression."""
    t[0] = t[2]  # Retourne l'expression parenthésée

def RA3(t):
    #"""T → F : Terme réduit à un facteur."""
    t[0] = t[1]  # Retourne le facteur comme terme

def RA4(t):
    # """T → T * F : Multiplication entre termes."""
    check_variable_type(t[1])  # Vérifie que le terme est déclaré et numérique
    check_variable_type(t[3])  # Vérifie que le facteur est déclaré et numérique
    temp_var = new_temp()
    quadruples.append(('*', t[1], t[3], temp_var))
    t[0] = temp_var  # Le résultat est dans une variable temporaire

def RA5(t):
    #"""E → T : Expression réduite à un terme."""
    t[0] = t[1]  # Retourne le terme comme expression

def RA6(t):
    # """E → E + T : Addition entre expressions."""
    check_variable_type(t[1])  # Vérifie que le terme est déclaré et numérique
    check_variable_type(t[3])  # Vérifie que le facteur est déclaré et numérique
    temp_var = new_temp()
    quadruples.append(('+', t[1], t[3], temp_var))
    t[0] = temp_var  # Le résultat est dans une variable temporaire

def RA7(t):
    #"""E → E - T : Soustraction entre expressions."""
    check_variable_type(t[1])  # Vérifie que le terme est déclaré et numérique
    check_variable_type(t[3])  # Vérifie que le facteur est déclaré et numérique
    temp_var = new_temp()
    quadruples.append(('-', t[1], t[3], temp_var))
    t[0] = temp_var  # Le résultat est dans une variable temporaire

def RA8(t):
    #"""T → T / F : Division entre termes."""
    check_variable_type(t[1])  # Vérifie que le terme est déclaré et numérique
    check_variable_type(t[3])  # Vérifie que le facteur est déclaré et numérique
    check_notDivZERO(t[3])
    temp_var = new_temp()
    quadruples.append(('/', t[1], t[3], temp_var))
    t[0] = temp_var  # Le résultat est dans une variable temporaire

# Fonction pour les erreurs syntaxiques
def p_error(t):
    if t:
        print(f"Syntax error at token {t.type}, value {t.value}")
    else:
        print("Syntax error at end of input")
# *********************************************************Read and write *********************************************************

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
    global quadruples
    found = False
    for entry in symbol_table:
        if entry[0] == var_name:  # Check if variable exists
            found = True
            var_type = entry[1]  # Get type from symbol table
            
            if var_type == 'CHAR':
               
                #entry[4] = value[0] if value else None
                quadruples.append(('READ', None, None, var_name if index is None else f"{var_name}[{index}]"))
                increment_quad_counter(1)
            
            elif var_type in ('INTEGER', 'FLOAT'):
               
                try:
                    #entry[4] = int(value) if var_type == 'INTEGER' else float(value)
                    quadruples.append(('READ', None, None, var_name if index is None else f"{var_name}[{index}]"))
                    increment_quad_counter(1)
                except ValueError:
                    print(f"Error: Invalid {var_type.lower()} input.")
            else:
                print(f"Unsupported type '{var_type}' for READ.")
            break

    if not found:
        print(f"Error: Variable '{var_name}' not declared.")

def process_read_array(var_name, index):
    global quadruples
    found = False
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Ensure it's an array
            found = True
            if 0 <= index < len(entry[4]):
                var_type = entry[1]
                if var_type == 'CHAR':
                  
                    quadruples.append(('READ', None, None, f"{var_name}[{index}]"))
                    increment_quad_counter(1)
                elif var_type in ('INTEGER', 'FLOAT'):
                   
                    try:
                      
                        quadruples.append(('READ', None, None, f"{var_name}[{index}]"))
                        increment_quad_counter(1)
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

# def get_variable_value(var_name):
#     for entry in symbol_table:
#         if entry[0] == var_name:
#             # Check if it's an array
#             if isinstance(entry[4], list):
#                 return None  # Indicates an error
#             return entry[4]  # Return the scalar value
#     return None  # Indicates variable not found

def get_array_value(var_name, index):
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):
            if 0 <= index < len(entry[4]):
                return entry[4][index]
            else:
                return None  # Indicates index out of bounds
    return None  # Indicates array not found


#**********************************************************START of for loop*********************************************************************

def p_statement_FORloop(t):
    #statement --> FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block {D}
    'statement : FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block '
    t[0] = ('statement',t[3],t[5],t[7],t[9])
    D(t)
    
def p_initialisation(t):
    #initialisation --> ID EQUALS INT {A}
    'initialisation : ID EQUALS INTEGER'
    if len(t) < 4:
        print('syntaxic error')
    else:
     t[0] = (t[1], t[3])
     A(t)

def p_step(t):
    #step --> INT {B} | ID {B1}
    '''step : INTEGER
            | ID'''
    if isinstance(t[1],int):
        B(t)
    elif isinstance(t[1],str):
        B1(t)
        
def p_BorneSup(t):
    #BorneSup --> ID {C}
    'BorneSup : ID'
    C(t)

def p_block(t):
    'block : LBRACE statements RBRACE'
    t[0] = t[2] 
 
#************************************************** Routines********************************************
sauve_BG = 0
step = 0 
var_iter = None
# Routine semantique A : Initialisation de la variable de boucle
def A(t):
    global quadruples,var_iter
    var_name = t[1]  # Nom de la variable
    var_iter = var_name
    var_value = t[3]  # Valeur initiale
    #Check that the variable is integer and declared.
    if find_in_symbol_table(var_name,"global") == False:
        raise_semantic_error(var_name, "is not declared.")
    if  (findType_in_symbol_table(var_name))!="INTEGER":
        raise_semantic_error(var_name, "must be of type INTEGER.")
    # check if the initialisation value in an int
    if R0_dec('INTEGER',var_value) == False:
        raise_semantic_error(var_name, "a une valeur d'initialisation invalide.")
    
    update_symbol_table(var_name,var_value)
    # Générer le quadruplet d'initialisation
    quadruples.append(('=', var_value, None, var_name))
    increment_quad_counter(1)
    print(f"Routine A : Initialisation '{var_name}' = {var_value}")

# Routine semantique B : Gestion du pas de la boucle cas int
def B(t):
    global quadruples, step
    step = t[1]  # Valeur du pas
    if not isinstance(step, int) or step <= 0:
        raise_semantic_error("step", "must be a positif integer")
    t[0]=t[1]
    print(f"Routine B : Pas de la boucle valide : step = {step}")

# Routine semantique B1 : Gestion du pas de la boucle cas id
def B1(t):
    global quadruples, step
    var_name = t[1]
    #Check that the variable is integer and declared.
    if find_in_symbol_table(var_name,"global") == False:
        raise_semantic_error(var_name, "is not declared.")
    if  (findType_in_symbol_table(var_name))!="INTEGER":
        raise_semantic_error(var_name, "must be of type INTEGER.")
    if get_variable_value(var_name) == None :
        raise_semantic_error(var_name,"is not initialized")
    elif get_variable_value(var_name) <= 0 :
        raise_semantic_error(var_name,"must be a positif integer")
    step = get_variable_value(var_name)
    t[0] = step
    print(f"Routine B : Pas de la boucle valide : step = {step}")

# Routine semantique C : Gestion de la borne superieure
def C(t):
    global quadruples, sauve_BG,step
    var_name = t[1]
    #Check that the variable is integer and declared.
    if find_in_symbol_table(var_name,"global") == False:
        raise_semantic_error(var_name, "is not declared.")
    if  (findType_in_symbol_table(var_name))!="INTEGER":
        raise_semantic_error(var_name, "must be of type INTEGER.")
    if get_variable_value(var_name) == None :
        raise_semantic_error(var_name,"is not initialized")
    elif get_variable_value(var_name) < step :
        raise_semantic_error(var_name,"must be at least equal to iteration variable")
    borne_sup = get_variable_value(var_name)
    print(f"Routine C : Borne superieure validee : {step} < {borne_sup}")
    # Sauvegarder l'étiquette de début de la boucle
    sauve_BG = get_counter
     # quadruplets
    fin= new_label()
    quadruples.append(('BG',fin ,var_iter, var_name))
    increment_quad_counter(1)

# Routine semantique D : Incrementation et branchement
def D(t):
    global  sauve_BG, step,var_iter
    # Générer l'incrémentation
    t0 = new_temp()
    quadruples.append(('+', var_iter, step, t0))
    increment_quad_counter(1)
    quadruples.append(('=', t0, None, var_iter))
    increment_quad_counter(1)
    # Retourner au début de la boucle
    statFor = new_label()
    quadruples.append(('BR', statFor, None, None))
    increment_quad_counter(1)

    print(f"Routine D : Incrementation et branchement de '{var_iter}' avec step = {step}")


 #**********************************IF condition*********************************************************
def p_statementIf(t):
    '''statement : IF LPAREN condition RPAREN block
                   | IF LPAREN condition RPAREN block ELSE block'''
    if len(t) == 6:  # If there is no else block
        t[0] = ('statement', t[3], t[5])
    elif len(t) == 8:  # If there is an else block
        t[0] = ('statement', t[3], t[5], t[7])

def p_condition(t):
    '''condition : ID EQUALS ID
                 | ID LT ID
                 | ID GT ID
                 | ID EQUALS factor
                 | ID LT factor
                 | ID GT factor'''
    t[0] = ('condition', t[2], t[1], t[3])  # ('operator', left, right)
       
    
# Build the parser
parser = yacc.yacc()
parserdebug = yacc.yacc(debug=True)

line_co =0
def increment_line_counter():
    global line_co  
    line_co += 1

def get_line():
    return line_co  
# Test the parser
def parse_statement(statement):
    global quadruples, temp_count 
    increment_line_counter()
    parser.parse(statement)
    return quadruples
def parse_program(program):
    global quadruples, temp_count
    increment_line_counter()
    parser.parse(program) 
    return quadruples  
def parser_statement_debug(program):
    global quadruples, temp_count
    increment_line_counter()
    parserdebug.parse(program,debug=True)
    return quadruples 

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

# Examples of usage
if __name__ == '__main__':
    expressions = [

        # "INTEGER c = 43  ;",
        # " INTEGER c = 15 ;",
        # "CHAR d = '5'  ;",
        # "INTEGER Matrix[3];"    ,
        # "FLOAT Ab[1] ;",
        # "Matrix[3]=5;",
        # "Matrix[2]=5;",
        # "Matrix[1]=3;",
        # "INTEGER x =Matrix[2];",
        # "INTEGER y =Matrix[1];",
        # "bool b = false ;",
        # "bool a = true  ;",
        # "bool m = true ;",
        # "bool d = false  ;",
        # "bool e = true  ;",
        # "bool f = a && b ||a&&!e ;",
        # "INTEGER i, n = 9;",
        # "FOR(i =3 : 1 : n){ m = b && a ; }",
        #"INTEGER k = 5;",                   # Déclaration et affectation d'une variable
        #"INTEGER l = 3;",                   # Déclaration et affectation d'une autre variable
        #"INTEGER p = 2 + 3;",               # Expression avec addition
        #" p = 1 * 3;",               # Expression avec addition
        # "l= k * p;",               # Expression avec multiplication
        #"INTEGER z = k + l * p;",           # Expression avec priorité des opérateurs (multiplication avant addition)
        # "INTEGER o = (k + l) * z;",         # Expression avec parenthèses
        # "INTEGER g = (k + l) * (p - z);",   # Expression plus complexe avec parenthèses
        # "INTEGER h = 8 / 0;",               # Expression avec division
        # "INTEGER r = k - l + z;",           # Expression avec addition et soustraction
        # "INTEGER j = (a + b) / (c - d);"    # Expression avec des parenthèses dans la division
        # "CONST INTEGER r = 5  ;",
        # "  r = 5  ;",#error
        # "INTEGER n = (+5.63) ;",#error

    ]

    prm = "VAR_GLOBAL{ CONST INTEGER G = 3; } DECLARATION{INTEGER a = 6; bool z = false;} INSTRUCTION{IF (a > 3){a = a + 2; IF(z=false){a = a*a;}}}"
     

    for stmt in expressions:
        print(f"Parsing statement: {stmt}")
        #parse_statement(stmt)
        print("\nSymbol Table:")
        display_symbol_table()
        print("-" * 40)
        # quads = parse_statement(stmt)
        for q in quadruples:
            print(q)
        print("-" * 30)

    #parser_statement_debug(prm)
    parse_program(prm)
    display_symbol_table()
    print("-" * 40)
    for q in quadruples:
        print(q)
    print("-" * 40)