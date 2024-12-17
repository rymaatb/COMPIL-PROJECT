import ply.lex as lex
import ply.yacc as yacc
import sys
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
t_COLON = r':'

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
    #,'DECLARATION':'DECLARATION',
    #'INSTRUCTION':'INSTRUCTION'
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


#stucture globale 
#def p_program(t):
 #  '''program : DECLARATION block INSTRUCTION block'''
  # t[0] = t[3]+t[7]

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

#INSTUCTIONS


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



# *********************************************Quadruplets *****************************************************
quadruplets = []
temp_count = 0
label_counter = 0
qc = 0
sauve_BG = 0
step = 0 
var_iter = None
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

# Bloc de code entre accolades

def p_statements(t):
    '''statements : statement
                  | statements statement  '''
    if len(t) == 2 :
        t[0] = [t[1]]
    else:
        t[0] = t[1] + [t[2]]

#**********************************************************start of for loop*********************************************************************

#  FOR(i =0 A :2 B :n C){ i=i+1 ;} D
#'statement : FOR LPAREN initialisation A COLON step B COLON BorneSup C RPAREN block D'
def p_statement_FORloop(t):
 # FOR(i =0 A :2 B :n C){ i=i+1 ;} D
    'statement : FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block '
    t[0] = ('statement',t[3],t[5],t[7],t[9])
    D(t)
    
def p_initialisation(t):
    'initialisation : ID EQUALS INT'
    if len(t) < 4:
        print('syntaxic error')
    else:
     t[0] = (t[1], t[3])
     A(t)

def p_step(t):
    '''step : INT
            | ID'''
    if isinstance(t[1],int):
        t[0] = t[1]
        B(t)
    elif isinstance(t[1],str):
        var_name = t[1]
        for entry in symbol_table:
         if entry[0] == var_name:  # entry[0] is the 'Name' field
            if entry[4] is not None:  # entry[4] is the 'Value' field
                t[0] = entry[4]
            else:
                print(f"Error: Variable '{var_name}' not initialized.")
                t[0] = 0
            return
    else:
         print(f"Error: Variable '{var_name}' not declared.")
         t[0] = 0   

def p_BorneSup(t):
    'BorneSup : ID'
    var_name = t[1]
    for entry in symbol_table:
        if entry[0] == var_name:  # entry[0] is the 'Name' field
          if entry[1] == "int":
            if entry[4] is not None:  # entry[4] is the 'Value' field
                t[0] = entry[4]
                C(t)
            else:
                print(f"Error: Variable '{var_name}' not initialized.")
                t[0] = 0
            return
          else:
            print(f"Error: Variable '{var_name}' is not a integer.")
        
    print(f"Error: Variable '{var_name}' not declared.")
    t[0] = 0   

def p_block(t):
    'block : LBRACE statements RBRACE'
    t[0] = t[2] 
   
#************************************************** Routines********************************************

# Routine semantique A : Initialisation de la variable de boucle
def A(t):
    global quadruplets, qc,var_iter
    var_name = t[1]  # Nom de la variable
    var_iter = var_name
    var_value = t[3]  # Valeur initiale
    update_symbol_table(var_name,var_value)
    # Vérifier que la variable et la valeur initiale existent
    if not isinstance(var_value, int):
        raise_semantic_error(var_name, "a une valeur d'initialisation invalide.")
    
    # Générer le quadruplet d'initialisation
    quadruplets.append(('=', var_value, None, var_name))
    qc += 1
    print(f"Routine A : Initialisation '{var_name}' = {var_value}")

# Routine semantique B : Gestion du pas de la boucle
def B(t):
    global quadruplets, qc, step
    step = t[1]  # Valeur du pas
    if not isinstance(step, int) or step <= 0:
        raise_semantic_error("step", "doit etre un entier positif non nul.")
    
    print(f"Routine B : Pas de la boucle valide : step = {step}")

# Routine semantique C : Gestion de la borne superieure
def C(t):
    global quadruplets, qc, sauve_BG,step
    if isinstance(t[1],str):
        var_name = t[1]
        for entry in symbol_table:
         if entry[0] == var_name:  # entry[0] is the 'Name' field
            if entry[4] is not None:  # entry[4] is the 'Value' field
                t[0] = entry[4]
                borne_sup = t[0]  # Valeur de la borne supérieure   
                if not isinstance(borne_sup, int):
                     raise_semantic_error("borne supérieure", "doit etre un entier valide.")
               
                # Sauvegarder l'étiquette de début de la boucle
                sauve_BG = qc
                # Générer la condition de comparaison
                fin= new_label()
                quadruplets.append(('BG',fin ,var_iter, var_name))
                qc += 1
                print(f"Routine C : Borne superieure validee : {step} < {borne_sup}")
            else:
                print(f"Error: Variable '{var_name}' not initialized.")
                t[0] = 0
            return
    else:
         print(f"Error: Variable '{var_name}' not declared.")
         t[0] = 0   

# Routine semantique D : Incrementation et branchement
def D(t):
    global quadruplets, qc, sauve_BG, step,var_iter
    # Générer l'incrémentation
    t0 = new_temp()
    quadruplets.append(('+', var_iter, step, t0))
    qc += 1
    quadruplets.append(('=', t0, None, var_iter))
    qc += 1

    # Retourner au début de la boucle
    statFor = new_label()
    quadruplets.append(('BR', statFor, None, None))
    qc += 1

    print(f"Routine D : Incrementation et branchement de '{var_iter}' avec step = {step}")

def raise_semantic_error(var_name, message):
    print(f"{var_name} {message}")
    raise SyntaxError(f"Semantic Error: Variable '{var_name}' {message}")


#***************************************************************************************************************




# Build the parser
parser = yacc.yacc()
parserdebug = yacc.yacc(debug=True)
# Test the parser
def parse_program(program):
    parser.parse(program)
def parse_statement(statement):
    parser.parse(statement)
def parser_statement_debug(statement):
    parserdebug.parse(statement,debug=True)

# display symbol table as matrix
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

if __name__ == '__main__':
    prm = [
    
        "DECLARATION{int i, n = 2;} INSTRUCTION{ for(i=0 : 1 : n){ i = i + 1;} }"
     ]
    stm = [
        "int i, n = 9,j=0;",
        "for(i =3 : 1 : n){ j = j + 1; j = j * 2;}",
    ]
    with open("testFOR.txt","w") as file:
        sys.stdout = file
        for quad in quadruplets:
              print(f"{quadruplets}")
        print("-" * 40)
        for line in stm:#prm:
            print(f"Parsing: {line}")
       #parse_program(line)
            parse_statement(line)
       #parser_statement_debug(line)
            display_symbol_table()
            for quad in quadruplets:
              print(f"{quad}")
            print("-" * 40)
