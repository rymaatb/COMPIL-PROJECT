import ply.lex as lex
import ply.yacc as yacc
from tabulate import tabulate

# List of tokens
tokens = (
    'VAR_GLOBAL', 'DECLARATION', 'CONST', 'INSTRUCTION',
    'ID', 'COMMENT',
    'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET',
    'SEMICOLON', 'COMMA',
    'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'LPAREN', 'RPAREN',
    'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE', 'AND', 'OR', 'NOT',
    'EQUALS', 'INT', 'FLOAT', 'CHAR', 'BOOL', 'INT_TYPE', 'FLOAT_TYPE', 'BOOL_TYPE', 'CHAR_TYPE', 'COLON',
    'IF', 'ELSE', 'FOR',
    'READ', 'WRITE',
)

# Regular expressions for keywords
t_VAR_GLOBAL = r'VAR_GLOBAL'
t_DECLARATION = r'DECLARATION'
t_INSTRUCTION = r'INSTRUCTION'

# Regular expressions for symbols
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','
t_LBRACKET = r'\['
t_RBRACKET = r'\]'

# Regular expressions for operators
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

# Regular expressions for I/O
t_READ = r'READ'
t_WRITE = r'WRITE'

# Reserved keywords
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR',
    'int': 'INT_TYPE',
    'float': 'FLOAT_TYPE',
    'bool': 'BOOL_TYPE',
    'char': 'CHAR_TYPE',
    'true': 'BOOL',
    'false': 'BOOL',
    'const': 'CONST',
    'READ': 'READ',
    'WRITE': 'WRITE',
}

# Token for identifiers
def t_ID(t):
    r'[A-Za-z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    if t.value == 'true':
        t.value = True
    elif t.value == 'false':
        t.value = False
    return t

# Tokens for constants
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_CHAR(t):
    r"'.'"
    t.value = t.value[1]  # Extract character
    return t

# Ignore comments
def t_COMMENT(t):
    r'%%.*'
    pass

# Handle newlines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Ignore spaces and tabs
t_ignore = ' \t'

# Handle errors
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Symbol table as a list of dictionaries
symbol_table = []

def find_in_symbol_table(name, scope):
    return any(entry[0] == name and entry[2] == scope for entry in symbol_table)

def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
    if find_in_symbol_table(name, scope):
        print(f"Warning: Variable '{name}' already declared in scope '{scope}'.")
    else:
        memory_address = hex(id(name))
        is_array = isinstance(value, list)
        entry = [name, var_type, scope, memory_address, value, additional_info, is_array]
        symbol_table.append(entry)

def update_symbol_table(name, value):
    for entry in symbol_table:
        if entry[0] == name:
            entry[4] = value
            break

# Grammar rules

# Variable and array declaration
def p_statement_declaration(t):
    '''statement : type declaration_list SEMICOLON
                 | CONST type ID EQUALS expression SEMICOLON'''
    scope = "global"
    if t[1] == 'const':
        var_type, var_name, value = t[2], t[3], t[5]
        add_to_symbol_table(var_name, var_type, scope, value, "constant")
    else:
        var_type = t[1]
        for var_name, value in t[2]:
            add_to_symbol_table(var_name, var_type, scope, value)







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
                   | ID LBRACKET NUMBER RBRACKET'''
    if len(t) == 2:
        t[0] = (t[1], None)
    elif len(t) == 4:
        t[0] = (t[1], t[3])
    else:
        size = t[3]
        t[0] = (t[1], [None] * size)  # Default array initialization



def p_expression(t):
    '''expression : ID
                  | FLOAT
                  | INT
                  | CHAR'''
    if isinstance(t[1], str) and not t[1].isdigit():  # If it's an identifier
        var_name = t[1]
        # Look up the variable in the symbol table
        for entry in symbol_table:
            if entry[0] == var_name:  # Found the variable
                if entry[4] is not None:  # Check if the variable is initialized
                    t[0] = entry[4]  # Use the stored value
                else:
                    print(f"Error: Variable '{var_name}' is not initialized.")
                    t[0] = None
                return
        print(f"Error: Variable '{var_name}' not declared.")
        t[0] = None
    else:
        # Handle constants directly
        t[0] = t[1]


def p_type(t):
    '''type : INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | CHAR_TYPE'''
    t[0] = t[1]

def p_array_declaration(t):
    'declaration : type ID LBRACKET NUMBER RBRACKET SEMICOLON'
    var_type, var_name, size = t[1], t[2], t[4]
    if size <= 0:
        print(f"Error: Invalid size for array '{var_name}'.")
    else:
        if var_type == "char":
            value = ['\x00'] * size  # Initialize char array as empty string
        else:
            value = [None] * size  # Default for other types
        add_to_symbol_table(var_name, var_type, "global", value, f"Array of size {size}")
    

def p_array_assignment(t):
    'statement : ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON'
    var_name, index, value = t[1], t[3], t[6]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                entry[4][index] = value
                print(f"Tableau '{var_name}' mis à jour : {entry[4]}")
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")

def p_array_access(t):
    'factor : ID LBRACKET expression RBRACKET'
    var_name, index = t[1], t[3]
    for entry in symbol_table:
        if entry[0] == var_name and isinstance(entry[4], list):  # Vérifie que c'est un tableau
            if isinstance(index, int) and 0 <= index < len(entry[4]):
                t[0] = entry[4][index]
                return
            else:
                print(f"Erreur : Indice hors limites pour '{var_name}'.")
                t[0] = None
                return
    print(f"Erreur : Tableau '{var_name}' non déclaré.")
    t[0] = None

# READ statement
def p_read_statement(t):
    'statement : READ LPAREN ID RPAREN SEMICOLON'
    var_name = t[3]
    for entry in symbol_table:
        if entry[0] == var_name:
            if entry[1] == "char" and entry[6]:  # Check if it's a char array
                value = input(f"Enter string for '{var_name}': ")
                if len(value) > len(entry[4]):  # Check if the input fits in the array
                    print(f"Error: Input string exceeds the size of '{var_name}'.")
                    return
                entry[4][:len(value)] = list(value)  # Store input as a list of chars
                print(f"Updated '{var_name}' with value: {entry[4]}")
            elif entry[1] == "char":  # Single char
                value = input(f"Enter value for '{var_name}': ")
                entry[4] = value[0] if value else None
            else:
                value = input(f"Enter value for '{var_name}': ")
                if entry[1] == "int":
                    entry[4] = int(value)
                elif entry[1] == "float":
                    entry[4] = float(value)
            return
    print(f"Error: Variable '{var_name}' not declared.")


# WRITE statement
def p_write_statement(t):
    'statement : WRITE LPAREN write_content RPAREN SEMICOLON'
    for content in t[3]:
        print(content, end=' ')
    print()

def p_write_content_single(t):
    'write_content : write_item'
    t[0] = [t[1]]

def p_write_content_multiple(t):
    'write_content : write_content COMMA write_item'
    t[0] = t[1] + [t[3]]

def p_write_item(t):
    '''write_item : ID
                  | ID LBRACKET NUMBER RBRACKET'''
    var_name = t[1]
    if len(t) == 2:
        for entry in symbol_table:
            if entry[0] == var_name:
                if entry[6]:  # Check if it's an array
                    t[0] = ''.join(entry[4]).strip('\x00')  # Convert char array to string
                else:
                    t[0] = entry[4]
                return
        print(f"Error: Variable '{var_name}' not declared.")
    else:
        index = t[3]
        for entry in symbol_table:
            if entry[0] == var_name and entry[6]:  # Check if it's an array
                if 0 <= index < len(entry[4]):
                    t[0] = entry[4][index]
                else:
                    print(f"Error: Index {index} out of bounds for '{var_name}'.")
                return
        print(f"Error: Array '{var_name}' not declared.")


# Error handling
def p_error(t):
    print(f"Syntax error at '{t.value}'" if t else "Syntax error at EOF")

# Build the parser
parser = yacc.yacc()

# Display symbol table
def display_symbol_table():
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info", "Is Array"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

# Test the parser
if __name__ == '__main__':
    expressions = [
        "int a;",
        "char tab[]=['a', 'b', 'c', 'd', 'e'];",
        "float c = 3.14;",
        "READ(c);",
        "WRITE(c);",
        "WRITE(tab[2]);",
        "READ(tab);",
        "WRITE(tab);",
    ]
    for stmt in expressions:
        print(f"Parsing statement: {stmt}")
        parser.parse(stmt)
        print("\nSymbol Table:")
        display_symbol_table()
        print("-" * 40)




def test_lexer(input_text):
    """
    Perform both lexical and syntactic analysis on the input text.
    """
    print("Lexical Analysis:")
    lexer.input(input_text)
    tokens = []
    
    while True:
        tok = lexer.token()
        if not tok:
            break
        tokens.append(tok)
        print(tok)
    
    print("\nSyntactic Analysis:")
    try:
        parser.parse(input_text)  # Pass the input text directly to the parser
        print("Parsing completed successfully.")
    except Exception as e:
        print(f"Parsing failed: {e}")

    print("\nSymbol Table:")
    display_symbol_table()
