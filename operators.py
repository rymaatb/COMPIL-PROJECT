import ply.lex as lex

# Token definitions
tokens = (
    'IDF',
    'NUMBER',
    'PLUS',
    'MINUS',
    'MULTIPLY',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'EQ',
    'NEQ',
    'LT',
    'LTE',
    'GT',
    'GTE',
    'AND',
    'OR',
    'NOT'
)



# Structure: [Name, Type, Scope, Memory Address, Value, Additional Info]
symbol_table = []

# Function to add tokens  to the symbol table if it does not already exist
# additional info is the type of var(interger , float...)
def add_symbol(name, symbol_type, scope, memory_address, value=None, additional_info=None):
    if not contains_symbol(name):
        symbol_table.append([
            name, 
            symbol_type, 
            scope, 
            memory_address, 
            value, 
            additional_info
        ])

# Function to check if a tokens exists in the symbol table
def contains_symbol(name):
    return any(row[0] == name for row in symbol_table)

# IDf rule 
def t_IDF(t):
    r'\b[A-Z][a-z0-9]{0,7}\b'
    add_symbol(
        name=t.value,
        symbol_type='Identifier',
        scope='global',  # assuming global scope for this example
        memory_address=hex(id(t.value)),  # using `id` as a mock address
        additional_info='Variable or Constant'
    )
    return t

# Rule for numbers (temporary till the merge of code)
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    add_symbol(
        name=f"const_{t.value}",  # unique key for constants
        symbol_type='Constant',
        scope='N/A',  # Constants may not have a specific scope
        memory_address=hex(id(t.value)),
        value=t.value
    )
    return t


# Rules for operators, adding each to the symbol table
def t_PLUS(t):
    r'\+'
    add_symbol(name='+', symbol_type='Arithmetic Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Addition')
    return t

def t_MINUS(t):
    r'-'
    add_symbol(name='-', symbol_type='Arithmetic Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Subtraction')
    return t

def t_MULTIPLY(t):
    r'\*'
    add_symbol(name='*', symbol_type='Arithmetic Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Multiplication')
    return t

def t_DIVIDE(t):
    r'/'
    add_symbol(name='/', symbol_type='Arithmetic Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Division')
    return t

def t_EQ(t):
    r'=='
    add_symbol(name='==', symbol_type='Comparison Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Equal To')
    return t

def t_NEQ(t):
    r'!='
    add_symbol(name='!=', symbol_type='Comparison Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Not Equal To')
    return t

def t_LT(t):
    r'<'
    add_symbol(name='<', symbol_type='Comparison Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Less Than')
    return t

def t_LTE(t):
    r'<='
    add_symbol(name='<=', symbol_type='Comparison Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Less Than or Equal To')
    return t

def t_GT(t):
    r'>'
    add_symbol(name='>', symbol_type='Comparison Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Greater Than')
    return t

def t_GTE(t):
    r'>='
    add_symbol(name='>=', symbol_type='Comparison Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Greater Than or Equal To')
    return t

def t_AND(t):
    r'&&'
    add_symbol(name='&&', symbol_type='Logical Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Logical AND')
    return t

def t_OR(t):
    r'\|\|'
    add_symbol(name='||', symbol_type='Logical Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Logical OR')
    return t

def t_NOT(t):
    r'!'
    add_symbol(name='!', symbol_type='Logical Operator', scope='global', memory_address=hex(id(t.value)), additional_info='Logical NOT')
    return t


# Ignored characters (spaces and tabs)
t_ignore = ' \t'

# Error handling rule
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Test 
expression_test = """
(3 + Xvar1) * Yvar123 - 10 / Zvar2 && Xvar3 == 1 || !(Yvar4 > 2)
(A + Bvar5) / Cvar6 - 7 * (Xvar3 <= Zvar8) && Yvar4 != 15
Zvar2 + Xvar1 - Yvar123 / (const_100 * Zvar2)
"""


# Process each line in the multi-line expression
print("Tokens and Symbol Table for Multi-Line Expression:\n")

for line_num, line in enumerate(expression_test.strip().splitlines(), start=1):
    print(f"Processing Line {line_num}: {line}")
    lexer.input(line)
    
    # Tokenize each line
    for tok in lexer:
        print(f"  {tok}")

# Print the symbol table as a matrix 
print("\nFinal Symbol Table (Matrix Format):")
print(f"{'Name':<10} {'Type':<10} {'Scope':<10} {'Memory Address':<18} {'Value':<10} {'Additional Info':<20}")
print("-" * 80)
for row in symbol_table:
     
    print(f"{row[0]:<10} {row[1]:<10} {row[2]:<10} {row[3]:<18} {str(row[4]) if row[4] is not None else '':<10} {row[5] if row[5] is not None else '':<20}")


