import ply.lex as lex
import ply.yacc as yacc
from tkinter import *
from tkinter import scrolledtext, messagebox
from graphviz import Digraph

# --------------------------------------
# 1. Lexical Analysis
# --------------------------------------

# Reserved words
reserved = {
    'VAR_GLOBAL': 'VAR_GLOBAL',
    'DECLARATION': 'DECLARATION',
    'INSTRUCTION': 'INSTRUCTION',
    'CONST': 'CONST',
    'INTEGER': 'INTEGER',
    'FLOAT': 'FLOAT',
    'CHAR': 'CHAR',
    'IF': 'IF',
    'ELSE': 'ELSE',
    'FOR': 'FOR',
    'READ': 'READ',
    'WRITE': 'WRITE'
}

tokens = [
    'ID', 'NUMBER', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE',
    'ASSIGN', 'SEMICOLON', 'LPAREN', 'RPAREN', 'LSQUARE', 'RSQUARE',
    'GT', 'LT', 'EQ', 'NEQ', 'GTE', 'LTE', 'LBRACE', 'RBRACE'
] + list(reserved.values())

# Token definitions
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_ASSIGN = r'='
t_SEMICOLON = r';'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LSQUARE = r'\['
t_RSQUARE = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_GT = r'>'
t_LT = r'<'
t_EQ = r'=='
t_NEQ = r'!='
t_GTE = r'>='
t_LTE = r'<='


def t_COMMENT(t):
    r'%%.*'
    pass  # Ignore comments


def t_ID(t):
    r'[A-Z][a-zA-Z0-9]{0,7}'
    t.type = reserved.get(t.value, 'ID')  # Reserved words
    return t


def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t


t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print(f"Illegal character {t.value[0]} at line {t.lexer.lineno}")
    t.lexer.skip(1)


lexer = lex.lex()

# --------------------------------------
# 2. Syntax Analysis
# --------------------------------------

# Grammar rules
def p_program(p):
    '''program : VAR_GLOBAL declarations instructions'''
    p[0] = ('program', p[2], p[3])


def p_declarations(p):
    '''declarations : declarations declaration
                    | declaration'''
    p[0] = p[1:] if len(p) > 2 else [p[1]]


def p_declaration(p):
    '''declaration : INTEGER ID SEMICOLON
                   | FLOAT ID SEMICOLON
                   | CHAR ID SEMICOLON
                   | CONST INTEGER ID ASSIGN NUMBER SEMICOLON
                   | CONST FLOAT ID ASSIGN NUMBER SEMICOLON
                   | CONST CHAR ID ASSIGN ID SEMICOLON
                   | INTEGER ID LSQUARE NUMBER RSQUARE SEMICOLON
                   | FLOAT ID LSQUARE NUMBER RSQUARE SEMICOLON
                   | CHAR ID LSQUARE NUMBER RSQUARE SEMICOLON'''
    if len(p) == 6 and p[1] == 'CONST':
        symbol_table.add(p[3], p[2], value=p[5], is_const=True)
    elif len(p) == 6:
        symbol_table.add(p[2], p[1])
    elif len(p) == 8:  # Array declaration
        symbol_table.add(p[2], p[1], size=p[4])
    p[0] = ('declaration', p[1:])


def p_instructions(p):
    '''instructions : instructions instruction
                    | instruction'''
    p[0] = p[1:] if len(p) > 2 else [p[1]]


def p_instruction(p):
    '''instruction : ID ASSIGN expression SEMICOLON
                   | ID LSQUARE expression RSQUARE ASSIGN expression SEMICOLON
                   | IF LPAREN condition RPAREN block
                   | FOR LPAREN ID ASSIGN expression SEMICOLON condition SEMICOLON expression RPAREN block
                   | READ LPAREN ID RPAREN SEMICOLON
                   | WRITE LPAREN expression RPAREN SEMICOLON'''
    if p[1] == 'READ':
        quadruples.append(('READ', None, None, p[3]))
    elif p[1] == 'WRITE':
        quadruples.append(('WRITE', None, None, p[3]))
    elif len(p) == 5:
        quadruples.append(('=', p[3], None, p[1]))
    elif len(p) == 8:
        temp = new_temp()
        quadruples.append(('ARRAY_ASSIGN', p[1], p[3], temp))
        quadruples.append(('=', p[6], None, temp))
    p[0] = p[1:]


def p_block(p):
    '''block : LBRACE instructions RBRACE'''
    p[0] = ('block', p[2])


def p_expression(p):
    '''expression : expression PLUS term
                  | expression MINUS term
                  | term'''
    if len(p) == 4:
        temp = new_temp()
        quadruples.append((p[2], p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]


def p_term(p):
    '''term : term MULTIPLY factor
            | term DIVIDE factor
            | factor'''
    if len(p) == 4:
        temp = new_temp()
        quadruples.append((p[2], p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]


def p_factor(p):
    '''factor : NUMBER
              | ID
              | ID LSQUARE expression RSQUARE'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 5:  # Array element
        temp = new_temp()
        quadruples.append(('ARRAY_ACCESS', p[1], p[3], temp))
        p[0] = temp


def p_condition(p):
    '''condition : expression GT expression
                 | expression LT expression
                 | expression EQ expression
                 | expression NEQ expression'''
    temp = new_temp()
    quadruples.append((p[2], p[1], p[3], temp))
    p[0] = temp


def p_error(p):
    print(f"Syntax error at {p.value!r}")


parser = yacc.yacc()

# --------------------------------------
# 3. Symbol Table Management
# --------------------------------------

class SymbolTable:
    def __init__(self):
        self.table = {}

    def add(self, name, var_type, value=None, is_const=False, size=None):
        if name in self.table:
            raise ValueError(f"Variable {name} already declared.")
        self.table[name] = {'type': var_type, 'value': value, 'is_const': is_const, 'size': size}

    def update(self, name, value):
        if name not in self.table:
            raise ValueError(f"Variable {name} not declared.")
        if self.table[name]['is_const']:
            raise ValueError(f"Cannot update constant {name}.")
        self.table[name]['value'] = value

    def delete(self, name):
        if name not in self.table:
            raise ValueError(f"Variable {name} not declared.")
        del self.table[name]

    def __repr__(self):
        return str(self.table)


symbol_table = SymbolTable()

# --------------------------------------
# 4. Intermediate Code Generation
# --------------------------------------

quadruples = []
temp_counter = 0


def new_temp():
    global temp_counter
    temp_counter += 1
    return f"T{temp_counter}"


# --------------------------------------
# 5. GUI Implementation
# --------------------------------------

class CompilerGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Mini-Compiler for MinING")
        
        # Input Code Section
        Label(root, text="Input Code:").grid(row=0, column=0, padx=10, pady=10)
        self.code_input = scrolledtext.ScrolledText(root, height=15, width=70)
        self.code_input.grid(row=1, column=0, padx=10, pady=5, columnspan=3)
        
        # Action Buttons
        Button(root, text="Run Compiler", command=self.run_compiler).grid(row=2, column=0, padx=10, pady=10)
        Button(root, text="Visualize Syntax Tree", command=self.visualize_syntax_tree).grid(row=2, column=1, padx=10, pady=10)
        Button(root, text="Show Symbol Table", command=self.show_symbol_table).grid(row=2, column=2, padx=10, pady=10)
        
        # Output Section
        Label(root, text="Compiler Output:").grid(row=3, column=0, padx=10, pady=10)
        self.output_display = scrolledtext.ScrolledText(root, height=15, width=70)
        self.output_display.grid(row=4, column=0, padx=10, pady=5, columnspan=3)
        
    def run_compiler(self):
        """Compile the code and display outputs."""
        code = self.code_input.get("1.0", END).strip()
        if not code:
            messagebox.showerror("Error", "No code provided!")
            return
        
        try:
            lexer.input(code)
            tokens = list(lexer)
            self.output_display.insert(END, "Tokens:\n")
            for token in tokens:
                self.output_display.insert(END, f"{token}\n")
            
            global result
            result = parser.parse(code)
            self.output_display.insert(END, "\nSymbol Table:\n")
            self.output_display.insert(END, f"{symbol_table}\n")
            
            self.output_display.insert(END, "\nIntermediate Code (Quadruples):\n")
            for quad in quadruples:
                self.output_display.insert(END, f"{quad}\n")
        except Exception as e:
            messagebox.showerror("Error", str(e))
    
    def visualize_syntax_tree(self):
        """Visualize the syntax tree using graphviz."""
        dot = Digraph()
        
        # Add nodes and edges for the syntax tree
        # Assuming result is a parsed tree structure from yacc
        def add_nodes_edges(tree, parent=None):
            if isinstance(tree, tuple):
                node_id = str(id(tree))
                dot.node(node_id, tree[0])
                if parent:
                    dot.edge(parent, node_id)
                for child in tree[1:]:
                    add_nodes_edges(child, node_id)
        
        # Assuming `result` is the parsed syntax tree from yacc
        if 'result' in globals() and result:
            add_nodes_edges(result)
            dot.render('syntax_tree', format='png', cleanup=True)
            messagebox.showinfo("Visualization", "Syntax tree rendered as 'syntax_tree.png'.")
        else:
            messagebox.showerror("Error", "No syntax tree to visualize!")
    
    def show_symbol_table(self):
        """Show the symbol table."""
        self.output_display.insert(END, "\nSymbol Table:\n")
        self.output_display.insert(END, f"{symbol_table}\n")


# Initialize the GUI
root = Tk()
app = CompilerGUI(root)
root.mainloop()
