symbol_table = []

def find_in_symbol_table(name, scope):
    """Find a variable in the symbol table by name and scope."""
    for entry in symbol_table:
        if entry[0] == name and entry[2] == scope:
            return entry  # Return the entry for better testing feedback
    return None  # Return None if no entry is found

def add_to_symbol_table(name, var_type, scope, value=None, additional_info=None):
    """Add a new variable to the symbol table or issue a warning if already declared."""
    if find_in_symbol_table(name, scope):
        print(f"Warning: Variable '{name}' already declared in scope '{scope}'.")
    else:
        # Memory address is derived from the variable's id
        memory_address = hex(id(name))
        is_array = isinstance(value, list)
        entry = [name, var_type, scope, memory_address, value, additional_info, is_array]
        symbol_table.append(entry)

def update_symbol_table(name, value):
    """Update the value of an existing variable in the symbol table."""
    entry = find_in_symbol_table(name, None)  # Can update in any scope
    if entry:
        entry[4] = value  # Update the value field
        print(f"Updated '{name}' with new value: {value}")
    else:
        print(f"Error: Variable '{name}' not found in the symbol table.")

def display_symbol_table():
    """Display the contents of the symbol table in a readable format."""
    from tabulate import tabulate
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info", "Is Array"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))

