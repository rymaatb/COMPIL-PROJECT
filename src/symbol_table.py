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

def display_symbol_table():
    from tabulate import tabulate
    headers = ["Name", "Type", "Scope", "Memory Address", "Value", "Additional Info", "Is Array"]
    print(tabulate(symbol_table, headers=headers, tablefmt="grid"))
