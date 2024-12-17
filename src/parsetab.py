
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'statementsAND BOOL_TYPE CHAR CHAR_TYPE COMMA CONST EQ EQUALS FLOAT FLOAT_TYPE GT GTE ID INTEGER INT_TYPE LBRACKET LPAREN LT LTE NEQ NOT NUMBER OR RBRACKET RPAREN SEMICOLONstatements : statement\n| statement statements statement : simple_assignment\n| array_declaration\n | declaration_assignment \n| array_assignment\n| type declaration_list SEMICOLON\n| const_declarationconst_declaration : CONST type ID EQUALS expression SEMICOLONarray_declaration : type ID LBRACKET INTEGER RBRACKET SEMICOLONdeclaration_list : declaration\n| declaration COMMA declaration_listdeclaration : ID\n| ID EQUALS expression\n| ID LBRACKET INTEGER RBRACKETexpression : ID\n| FLOAT\n| INTEGER\n| CHARtype : INT_TYPE\n| FLOAT_TYPE\n| BOOL_TYPE\n| CHAR_TYPEexpression : termterm : factorfactor : INTEGERexpression : expression OR expressionexpression :  expression  AND expression  expression : expression EQ expression\n| expression NEQ expression\n| expression LT expression\n| expression GT expression\n| expression LTE expression\n| expression GTE expressionexpression : NOT expressioncomparison : LPAREN expression RPARENcomparison : INTEGERfactor : comparisonfactor : IDfactor : ID LBRACKET expression RBRACKETsimple_assignment : ID EQUALS expression SEMICOLONarray_assignment : ID LBRACKET expression RBRACKET EQUALS expression SEMICOLONdeclaration_assignment : type ID EQUALS expression SEMICOLON'
    
_lr_action_items = {'ID':([0,2,3,4,5,6,7,8,10,11,12,13,19,20,21,22,24,25,32,35,42,43,44,45,46,47,48,49,50,51,55,57,58,70,72,77,79,],[9,9,-3,-4,-5,-6,17,-8,-20,-21,-22,-23,26,26,37,-7,26,41,26,26,26,-41,26,26,26,26,26,26,26,26,26,-43,26,26,-10,-9,-42,]),'INT_TYPE':([0,2,3,4,5,6,8,14,22,43,57,72,77,79,],[10,10,-3,-4,-5,-6,-8,10,-7,-41,-43,-10,-9,-42,]),'FLOAT_TYPE':([0,2,3,4,5,6,8,14,22,43,57,72,77,79,],[11,11,-3,-4,-5,-6,-8,11,-7,-41,-43,-10,-9,-42,]),'BOOL_TYPE':([0,2,3,4,5,6,8,14,22,43,57,72,77,79,],[12,12,-3,-4,-5,-6,-8,12,-7,-41,-43,-10,-9,-42,]),'CHAR_TYPE':([0,2,3,4,5,6,8,14,22,43,57,72,77,79,],[13,13,-3,-4,-5,-6,-8,13,-7,-41,-43,-10,-9,-42,]),'CONST':([0,2,3,4,5,6,8,22,43,57,72,77,79,],[14,14,-3,-4,-5,-6,-8,-7,-41,-43,-10,-9,-42,]),'$end':([1,2,3,4,5,6,8,15,22,43,57,72,77,79,],[0,-1,-3,-4,-5,-6,-8,-2,-7,-41,-43,-10,-9,-42,]),'EQUALS':([9,17,37,41,54,],[19,24,55,58,70,]),'LBRACKET':([9,17,26,41,],[20,23,42,59,]),'SEMICOLON':([16,17,18,26,27,28,29,30,31,33,34,39,40,41,52,56,61,62,63,64,65,66,67,68,69,71,73,75,76,78,],[22,-13,-11,-16,43,-17,-18,-19,-24,-25,-38,57,-12,-13,-35,72,-27,-28,-29,-30,-31,-32,-33,-34,-36,77,-14,-40,79,-15,]),'COMMA':([17,18,26,28,29,30,31,33,34,39,41,52,56,61,62,63,64,65,66,67,68,69,73,75,78,],[-13,25,-16,-17,-18,-19,-24,-25,-38,-14,-13,-35,-15,-27,-28,-29,-30,-31,-32,-33,-34,-36,-14,-40,-15,]),'FLOAT':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,]),'INTEGER':([19,20,23,24,32,35,42,44,45,46,47,48,49,50,51,55,58,59,70,],[29,29,38,29,29,29,29,29,29,29,29,29,29,29,29,29,29,74,29,]),'CHAR':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,]),'NOT':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,]),'LPAREN':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,]),'OR':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,44,-17,-18,-19,-24,-25,-38,44,44,44,44,44,44,44,44,44,44,44,44,44,-36,44,44,-40,44,]),'AND':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,45,-17,-18,-19,-24,-25,-38,45,45,45,45,45,45,45,45,45,45,45,45,45,-36,45,45,-40,45,]),'EQ':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,46,-17,-18,-19,-24,-25,-38,46,46,46,46,46,46,46,46,46,46,46,46,46,-36,46,46,-40,46,]),'NEQ':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,47,-17,-18,-19,-24,-25,-38,47,47,47,47,47,47,47,47,47,47,47,47,47,-36,47,47,-40,47,]),'LT':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,48,-17,-18,-19,-24,-25,-38,48,48,48,48,48,48,48,48,48,48,48,48,48,-36,48,48,-40,48,]),'GT':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,49,-17,-18,-19,-24,-25,-38,49,49,49,49,49,49,49,49,49,49,49,49,49,-36,49,49,-40,49,]),'LTE':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,50,-17,-18,-19,-24,-25,-38,50,50,50,50,50,50,50,50,50,50,50,50,50,-36,50,50,-40,50,]),'GTE':([26,27,28,29,30,31,33,34,36,39,52,53,60,61,62,63,64,65,66,67,68,69,71,73,75,76,],[-16,51,-17,-18,-19,-24,-25,-38,51,51,51,51,51,51,51,51,51,51,51,51,51,-36,51,51,-40,51,]),'RBRACKET':([26,28,29,30,31,33,34,36,38,52,60,61,62,63,64,65,66,67,68,69,74,75,],[-16,-17,-18,-19,-24,-25,-38,54,56,-35,75,-27,-28,-29,-30,-31,-32,-33,-34,-36,78,-40,]),'RPAREN':([26,28,29,30,31,33,34,52,53,61,62,63,64,65,66,67,68,69,75,],[-16,-17,-18,-19,-24,-25,-38,-35,69,-27,-28,-29,-30,-31,-32,-33,-34,-36,-40,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'statements':([0,2,],[1,15,]),'statement':([0,2,],[2,2,]),'simple_assignment':([0,2,],[3,3,]),'array_declaration':([0,2,],[4,4,]),'declaration_assignment':([0,2,],[5,5,]),'array_assignment':([0,2,],[6,6,]),'type':([0,2,14,],[7,7,21,]),'const_declaration':([0,2,],[8,8,]),'declaration_list':([7,25,],[16,40,]),'declaration':([7,25,],[18,18,]),'expression':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[27,36,39,52,53,60,61,62,63,64,65,66,67,68,71,73,76,]),'term':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,]),'factor':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,]),'comparison':([19,20,24,32,35,42,44,45,46,47,48,49,50,51,55,58,70,],[34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> statements","S'",1,None,None,None),
  ('statements -> statement','statements',1,'p_statements','parser.py',127),
  ('statements -> statement statements','statements',2,'p_statements','parser.py',128),
  ('statement -> simple_assignment','statement',1,'p_statement','parser.py',134),
  ('statement -> array_declaration','statement',1,'p_statement','parser.py',135),
  ('statement -> declaration_assignment','statement',1,'p_statement','parser.py',136),
  ('statement -> array_assignment','statement',1,'p_statement','parser.py',137),
  ('statement -> type declaration_list SEMICOLON','statement',3,'p_statement','parser.py',138),
  ('statement -> const_declaration','statement',1,'p_statement','parser.py',139),
  ('const_declaration -> CONST type ID EQUALS expression SEMICOLON','const_declaration',6,'p_const_declaration','parser.py',146),
  ('array_declaration -> type ID LBRACKET INTEGER RBRACKET SEMICOLON','array_declaration',6,'p_array_declaration','parser.py',153),
  ('declaration_list -> declaration','declaration_list',1,'p_declaration_list','parser.py',173),
  ('declaration_list -> declaration COMMA declaration_list','declaration_list',3,'p_declaration_list','parser.py',174),
  ('declaration -> ID','declaration',1,'p_declaration','parser.py',181),
  ('declaration -> ID EQUALS expression','declaration',3,'p_declaration','parser.py',182),
  ('declaration -> ID LBRACKET INTEGER RBRACKET','declaration',4,'p_declaration','parser.py',183),
  ('expression -> ID','expression',1,'p_expression','parser.py',201),
  ('expression -> FLOAT','expression',1,'p_expression','parser.py',202),
  ('expression -> INTEGER','expression',1,'p_expression','parser.py',203),
  ('expression -> CHAR','expression',1,'p_expression','parser.py',204),
  ('type -> INT_TYPE','type',1,'p_type','parser.py',213),
  ('type -> FLOAT_TYPE','type',1,'p_type','parser.py',214),
  ('type -> BOOL_TYPE','type',1,'p_type','parser.py',215),
  ('type -> CHAR_TYPE','type',1,'p_type','parser.py',216),
  ('expression -> term','expression',1,'p_expression_term','parser.py',221),
  ('term -> factor','term',1,'p_term_factor','parser.py',226),
  ('factor -> INTEGER','factor',1,'p_factor_num','parser.py',231),
  ('expression -> expression OR expression','expression',3,'p_expression_or','parser.py',234),
  ('expression -> expression AND expression','expression',3,'p_expression_and','parser.py',239),
  ('expression -> expression EQ expression','expression',3,'p_expression_comparison','parser.py',243),
  ('expression -> expression NEQ expression','expression',3,'p_expression_comparison','parser.py',244),
  ('expression -> expression LT expression','expression',3,'p_expression_comparison','parser.py',245),
  ('expression -> expression GT expression','expression',3,'p_expression_comparison','parser.py',246),
  ('expression -> expression LTE expression','expression',3,'p_expression_comparison','parser.py',247),
  ('expression -> expression GTE expression','expression',3,'p_expression_comparison','parser.py',248),
  ('expression -> NOT expression','expression',2,'p_expression_not','parser.py',263),
  ('comparison -> LPAREN expression RPAREN','comparison',3,'p_comparison_expr','parser.py',267),
  ('comparison -> INTEGER','comparison',1,'p_comparison_number','parser.py',272),
  ('factor -> comparison','factor',1,'p_factor_comparison','parser.py',277),
  ('factor -> ID','factor',1,'p_factor_id','parser.py',286),
  ('factor -> ID LBRACKET expression RBRACKET','factor',4,'p_array_access','parser.py',300),
  ('simple_assignment -> ID EQUALS expression SEMICOLON','simple_assignment',4,'p_variable_assignment','parser.py',319),
  ('array_assignment -> ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON','array_assignment',7,'p_array_assignment','parser.py',345),
  ('declaration_assignment -> type ID EQUALS expression SEMICOLON','declaration_assignment',5,'p_declaration_assignment','parser.py',380),
]
