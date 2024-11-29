
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'statementsAND BOOL_TYPE CHAR CHAR_TYPE COMMA CONST EQ EQUALS FLOAT FLOAT_TYPE GT GTE ID INTEGER INT_TYPE LBRACKET LPAREN LT LTE NEQ NOT NUMBER OR RBRACKET RPAREN SEMICOLONstatements : statement\n| statement statements statement : type ID LBRACKET INTEGER RBRACKET SEMICOLON\n| ID LBRACKET INTEGER RBRACKET EQUALS INTEGER SEMICOLON\n| ID EQUALS expression SEMICOLON\n| type declaration_list SEMICOLON\n| CONST type ID EQUALS expression SEMICOLONdeclaration_list : declaration\n| declaration COMMA declaration_listdeclaration : ID\n| ID EQUALS expression\n| ID LBRACKET INTEGER RBRACKETexpression : ID\n| FLOAT\n| INTEGER\n| CHARtype : INT_TYPE\n| FLOAT_TYPE\n| BOOL_TYPE\n| CHAR_TYPEexpression : termterm : factorfactor : INTEGERexpression : expression OR expressionexpression :  expression  AND expression  expression : expression EQ expression\n| expression NEQ expression\n| expression LT expression\n| expression GT expression\n| expression LTE expression\n| expression GTE expressionexpression : NOT expressioncomparison : LPAREN expression RPARENcomparison : INTEGERfactor : comparisonfactor : IDfactor : ID LBRACKET expression RBRACKETstatement : ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON'
    
_lr_action_items = {'ID':([0,2,3,6,7,8,9,14,15,16,18,19,20,27,30,38,41,42,43,44,45,46,47,48,51,52,57,68,73,75,76,],[4,4,11,-17,-18,-19,-20,21,21,33,21,-6,37,21,21,21,21,21,21,21,21,21,21,21,-5,21,21,-3,-7,-4,-38,]),'CONST':([0,2,19,51,68,73,75,76,],[5,5,-6,-5,-3,-7,-4,-38,]),'INT_TYPE':([0,2,5,19,51,68,73,75,76,],[6,6,6,-6,-5,-3,-7,-4,-38,]),'FLOAT_TYPE':([0,2,5,19,51,68,73,75,76,],[7,7,7,-6,-5,-3,-7,-4,-38,]),'BOOL_TYPE':([0,2,5,19,51,68,73,75,76,],[8,8,8,-6,-5,-3,-7,-4,-38,]),'CHAR_TYPE':([0,2,5,19,51,68,73,75,76,],[9,9,9,-6,-5,-3,-7,-4,-38,]),'$end':([1,2,10,19,51,68,73,75,76,],[0,-1,-2,-6,-5,-3,-7,-4,-38,]),'LBRACKET':([4,11,21,37,],[14,17,38,54,]),'EQUALS':([4,11,33,37,39,40,],[15,18,52,18,56,57,]),'COMMA':([11,13,21,24,25,26,28,29,32,35,37,49,53,58,59,60,61,62,63,64,65,66,70,74,],[-10,20,-13,-14,-16,-21,-22,-35,-15,-11,-10,-32,-12,-24,-25,-26,-27,-28,-29,-30,-31,-33,-37,-12,]),'SEMICOLON':([11,12,13,21,24,25,26,28,29,31,32,35,36,37,49,53,58,59,60,61,62,63,64,65,66,67,70,71,72,74,],[-10,19,-8,-13,-14,-16,-21,-22,-35,51,-15,-11,-9,-10,-32,68,-24,-25,-26,-27,-28,-29,-30,-31,-33,73,-37,75,76,-12,]),'INTEGER':([14,15,17,18,27,30,38,41,42,43,44,45,46,47,48,52,54,56,57,],[22,32,34,32,32,32,32,32,32,32,32,32,32,32,32,32,69,71,32,]),'FLOAT':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,]),'CHAR':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,]),'NOT':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,]),'LPAREN':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,]),'RBRACKET':([21,22,23,24,25,26,28,29,32,34,49,55,58,59,60,61,62,63,64,65,66,69,70,],[-13,39,40,-14,-16,-21,-22,-35,-15,53,-32,70,-24,-25,-26,-27,-28,-29,-30,-31,-33,74,-37,]),'OR':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,41,-14,-16,-21,-22,-35,41,-15,41,41,41,41,41,41,41,41,41,41,41,41,-33,41,-37,41,]),'AND':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,42,-14,-16,-21,-22,-35,42,-15,42,42,42,42,42,42,42,42,42,42,42,42,-33,42,-37,42,]),'EQ':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,43,-14,-16,-21,-22,-35,43,-15,43,43,43,43,43,43,43,43,43,43,43,43,-33,43,-37,43,]),'NEQ':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,44,-14,-16,-21,-22,-35,44,-15,44,44,44,44,44,44,44,44,44,44,44,44,-33,44,-37,44,]),'LT':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,45,-14,-16,-21,-22,-35,45,-15,45,45,45,45,45,45,45,45,45,45,45,45,-33,45,-37,45,]),'GT':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,46,-14,-16,-21,-22,-35,46,-15,46,46,46,46,46,46,46,46,46,46,46,46,-33,46,-37,46,]),'LTE':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,47,-14,-16,-21,-22,-35,47,-15,47,47,47,47,47,47,47,47,47,47,47,47,-33,47,-37,47,]),'GTE':([21,22,23,24,25,26,28,29,31,32,35,49,50,55,58,59,60,61,62,63,64,65,66,67,70,72,],[-13,-15,48,-14,-16,-21,-22,-35,48,-15,48,48,48,48,48,48,48,48,48,48,48,48,-33,48,-37,48,]),'RPAREN':([21,24,25,26,28,29,32,49,50,58,59,60,61,62,63,64,65,66,70,],[-13,-14,-16,-21,-22,-35,-15,-32,66,-24,-25,-26,-27,-28,-29,-30,-31,-33,-37,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'statements':([0,2,],[1,10,]),'statement':([0,2,],[2,2,]),'type':([0,2,5,],[3,3,16,]),'declaration_list':([3,20,],[12,36,]),'declaration':([3,20,],[13,13,]),'expression':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[23,31,35,49,50,55,58,59,60,61,62,63,64,65,67,72,]),'term':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,]),'factor':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,]),'comparison':([14,15,18,27,30,38,41,42,43,44,45,46,47,48,52,57,],[29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> statements","S'",1,None,None,None),
  ('statements -> statement','statements',1,'p_statements','parser.py',124),
  ('statements -> statement statements','statements',2,'p_statements','parser.py',125),
  ('statement -> type ID LBRACKET INTEGER RBRACKET SEMICOLON','statement',6,'p_statement','parser.py',132),
  ('statement -> ID LBRACKET INTEGER RBRACKET EQUALS INTEGER SEMICOLON','statement',7,'p_statement','parser.py',133),
  ('statement -> ID EQUALS expression SEMICOLON','statement',4,'p_statement','parser.py',134),
  ('statement -> type declaration_list SEMICOLON','statement',3,'p_statement','parser.py',135),
  ('statement -> CONST type ID EQUALS expression SEMICOLON','statement',6,'p_statement','parser.py',136),
  ('declaration_list -> declaration','declaration_list',1,'p_declaration_list','parser.py',169),
  ('declaration_list -> declaration COMMA declaration_list','declaration_list',3,'p_declaration_list','parser.py',170),
  ('declaration -> ID','declaration',1,'p_declaration','parser.py',177),
  ('declaration -> ID EQUALS expression','declaration',3,'p_declaration','parser.py',178),
  ('declaration -> ID LBRACKET INTEGER RBRACKET','declaration',4,'p_declaration','parser.py',179),
  ('expression -> ID','expression',1,'p_expression','parser.py',197),
  ('expression -> FLOAT','expression',1,'p_expression','parser.py',198),
  ('expression -> INTEGER','expression',1,'p_expression','parser.py',199),
  ('expression -> CHAR','expression',1,'p_expression','parser.py',200),
  ('type -> INT_TYPE','type',1,'p_type','parser.py',205),
  ('type -> FLOAT_TYPE','type',1,'p_type','parser.py',206),
  ('type -> BOOL_TYPE','type',1,'p_type','parser.py',207),
  ('type -> CHAR_TYPE','type',1,'p_type','parser.py',208),
  ('expression -> term','expression',1,'p_expression_term','parser.py',213),
  ('term -> factor','term',1,'p_term_factor','parser.py',218),
  ('factor -> INTEGER','factor',1,'p_factor_num','parser.py',223),
  ('expression -> expression OR expression','expression',3,'p_expression_or','parser.py',226),
  ('expression -> expression AND expression','expression',3,'p_expression_and','parser.py',231),
  ('expression -> expression EQ expression','expression',3,'p_expression_comparison','parser.py',235),
  ('expression -> expression NEQ expression','expression',3,'p_expression_comparison','parser.py',236),
  ('expression -> expression LT expression','expression',3,'p_expression_comparison','parser.py',237),
  ('expression -> expression GT expression','expression',3,'p_expression_comparison','parser.py',238),
  ('expression -> expression LTE expression','expression',3,'p_expression_comparison','parser.py',239),
  ('expression -> expression GTE expression','expression',3,'p_expression_comparison','parser.py',240),
  ('expression -> NOT expression','expression',2,'p_expression_not','parser.py',255),
  ('comparison -> LPAREN expression RPAREN','comparison',3,'p_comparison_expr','parser.py',259),
  ('comparison -> INTEGER','comparison',1,'p_comparison_number','parser.py',264),
  ('factor -> comparison','factor',1,'p_factor_comparison','parser.py',269),
  ('factor -> ID','factor',1,'p_factor_id','parser.py',278),
  ('factor -> ID LBRACKET expression RBRACKET','factor',4,'p_array_access','parser.py',294),
  ('statement -> ID LBRACKET expression RBRACKET EQUALS expression SEMICOLON','statement',7,'p_array_assignment','parser.py',310),
]
