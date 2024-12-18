
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND BOOL BOOL_TYPE CHAR CHAR_TYPE COLON COMMA COMMENT CONST DECLARATION DIVIDE ELSE EQ EQUALS FALSE FLOAT FLOAT_TYPE FOR GT GTE ID IF INSTRUCTION INTEGER INT_TYPE LBRACKET LPAREN LT LTE MINUS MULTIPLY NEQ NOT NUMBER OR PLUS RBRACKET RPAREN SEMICOLON TRUE VAR_GLOBALstatement : type declaration_list SEMICOLON\n| CONST type ID EQUALS expression SEMICOLONdeclaration_list : declaration\n| declaration COMMA declaration_listdeclaration : ID\n| ID EQUALS expressionstatement : ID EQUALS expression SEMICOLONtype : INT_TYPE\n| FLOAT_TYPE\n| BOOL_TYPE\n| CHAR_TYPEfactor : INTEGER\n| FLOAT\n| CHAR\n| BOOL\n| MINUS factor\n| PLUS factor\n| LPAREN INTEGER RPAREN\n| LPAREN FLOAT RPAREN\n| LPAREN MINUS FLOAT RPAREN\nstatements : statement\n| statement statements statement : simple_assignment\n| array_declarationTab\n  | declaration_assignment \n| array_assignment\n| type declarationTab_listTab SEMICOLON\n| const_declarationTabconst_declarationTab : CONST type ID EQUALS expressionTab SEMICOLONarray_declarationTab : type ID LBRACKET INTEGER RBRACKET SEMICOLONdeclarationTab_listTab : declarationTab\n| declarationTab COMMA declarationTab_listTabdeclarationTab : ID\n| ID EQUALS expressionTab\n| ID LBRACKET INTEGER RBRACKETexpressionTab : ID\n| FLOAT\n| INTEGER\n| CHARsimple_assignment : ID EQUALS factor SEMICOLONfactor : ID LBRACKET expressionTab RBRACKETsimple_assignment : ID EQUALS expressionTab SEMICOLONarray_assignment : ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLONdeclaration_assignment : type ID EQUALS expressionTab SEMICOLONexpression : expression OR term\n| termterm : term AND factor \n| factorfactor : ID\n| TRUE\n| FALSE\n| NOT ID'
    
_lr_action_items = {'CONST':([0,],[3,]),'ID':([0,2,10,11,12,13,19,20,21,25,26,27,38,39,43,61,62,64,67,81,82,92,],[4,16,-8,-9,-10,-11,28,29,44,50,58,60,72,72,77,50,44,72,72,72,44,44,]),'INT_TYPE':([0,3,],[10,10,]),'FLOAT_TYPE':([0,3,],[11,11,]),'BOOL_TYPE':([0,3,],[12,12,]),'CHAR_TYPE':([0,3,],[13,13,]),'$end':([1,5,6,7,8,9,22,23,63,65,66,80,93,96,97,102,],[0,-23,-24,-25,-26,-28,-1,-27,-7,-40,-42,-44,-30,-2,-29,-43,]),'EQUALS':([4,16,28,58,60,78,],[20,25,61,81,82,92,]),'LBRACKET':([4,16,29,50,60,72,],[21,24,62,62,83,62,]),'SEMICOLON':([14,15,16,17,18,29,30,31,32,33,34,35,36,37,41,42,44,46,47,48,50,51,52,53,54,55,56,57,58,59,60,68,69,70,71,72,73,77,79,84,85,87,88,89,90,94,98,99,100,101,],[22,23,-5,-3,-31,-36,63,65,66,-46,-12,-13,-14,-15,-50,-51,-36,-37,-38,-39,-36,80,-6,-13,-12,-14,-48,-4,-5,-32,-33,-16,-12,-13,-14,-49,-17,-52,93,96,97,-45,-47,-18,-19,-34,-41,-20,102,-35,]),'COMMA':([16,17,18,33,37,41,42,44,46,47,48,50,51,52,53,54,55,56,58,60,68,69,70,71,72,73,77,79,87,88,89,90,94,98,99,101,],[-5,26,27,-46,-15,-50,-51,-36,-37,-38,-39,-36,-34,-6,-13,-12,-14,-48,-5,-33,-16,-12,-13,-14,-49,-17,-52,-35,-45,-47,-18,-19,-34,-41,-20,-35,]),'INTEGER':([20,21,24,25,38,39,40,61,62,64,67,81,82,83,92,],[34,47,49,54,69,69,74,54,47,69,69,69,47,95,47,]),'FLOAT':([20,21,25,38,39,40,61,62,64,67,76,81,82,92,],[35,46,53,70,70,75,53,46,70,70,91,70,46,46,]),'CHAR':([20,21,25,38,39,61,62,64,67,81,82,92,],[36,48,55,71,71,55,48,71,71,71,48,48,]),'BOOL':([20,25,38,39,61,64,67,81,],[37,37,37,37,37,37,37,37,]),'MINUS':([20,25,38,39,40,61,64,67,81,],[38,38,38,38,76,38,38,38,38,]),'PLUS':([20,25,38,39,61,64,67,81,],[39,39,39,39,39,39,39,39,]),'LPAREN':([20,25,38,39,61,64,67,81,],[40,40,40,40,40,40,40,40,]),'TRUE':([20,25,38,39,61,64,67,81,],[41,41,41,41,41,41,41,41,]),'FALSE':([20,25,38,39,61,64,67,81,],[42,42,42,42,42,42,42,42,]),'NOT':([20,25,38,39,61,64,67,81,],[43,43,43,43,43,43,43,43,]),'AND':([29,31,33,34,35,36,37,41,42,50,53,54,55,56,68,69,70,71,72,73,77,87,88,89,90,98,99,],[-49,-48,67,-12,-13,-14,-15,-50,-51,-49,-13,-12,-14,-48,-16,-12,-13,-14,-49,-17,-52,67,-47,-18,-19,-41,-20,]),'OR':([29,30,31,33,34,35,36,37,41,42,50,52,53,54,55,56,68,69,70,71,72,73,77,84,87,88,89,90,98,99,],[-49,64,-48,-46,-12,-13,-14,-15,-50,-51,-49,64,-13,-12,-14,-48,-16,-12,-13,-14,-49,-17,-52,64,-45,-47,-18,-19,-41,-20,]),'RBRACKET':([44,45,46,47,48,49,86,95,],[-36,78,-37,-38,-39,79,98,101,]),'RPAREN':([74,75,91,],[89,90,99,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'statement':([0,],[1,]),'type':([0,3,],[2,19,]),'simple_assignment':([0,],[5,]),'array_declarationTab':([0,],[6,]),'declaration_assignment':([0,],[7,]),'array_assignment':([0,],[8,]),'const_declarationTab':([0,],[9,]),'declaration_list':([2,26,],[14,57,]),'declarationTab_listTab':([2,27,],[15,59,]),'declaration':([2,26,],[17,17,]),'declarationTab':([2,27,],[18,18,]),'expression':([20,25,61,81,],[30,52,84,52,]),'factor':([20,25,38,39,61,64,67,81,],[31,56,68,73,56,56,88,56,]),'expressionTab':([20,21,25,61,62,82,92,],[32,45,51,85,86,94,100,]),'term':([20,25,61,64,81,],[33,33,33,87,33,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> statement","S'",1,None,None,None),
  ('statement -> type declaration_list SEMICOLON','statement',3,'p_statement_declaration','main.py',168),
  ('statement -> CONST type ID EQUALS expression SEMICOLON','statement',6,'p_statement_declaration','main.py',169),
  ('declaration_list -> declaration','declaration_list',1,'p_declaration_list','main.py',203),
  ('declaration_list -> declaration COMMA declaration_list','declaration_list',3,'p_declaration_list','main.py',204),
  ('declaration -> ID','declaration',1,'p_declaration','main.py',214),
  ('declaration -> ID EQUALS expression','declaration',3,'p_declaration','main.py',215),
  ('statement -> ID EQUALS expression SEMICOLON','statement',4,'p_statement_assignment','main.py',224),
  ('type -> INT_TYPE','type',1,'p_type','main.py',233),
  ('type -> FLOAT_TYPE','type',1,'p_type','main.py',234),
  ('type -> BOOL_TYPE','type',1,'p_type','main.py',235),
  ('type -> CHAR_TYPE','type',1,'p_type','main.py',236),
  ('factor -> INTEGER','factor',1,'p_factor_number','main.py',240),
  ('factor -> FLOAT','factor',1,'p_factor_number','main.py',241),
  ('factor -> CHAR','factor',1,'p_factor_number','main.py',242),
  ('factor -> BOOL','factor',1,'p_factor_number','main.py',243),
  ('factor -> MINUS factor','factor',2,'p_factor_number','main.py',244),
  ('factor -> PLUS factor','factor',2,'p_factor_number','main.py',245),
  ('factor -> LPAREN INTEGER RPAREN','factor',3,'p_factor_number','main.py',246),
  ('factor -> LPAREN FLOAT RPAREN','factor',3,'p_factor_number','main.py',247),
  ('factor -> LPAREN MINUS FLOAT RPAREN','factor',4,'p_factor_number','main.py',248),
  ('statements -> statement','statements',1,'p_statements','main.py',269),
  ('statements -> statement statements','statements',2,'p_statements','main.py',270),
  ('statement -> simple_assignment','statement',1,'p_statement','main.py',277),
  ('statement -> array_declarationTab','statement',1,'p_statement','main.py',278),
  ('statement -> declaration_assignment','statement',1,'p_statement','main.py',279),
  ('statement -> array_assignment','statement',1,'p_statement','main.py',280),
  ('statement -> type declarationTab_listTab SEMICOLON','statement',3,'p_statement','main.py',281),
  ('statement -> const_declarationTab','statement',1,'p_statement','main.py',282),
  ('const_declarationTab -> CONST type ID EQUALS expressionTab SEMICOLON','const_declarationTab',6,'p_const_declarationTab','main.py',285),
  ('array_declarationTab -> type ID LBRACKET INTEGER RBRACKET SEMICOLON','array_declarationTab',6,'p_array_declarationTab','main.py',291),
  ('declarationTab_listTab -> declarationTab','declarationTab_listTab',1,'p_declarationTab_listTab','main.py',304),
  ('declarationTab_listTab -> declarationTab COMMA declarationTab_listTab','declarationTab_listTab',3,'p_declarationTab_listTab','main.py',305),
  ('declarationTab -> ID','declarationTab',1,'p_declarationTab','main.py',312),
  ('declarationTab -> ID EQUALS expressionTab','declarationTab',3,'p_declarationTab','main.py',313),
  ('declarationTab -> ID LBRACKET INTEGER RBRACKET','declarationTab',4,'p_declarationTab','main.py',314),
  ('expressionTab -> ID','expressionTab',1,'p_expressionTab','main.py',327),
  ('expressionTab -> FLOAT','expressionTab',1,'p_expressionTab','main.py',328),
  ('expressionTab -> INTEGER','expressionTab',1,'p_expressionTab','main.py',329),
  ('expressionTab -> CHAR','expressionTab',1,'p_expressionTab','main.py',330),
  ('simple_assignment -> ID EQUALS factor SEMICOLON','simple_assignment',4,'p_access_and_assignment','main.py',335),
  ('factor -> ID LBRACKET expressionTab RBRACKET','factor',4,'p_array_access','main.py',380),
  ('simple_assignment -> ID EQUALS expressionTab SEMICOLON','simple_assignment',4,'p_variable_assignment','main.py',397),
  ('array_assignment -> ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLON','array_assignment',7,'p_array_assignment','main.py',436),
  ('declaration_assignment -> type ID EQUALS expressionTab SEMICOLON','declaration_assignment',5,'p_declaration_assignment','main.py',464),
  ('expression -> expression OR term','expression',3,'p_expression_el','main.py',502),
  ('expression -> term','expression',1,'p_expression_el','main.py',503),
  ('term -> term AND factor','term',3,'p_term_tl','main.py',513),
  ('term -> factor','term',1,'p_term_tl','main.py',514),
  ('factor -> ID','factor',1,'p_factor_fl','main.py',524),
  ('factor -> TRUE','factor',1,'p_factor_fl','main.py',525),
  ('factor -> FALSE','factor',1,'p_factor_fl','main.py',526),
  ('factor -> NOT ID','factor',2,'p_factor_fl','main.py',527),
]
