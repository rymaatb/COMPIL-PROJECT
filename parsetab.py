
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'ASSIGN CHAR CONST DECLARATION DIVIDE ELSE EQ FLOAT FOR GT GTE ID IF INSTRUCTION INTEGER LBRACE LPAREN LSQUARE LT LTE MINUS MULTIPLY NEQ NUMBER PLUS RBRACE READ RPAREN RSQUARE SEMICOLON VAR_GLOBAL WRITEprogram : VAR_GLOBAL declarations instructionsdeclarations : declarations declaration\n                    | declarationdeclaration : INTEGER ID SEMICOLON\n                   | FLOAT ID SEMICOLON\n                   | CHAR ID SEMICOLON\n                   | CONST INTEGER ID ASSIGN NUMBER SEMICOLON\n                   | CONST FLOAT ID ASSIGN NUMBER SEMICOLON\n                   | CONST CHAR ID ASSIGN ID SEMICOLON\n                   | INTEGER ID LSQUARE NUMBER RSQUARE SEMICOLON\n                   | FLOAT ID LSQUARE NUMBER RSQUARE SEMICOLON\n                   | CHAR ID LSQUARE NUMBER RSQUARE SEMICOLONinstructions : instructions instruction\n                    | instructioninstruction : ID ASSIGN expression SEMICOLON\n                   | ID LSQUARE expression RSQUARE ASSIGN expression SEMICOLON\n                   | IF LPAREN condition RPAREN block\n                   | FOR LPAREN ID ASSIGN expression SEMICOLON condition SEMICOLON expression RPAREN block\n                   | READ LPAREN ID RPAREN SEMICOLON\n                   | WRITE LPAREN expression RPAREN SEMICOLONblock : LBRACE instructions RBRACEexpression : expression PLUS term\n                  | expression MINUS term\n                  | termterm : term MULTIPLY factor\n            | term DIVIDE factor\n            | factorfactor : NUMBER\n              | ID\n              | ID LSQUARE expression RSQUAREcondition : expression GT expression\n                 | expression LT expression\n                 | expression EQ expression\n                 | expression NEQ expression'
    
_lr_action_items = {'VAR_GLOBAL':([0,],[2,]),'$end':([1,9,11,23,57,83,90,91,102,103,108,],[0,-1,-14,-13,-15,-17,-19,-20,-16,-21,-18,]),'INTEGER':([2,3,4,8,10,30,32,34,92,93,94,95,96,97,],[5,5,-3,20,-2,-4,-5,-6,-10,-11,-12,-7,-8,-9,]),'FLOAT':([2,3,4,8,10,30,32,34,92,93,94,95,96,97,],[6,6,-3,21,-2,-4,-5,-6,-10,-11,-12,-7,-8,-9,]),'CHAR':([2,3,4,8,10,30,32,34,92,93,94,95,96,97,],[7,7,-3,22,-2,-4,-5,-6,-10,-11,-12,-7,-8,-9,]),'CONST':([2,3,4,10,30,32,34,92,93,94,95,96,97,],[8,8,-3,-2,-4,-5,-6,-10,-11,-12,-7,-8,-9,]),'ID':([3,4,5,6,7,9,10,11,20,21,22,23,24,25,26,27,28,29,30,32,34,55,56,57,58,59,60,61,64,65,66,67,68,82,83,84,90,91,92,93,94,95,96,97,100,101,102,103,105,108,],[12,-3,17,18,19,12,-2,-14,36,37,38,-13,39,39,39,47,48,39,-4,-5,-6,76,39,-15,39,39,39,39,39,39,39,39,39,39,-17,12,-19,-20,-10,-11,-12,-7,-8,-9,12,39,-16,-21,39,-18,]),'IF':([3,4,9,10,11,23,30,32,34,57,83,84,90,91,92,93,94,95,96,97,100,102,103,108,],[13,-3,13,-2,-14,-13,-4,-5,-6,-15,-17,13,-19,-20,-10,-11,-12,-7,-8,-9,13,-16,-21,-18,]),'FOR':([3,4,9,10,11,23,30,32,34,57,83,84,90,91,92,93,94,95,96,97,100,102,103,108,],[14,-3,14,-2,-14,-13,-4,-5,-6,-15,-17,14,-19,-20,-10,-11,-12,-7,-8,-9,14,-16,-21,-18,]),'READ':([3,4,9,10,11,23,30,32,34,57,83,84,90,91,92,93,94,95,96,97,100,102,103,108,],[15,-3,15,-2,-14,-13,-4,-5,-6,-15,-17,15,-19,-20,-10,-11,-12,-7,-8,-9,15,-16,-21,-18,]),'WRITE':([3,4,9,10,11,23,30,32,34,57,83,84,90,91,92,93,94,95,96,97,100,102,103,108,],[16,-3,16,-2,-14,-13,-4,-5,-6,-15,-17,16,-19,-20,-10,-11,-12,-7,-8,-9,16,-16,-21,-18,]),'RBRACE':([11,23,57,83,90,91,100,102,103,108,],[-14,-13,-15,-17,-19,-20,103,-16,-21,-18,]),'ASSIGN':([12,36,37,38,47,62,],[24,53,54,55,68,82,]),'LSQUARE':([12,17,18,19,39,],[25,31,33,35,56,]),'LPAREN':([13,14,15,16,],[26,27,28,29,]),'SEMICOLON':([17,18,19,39,40,41,42,43,69,70,71,72,73,74,75,76,78,79,80,81,85,86,87,88,89,98,99,104,],[30,32,34,-29,57,-24,-27,-28,90,91,92,93,94,95,96,97,-22,-23,-25,-26,-31,-32,-33,-34,101,-30,102,105,]),'NUMBER':([24,25,26,29,31,33,35,53,54,56,58,59,60,61,64,65,66,67,68,82,101,105,],[43,43,43,43,50,51,52,74,75,43,43,43,43,43,43,43,43,43,43,43,43,43,]),'MULTIPLY':([39,41,42,43,78,79,80,81,98,],[-29,60,-27,-28,60,60,-25,-26,-30,]),'DIVIDE':([39,41,42,43,78,79,80,81,98,],[-29,61,-27,-28,61,61,-25,-26,-30,]),'PLUS':([39,40,41,42,43,44,46,49,77,78,79,80,81,85,86,87,88,89,98,99,106,],[-29,58,-24,-27,-28,58,58,58,58,-22,-23,-25,-26,58,58,58,58,58,-30,58,58,]),'MINUS':([39,40,41,42,43,44,46,49,77,78,79,80,81,85,86,87,88,89,98,99,106,],[-29,59,-24,-27,-28,59,59,59,59,-22,-23,-25,-26,59,59,59,59,59,-30,59,59,]),'RSQUARE':([39,41,42,43,44,50,51,52,77,78,79,80,81,98,],[-29,-24,-27,-28,62,71,72,73,98,-22,-23,-25,-26,-30,]),'GT':([39,41,42,43,46,78,79,80,81,98,],[-29,-24,-27,-28,64,-22,-23,-25,-26,-30,]),'LT':([39,41,42,43,46,78,79,80,81,98,],[-29,-24,-27,-28,65,-22,-23,-25,-26,-30,]),'EQ':([39,41,42,43,46,78,79,80,81,98,],[-29,-24,-27,-28,66,-22,-23,-25,-26,-30,]),'NEQ':([39,41,42,43,46,78,79,80,81,98,],[-29,-24,-27,-28,67,-22,-23,-25,-26,-30,]),'RPAREN':([39,41,42,43,45,48,49,78,79,80,81,85,86,87,88,98,106,],[-29,-24,-27,-28,63,69,70,-22,-23,-25,-26,-31,-32,-33,-34,-30,107,]),'LBRACE':([63,107,],[84,84,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'declarations':([2,],[3,]),'declaration':([2,3,],[4,10,]),'instructions':([3,84,],[9,100,]),'instruction':([3,9,84,100,],[11,23,11,23,]),'expression':([24,25,26,29,56,64,65,66,67,68,82,101,105,],[40,44,46,49,77,85,86,87,88,89,99,46,106,]),'term':([24,25,26,29,56,58,59,64,65,66,67,68,82,101,105,],[41,41,41,41,41,78,79,41,41,41,41,41,41,41,41,]),'factor':([24,25,26,29,56,58,59,60,61,64,65,66,67,68,82,101,105,],[42,42,42,42,42,42,42,80,81,42,42,42,42,42,42,42,42,]),'condition':([26,101,],[45,104,]),'block':([63,107,],[83,108,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> VAR_GLOBAL declarations instructions','program',3,'p_program','new.py',92),
  ('declarations -> declarations declaration','declarations',2,'p_declarations','new.py',97),
  ('declarations -> declaration','declarations',1,'p_declarations','new.py',98),
  ('declaration -> INTEGER ID SEMICOLON','declaration',3,'p_declaration','new.py',103),
  ('declaration -> FLOAT ID SEMICOLON','declaration',3,'p_declaration','new.py',104),
  ('declaration -> CHAR ID SEMICOLON','declaration',3,'p_declaration','new.py',105),
  ('declaration -> CONST INTEGER ID ASSIGN NUMBER SEMICOLON','declaration',6,'p_declaration','new.py',106),
  ('declaration -> CONST FLOAT ID ASSIGN NUMBER SEMICOLON','declaration',6,'p_declaration','new.py',107),
  ('declaration -> CONST CHAR ID ASSIGN ID SEMICOLON','declaration',6,'p_declaration','new.py',108),
  ('declaration -> INTEGER ID LSQUARE NUMBER RSQUARE SEMICOLON','declaration',6,'p_declaration','new.py',109),
  ('declaration -> FLOAT ID LSQUARE NUMBER RSQUARE SEMICOLON','declaration',6,'p_declaration','new.py',110),
  ('declaration -> CHAR ID LSQUARE NUMBER RSQUARE SEMICOLON','declaration',6,'p_declaration','new.py',111),
  ('instructions -> instructions instruction','instructions',2,'p_instructions','new.py',122),
  ('instructions -> instruction','instructions',1,'p_instructions','new.py',123),
  ('instruction -> ID ASSIGN expression SEMICOLON','instruction',4,'p_instruction','new.py',128),
  ('instruction -> ID LSQUARE expression RSQUARE ASSIGN expression SEMICOLON','instruction',7,'p_instruction','new.py',129),
  ('instruction -> IF LPAREN condition RPAREN block','instruction',5,'p_instruction','new.py',130),
  ('instruction -> FOR LPAREN ID ASSIGN expression SEMICOLON condition SEMICOLON expression RPAREN block','instruction',11,'p_instruction','new.py',131),
  ('instruction -> READ LPAREN ID RPAREN SEMICOLON','instruction',5,'p_instruction','new.py',132),
  ('instruction -> WRITE LPAREN expression RPAREN SEMICOLON','instruction',5,'p_instruction','new.py',133),
  ('block -> LBRACE instructions RBRACE','block',3,'p_block','new.py',148),
  ('expression -> expression PLUS term','expression',3,'p_expression','new.py',153),
  ('expression -> expression MINUS term','expression',3,'p_expression','new.py',154),
  ('expression -> term','expression',1,'p_expression','new.py',155),
  ('term -> term MULTIPLY factor','term',3,'p_term','new.py',165),
  ('term -> term DIVIDE factor','term',3,'p_term','new.py',166),
  ('term -> factor','term',1,'p_term','new.py',167),
  ('factor -> NUMBER','factor',1,'p_factor','new.py',177),
  ('factor -> ID','factor',1,'p_factor','new.py',178),
  ('factor -> ID LSQUARE expression RSQUARE','factor',4,'p_factor','new.py',179),
  ('condition -> expression GT expression','condition',3,'p_condition','new.py',189),
  ('condition -> expression LT expression','condition',3,'p_condition','new.py',190),
  ('condition -> expression EQ expression','condition',3,'p_condition','new.py',191),
  ('condition -> expression NEQ expression','condition',3,'p_condition','new.py',192),
]
