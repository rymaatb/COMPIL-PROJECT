
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND BOOL BOOL_TYPE CHAR CHAR_TYPE COLON COMMA COMMENT CONST DECLARATION DIVIDE ELSE EQ EQUALS FLOAT FLOAT_TYPE FOR GT GTE ID IF INSTRUCTION INT INT_TYPE LBRACE LBRACKET LPAREN LT LTE MINUS MULTIPLY NEQ NOT NUMBER OR PLUS RBRACE RBRACKET READ RPAREN SEMICOLON VAR_GLOBAL WRITEstatement : type declaration_list SEMICOLON\n                 | CONST type ID EQUALS expression SEMICOLONdeclaration_list : declaration\n                        | declaration COMMA declaration_listdeclaration : ID\n                   | ID EQUALS expression\n                   | ID LBRACKET NUMBER RBRACKETexpression : ID\n                  | FLOAT\n                  | INT\n                  | CHARtype : INT_TYPE\n            | FLOAT_TYPE\n            | BOOL_TYPE\n            | CHAR_TYPEstatement : READ LPAREN ID RPAREN SEMICOLONstatement : WRITE LPAREN write_content RPAREN SEMICOLONwrite_content : write_itemwrite_content : write_content COMMA write_itemwrite_item : ID\n                  | ID LBRACKET NUMBER RBRACKET'
    
_lr_action_items = {'CONST':([0,],[3,]),'READ':([0,],[4,]),'WRITE':([0,],[5,]),'INT_TYPE':([0,3,],[6,6,]),'FLOAT_TYPE':([0,3,],[7,7,]),'BOOL_TYPE':([0,3,],[8,8,]),'CHAR_TYPE':([0,3,],[9,9,]),'$end':([1,16,39,40,43,],[0,-1,-16,-17,-2,]),'ID':([2,6,7,8,9,13,14,15,17,18,32,35,],[12,-12,-13,-14,-15,20,21,24,12,26,26,24,]),'LPAREN':([4,5,],[14,15,]),'SEMICOLON':([10,11,12,25,26,27,28,29,30,33,34,37,38,],[16,-3,-5,-4,-8,-6,-9,-10,-11,39,40,-7,43,]),'COMMA':([11,12,22,23,24,26,27,28,29,30,37,41,44,],[17,-5,35,-18,-20,-8,-6,-9,-10,-11,-7,-19,-21,]),'EQUALS':([12,20,],[18,32,]),'LBRACKET':([12,24,],[19,36,]),'FLOAT':([18,32,],[28,28,]),'INT':([18,32,],[29,29,]),'CHAR':([18,32,],[30,30,]),'NUMBER':([19,36,],[31,42,]),'RPAREN':([21,22,23,24,41,44,],[33,34,-18,-20,-19,-21,]),'RBRACKET':([31,42,],[37,44,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'statement':([0,],[1,]),'type':([0,3,],[2,13,]),'declaration_list':([2,17,],[10,25,]),'declaration':([2,17,],[11,11,]),'write_content':([15,],[22,]),'write_item':([15,35,],[23,41,]),'expression':([18,32,],[27,38,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> statement","S'",1,None,None,None),
  ('statement -> type declaration_list SEMICOLON','statement',3,'p_statement_declaration','compiler.py',139),
  ('statement -> CONST type ID EQUALS expression SEMICOLON','statement',6,'p_statement_declaration','compiler.py',140),
  ('declaration_list -> declaration','declaration_list',1,'p_declaration_list','compiler.py',157),
  ('declaration_list -> declaration COMMA declaration_list','declaration_list',3,'p_declaration_list','compiler.py',158),
  ('declaration -> ID','declaration',1,'p_declaration','compiler.py',165),
  ('declaration -> ID EQUALS expression','declaration',3,'p_declaration','compiler.py',166),
  ('declaration -> ID LBRACKET NUMBER RBRACKET','declaration',4,'p_declaration','compiler.py',167),
  ('expression -> ID','expression',1,'p_expression','compiler.py',179),
  ('expression -> FLOAT','expression',1,'p_expression','compiler.py',180),
  ('expression -> INT','expression',1,'p_expression','compiler.py',181),
  ('expression -> CHAR','expression',1,'p_expression','compiler.py',182),
  ('type -> INT_TYPE','type',1,'p_type','compiler.py',202),
  ('type -> FLOAT_TYPE','type',1,'p_type','compiler.py',203),
  ('type -> BOOL_TYPE','type',1,'p_type','compiler.py',204),
  ('type -> CHAR_TYPE','type',1,'p_type','compiler.py',205),
  ('statement -> READ LPAREN ID RPAREN SEMICOLON','statement',5,'p_read_statement','compiler.py',210),
  ('statement -> WRITE LPAREN write_content RPAREN SEMICOLON','statement',5,'p_write_statement','compiler.py',226),
  ('write_content -> write_item','write_content',1,'p_write_content_single','compiler.py',232),
  ('write_content -> write_content COMMA write_item','write_content',3,'p_write_content_multiple','compiler.py',236),
  ('write_item -> ID','write_item',1,'p_write_item','compiler.py',240),
  ('write_item -> ID LBRACKET NUMBER RBRACKET','write_item',4,'p_write_item','compiler.py',241),
]
