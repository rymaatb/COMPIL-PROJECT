
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND BOOL BOOL_TYPE CHAR CHAR_TYPE COLON COMMA COMMENT CONST DECLARATION DIVIDE ELSE EQ EQUALS FALSE FLOAT FLOAT_TYPE FOR GT GTE ID IF INSTRUCTION INTEGER INT_TYPE LBRACE LBRACKET LPAREN LT LTE MINUS MULTIPLY NEQ NOT NUMBER OR PLUS RBRACE RBRACKET READ RPAREN SEMICOLON TRUE VAR_GLOBAL WRITEPROGRAMME : varGlobal declaration_ instructionvarGlobal : VAR_GLOBAL LBRACE declarations RBRACE declaration_ : DECLARATION LBRACE declarations RBRACEinstruction : INSTRUCTION LBRACE statements RBRACEdeclarations : declaration declarations\n                    | declaration\n                    | emptyempty :declaration : type declaration_list SEMICOLON\n                   | CONST type ID EQUALS expression SEMICOLONdeclaration_list : declaration\n                        | declaration COMMA declaration_listdeclaration : ID\n                   | ID EQUALS expressionstatement : ID EQUALS expression SEMICOLONtype : INT_TYPE\n            | FLOAT_TYPE\n            | BOOL_TYPE\n            | CHAR_TYPEfactor : INTEGER\n              | FLOAT\n              | CHAR\n              | BOOL\n              | MINUS factor\n              | PLUS factor\n              | LPAREN INTEGER RPAREN\n              | LPAREN FLOAT RPAREN\n              | LPAREN MINUS FLOAT RPAREN\n              statements : statement\n                  | statement statements\n                  | empty statement : simple_assignment\n                 | array_assignment\n                 declaration : array_declarationTab\n                 | type declarationTab_listTab SEMICOLON\n                 | const_declarationTabsimple_assignment : ID EQUALS expressionTab SEMICOLONconst_declarationTab : CONST type ID EQUALS expressionTab SEMICOLONarray_declarationTab : type ID LBRACKET INTEGER RBRACKET SEMICOLONdeclarationTab_listTab : declarationTab\n                        | declarationTab COMMA declarationTab_listTabdeclarationTab : ID\n                   | ID EQUALS expressionTab\n                   | ID LBRACKET INTEGER RBRACKETexpressionTab : ID\n                  | FLOAT\n                  | INTEGER\n                  | CHARfactor : ID LBRACKET expressionTab RBRACKETarray_assignment : ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLONexpression : expression OR term\n                  | termterm : term AND factor \n            | factorfactor : ID\n              | TRUE\n              | FALSE\n              | NOT IDstatement : type ID EQUALS expression_arithmetique SEMICOLONstatement :  ID EQUALS expression_arithmetique SEMICOLONexpression_arithmetique : expression_arithmetique PLUS term_arithmetique  \n    expression_arithmetique : expression_arithmetique MINUS term_arithmetiqueexpression_arithmetique : term_arithmetiqueterm_arithmetique : term_arithmetique MULTIPLY term_arithmetiqueterm_arithmetique : term_arithmetique DIVIDE factor_arithmetiqueterm_arithmetique : factor_arithmetiquefactor_arithmetique : ID\n                        | INTEGER\n                        | FLOAT\n                        factor_arithmetique : LPAREN expression_arithmetique RPARENstatement : READ LPAREN ID RPAREN SEMICOLON\n                 | READ LPAREN ID LBRACKET INTEGER RBRACKET RPAREN SEMICOLONstatement : WRITE LPAREN write_content RPAREN SEMICOLONwrite_content : write_itemwrite_content : write_content COMMA write_itemwrite_item : ID\n                  | ID LBRACKET INTEGER RBRACKETstatement : FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block initialisation : ID EQUALS INTEGERstep : INTEGER\n            | IDBorneSup : IDblock : LBRACE statements RBRACE'
    
_lr_action_items = {'VAR_GLOBAL':([0,],[3,]),'$end':([1,7,65,],[0,-1,-4,]),'DECLARATION':([2,24,],[5,-2,]),'LBRACE':([3,5,8,179,],[6,9,22,181,]),'INSTRUCTION':([4,43,],[8,-3,]),'CONST':([6,9,11,13,15,16,17,18,19,20,21,44,45,48,51,52,53,54,55,56,57,58,62,63,86,87,91,119,120,121,122,147,149,150,151,152,],[14,14,14,14,-13,-34,-36,-16,-17,-18,-19,-9,-35,14,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-39,-10,-38,-49,-28,]),'ID':([6,9,11,13,15,16,17,18,19,20,21,22,31,32,34,37,38,39,44,45,47,48,49,51,52,53,54,55,56,57,58,59,60,62,63,64,67,68,70,71,72,82,83,84,85,86,87,91,100,106,114,119,120,121,122,124,125,126,127,128,129,130,131,143,145,147,149,150,151,152,158,159,160,162,173,174,178,180,181,183,],[15,15,15,28,-13,-34,-36,-16,-17,-18,-19,36,50,51,36,-32,-33,69,-9,-35,74,15,81,-55,-14,-52,-54,-20,-21,-22,-23,51,51,-56,-57,91,92,101,107,110,112,74,101,51,51,-24,-25,-58,135,135,101,-51,-53,-26,-27,-15,-60,135,135,-37,135,135,135,110,167,-39,-10,-38,-49,-28,101,-59,-71,-73,177,-50,-72,-78,36,-83,]),'RBRACE':([6,9,10,11,12,15,16,17,22,23,25,33,34,35,37,38,44,45,51,52,53,54,55,56,57,58,62,63,66,86,87,91,119,120,121,122,124,125,128,147,149,150,151,152,159,160,162,174,178,180,181,182,183,],[-8,-8,24,-6,-7,-13,-34,-36,-8,43,-5,65,-8,-31,-32,-33,-9,-35,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-30,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-50,-72,-78,-8,183,-83,]),'INT_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,44,45,48,51,52,53,54,55,56,57,58,62,63,86,87,91,119,120,121,122,124,125,128,147,149,150,151,152,159,160,162,174,178,180,181,183,],[18,18,18,18,18,-13,-34,-36,-16,-17,-18,-19,18,18,-32,-33,-9,-35,18,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-50,-72,-78,18,-83,]),'FLOAT_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,44,45,48,51,52,53,54,55,56,57,58,62,63,86,87,91,119,120,121,122,124,125,128,147,149,150,151,152,159,160,162,174,178,180,181,183,],[19,19,19,19,19,-13,-34,-36,-16,-17,-18,-19,19,19,-32,-33,-9,-35,19,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-50,-72,-78,19,-83,]),'BOOL_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,44,45,48,51,52,53,54,55,56,57,58,62,63,86,87,91,119,120,121,122,124,125,128,147,149,150,151,152,159,160,162,174,178,180,181,183,],[20,20,20,20,20,-13,-34,-36,-16,-17,-18,-19,20,20,-32,-33,-9,-35,20,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-50,-72,-78,20,-83,]),'CHAR_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,44,45,48,51,52,53,54,55,56,57,58,62,63,86,87,91,119,120,121,122,124,125,128,147,149,150,151,152,159,160,162,174,178,180,181,183,],[21,21,21,21,21,-13,-34,-36,-16,-17,-18,-19,21,21,-32,-33,-9,-35,21,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-50,-72,-78,21,-83,]),'COMMA':([15,16,17,28,29,30,44,45,51,52,53,54,55,56,57,58,62,63,74,75,76,77,78,81,86,87,91,101,103,104,105,108,109,110,113,119,120,121,122,147,149,150,151,152,163,169,172,],[-13,-34,-36,-13,48,49,-9,-35,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-45,-43,-21,-20,-22,-42,-24,-25,-58,-45,-46,-47,-48,143,-74,-76,-44,-51,-53,-26,-27,-39,-10,-38,-49,-28,-75,-44,-77,]),'SEMICOLON':([15,16,17,26,27,28,29,30,44,45,51,52,53,54,55,56,57,58,62,63,74,75,76,77,78,79,80,81,86,87,91,92,93,94,95,96,97,98,99,101,103,104,105,113,116,117,119,120,121,122,135,137,138,139,140,142,147,149,150,151,152,153,154,155,156,157,169,170,175,],[-13,-34,-36,44,45,-13,-11,-40,-9,-35,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-45,-43,-21,-20,-22,-12,-41,-42,-24,-25,-58,-45,124,125,128,-63,-21,-20,-66,-45,-46,-47,-48,147,149,150,-51,-53,-26,-27,-67,159,-68,-69,160,162,-39,-10,-38,-49,-28,-61,-62,-64,-65,-70,-44,174,178,]),'EQUALS':([15,28,36,50,69,81,112,136,],[32,47,67,82,106,114,146,158,]),'READ':([22,34,37,38,124,125,128,159,160,162,174,178,180,181,183,],[40,40,-32,-33,-15,-60,-37,-59,-71,-73,-50,-72,-78,40,-83,]),'WRITE':([22,34,37,38,124,125,128,159,160,162,174,178,180,181,183,],[41,41,-32,-33,-15,-60,-37,-59,-71,-73,-50,-72,-78,41,-83,]),'FOR':([22,34,37,38,124,125,128,159,160,162,174,178,180,181,183,],[42,42,-32,-33,-15,-60,-37,-59,-71,-73,-50,-72,-78,42,-83,]),'LBRACKET':([28,36,51,74,81,92,107,110,],[46,68,83,83,115,83,141,144,]),'INTEGER':([32,46,47,59,60,61,67,68,82,83,84,85,100,106,114,115,126,127,129,130,131,141,144,145,146,158,],[55,73,77,55,55,88,98,104,77,104,55,55,132,138,104,148,138,138,138,138,138,161,164,166,168,104,]),'FLOAT':([32,47,59,60,61,67,68,82,83,84,85,90,100,106,114,126,127,129,130,131,158,],[56,76,56,56,89,97,103,76,103,56,56,123,133,139,103,139,139,139,139,139,103,]),'CHAR':([32,47,59,60,67,68,82,83,84,85,114,158,],[57,78,57,57,78,105,78,105,57,57,105,105,]),'BOOL':([32,47,59,60,67,82,84,85,],[58,58,58,58,58,58,58,58,]),'MINUS':([32,47,59,60,61,67,82,84,85,92,94,96,97,98,99,100,132,133,134,135,137,138,139,153,154,155,156,157,],[59,59,59,59,90,59,59,59,59,-67,127,-63,-69,-68,-66,90,-68,-69,127,-67,127,-68,-69,-61,-62,-64,-65,-70,]),'PLUS':([32,47,59,60,67,82,84,85,92,94,96,97,98,99,132,133,134,135,137,138,139,153,154,155,156,157,],[60,60,60,60,60,60,60,60,-67,126,-63,-69,-68,-66,-68,-69,126,-67,126,-68,-69,-61,-62,-64,-65,-70,]),'LPAREN':([32,40,41,42,47,59,60,67,82,84,85,100,106,126,127,129,130,131,],[61,70,71,72,61,61,61,100,61,61,61,131,131,131,131,131,131,131,]),'TRUE':([32,47,59,60,67,82,84,85,],[62,62,62,62,62,62,62,62,]),'FALSE':([32,47,59,60,67,82,84,85,],[63,63,63,63,63,63,63,63,]),'NOT':([32,47,59,60,67,82,84,85,],[64,64,64,64,64,64,64,64,]),'AND':([51,53,54,55,56,57,58,62,63,74,76,77,78,86,87,91,92,97,98,119,120,121,122,151,152,],[-55,85,-54,-20,-21,-22,-23,-56,-57,-55,-21,-20,-22,-24,-25,-58,-55,-21,-20,85,-53,-26,-27,-49,-28,]),'OR':([51,52,53,54,55,56,57,58,62,63,74,76,77,78,86,87,91,92,93,97,98,116,119,120,121,122,151,152,],[-55,84,-52,-54,-20,-21,-22,-23,-56,-57,-55,-21,-20,-22,-24,-25,-58,-55,84,-21,-20,84,-51,-53,-26,-27,-49,-28,]),'RBRACKET':([73,101,102,103,104,105,118,148,161,164,],[113,-45,136,-46,-47,-48,151,169,171,172,]),'RPAREN':([88,89,96,99,107,108,109,110,123,132,133,134,135,138,139,153,154,155,156,157,163,171,172,176,177,],[121,122,-63,-66,140,142,-74,-76,152,121,122,157,-67,-68,-69,-61,-62,-64,-65,-70,-75,175,-77,179,-82,]),'MULTIPLY':([92,96,97,98,99,132,133,135,138,139,153,154,155,156,157,],[-67,129,-69,-68,-66,-68,-69,-67,-68,-69,129,129,129,-65,-70,]),'DIVIDE':([92,96,97,98,99,132,133,135,138,139,153,154,155,156,157,],[-67,130,-69,-68,-66,-68,-69,-67,-68,-69,130,130,130,-65,-70,]),'COLON':([111,165,166,167,168,],[145,173,-80,-81,-79,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMME':([0,],[1,]),'varGlobal':([0,],[2,]),'declaration_':([2,],[4,]),'instruction':([4,],[7,]),'declarations':([6,9,11,],[10,23,25,]),'declaration':([6,9,11,13,48,],[11,11,11,29,29,]),'empty':([6,9,11,22,34,181,],[12,12,12,35,35,35,]),'type':([6,9,11,13,14,22,34,48,181,],[13,13,13,13,31,39,39,13,39,]),'array_declarationTab':([6,9,11,13,48,],[16,16,16,16,16,]),'const_declarationTab':([6,9,11,13,48,],[17,17,17,17,17,]),'declaration_list':([13,48,],[26,79,]),'declarationTab_listTab':([13,49,],[27,80,]),'declarationTab':([13,49,],[30,30,]),'statements':([22,34,181,],[33,66,182,]),'statement':([22,34,181,],[34,34,34,]),'simple_assignment':([22,34,181,],[37,37,37,]),'array_assignment':([22,34,181,],[38,38,38,]),'expression':([32,47,67,82,],[52,52,93,116,]),'term':([32,47,67,82,84,],[53,53,53,53,119,]),'factor':([32,47,59,60,67,82,84,85,],[54,54,86,87,54,54,54,120,]),'expressionTab':([47,67,68,82,83,114,158,],[75,95,102,117,118,75,170,]),'expression_arithmetique':([67,100,106,131,],[94,134,137,134,]),'term_arithmetique':([67,100,106,126,127,129,131,],[96,96,96,153,154,155,96,]),'factor_arithmetique':([67,100,106,126,127,129,130,131,],[99,99,99,99,99,99,156,99,]),'write_content':([71,],[108,]),'write_item':([71,143,],[109,163,]),'initialisation':([72,],[111,]),'step':([145,],[165,]),'BorneSup':([173,],[176,]),'block':([179,],[180,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMME","S'",1,None,None,None),
  ('PROGRAMME -> varGlobal declaration_ instruction','PROGRAMME',3,'p_PROGRAMME','parser.py',107),
  ('varGlobal -> VAR_GLOBAL LBRACE declarations RBRACE','varGlobal',4,'p_varGlobal','parser.py',110),
  ('declaration_ -> DECLARATION LBRACE declarations RBRACE','declaration_',4,'p_declaration_','parser.py',113),
  ('instruction -> INSTRUCTION LBRACE statements RBRACE','instruction',4,'p_instruction','parser.py',116),
  ('declarations -> declaration declarations','declarations',2,'p_declarations','parser.py',119),
  ('declarations -> declaration','declarations',1,'p_declarations','parser.py',120),
  ('declarations -> empty','declarations',1,'p_declarations','parser.py',121),
  ('empty -> <empty>','empty',0,'p_empty','parser.py',127),
  ('declaration -> type declaration_list SEMICOLON','declaration',3,'p_statement_declaration','parser.py',132),
  ('declaration -> CONST type ID EQUALS expression SEMICOLON','declaration',6,'p_statement_declaration','parser.py',133),
  ('declaration_list -> declaration','declaration_list',1,'p_declaration_list','parser.py',174),
  ('declaration_list -> declaration COMMA declaration_list','declaration_list',3,'p_declaration_list','parser.py',175),
  ('declaration -> ID','declaration',1,'p_declaration','parser.py',185),
  ('declaration -> ID EQUALS expression','declaration',3,'p_declaration','parser.py',186),
  ('statement -> ID EQUALS expression SEMICOLON','statement',4,'p_statement_assignment','parser.py',198),
  ('type -> INT_TYPE','type',1,'p_type','parser.py',215),
  ('type -> FLOAT_TYPE','type',1,'p_type','parser.py',216),
  ('type -> BOOL_TYPE','type',1,'p_type','parser.py',217),
  ('type -> CHAR_TYPE','type',1,'p_type','parser.py',218),
  ('factor -> INTEGER','factor',1,'p_factor_number','parser.py',222),
  ('factor -> FLOAT','factor',1,'p_factor_number','parser.py',223),
  ('factor -> CHAR','factor',1,'p_factor_number','parser.py',224),
  ('factor -> BOOL','factor',1,'p_factor_number','parser.py',225),
  ('factor -> MINUS factor','factor',2,'p_factor_number','parser.py',226),
  ('factor -> PLUS factor','factor',2,'p_factor_number','parser.py',227),
  ('factor -> LPAREN INTEGER RPAREN','factor',3,'p_factor_number','parser.py',228),
  ('factor -> LPAREN FLOAT RPAREN','factor',3,'p_factor_number','parser.py',229),
  ('factor -> LPAREN MINUS FLOAT RPAREN','factor',4,'p_factor_number','parser.py',230),
  ('statements -> statement','statements',1,'p_statements','parser.py',251),
  ('statements -> statement statements','statements',2,'p_statements','parser.py',252),
  ('statements -> empty','statements',1,'p_statements','parser.py',253),
  ('statement -> simple_assignment','statement',1,'p_statement','parser.py',260),
  ('statement -> array_assignment','statement',1,'p_statement','parser.py',261),
  ('declaration -> array_declarationTab','declaration',1,'p_declaration_t','parser.py',264),
  ('declaration -> type declarationTab_listTab SEMICOLON','declaration',3,'p_declaration_t','parser.py',265),
  ('declaration -> const_declarationTab','declaration',1,'p_declaration_t','parser.py',266),
  ('simple_assignment -> ID EQUALS expressionTab SEMICOLON','simple_assignment',4,'p_simple_assignment','parser.py',269),
  ('const_declarationTab -> CONST type ID EQUALS expressionTab SEMICOLON','const_declarationTab',6,'p_const_declarationTab','parser.py',285),
  ('array_declarationTab -> type ID LBRACKET INTEGER RBRACKET SEMICOLON','array_declarationTab',6,'p_array_declarationTab','parser.py',292),
  ('declarationTab_listTab -> declarationTab','declarationTab_listTab',1,'p_declarationTab_listTab','parser.py',305),
  ('declarationTab_listTab -> declarationTab COMMA declarationTab_listTab','declarationTab_listTab',3,'p_declarationTab_listTab','parser.py',306),
  ('declarationTab -> ID','declarationTab',1,'p_declarationTab','parser.py',313),
  ('declarationTab -> ID EQUALS expressionTab','declarationTab',3,'p_declarationTab','parser.py',314),
  ('declarationTab -> ID LBRACKET INTEGER RBRACKET','declarationTab',4,'p_declarationTab','parser.py',315),
  ('expressionTab -> ID','expressionTab',1,'p_expressionTab','parser.py',328),
  ('expressionTab -> FLOAT','expressionTab',1,'p_expressionTab','parser.py',329),
  ('expressionTab -> INTEGER','expressionTab',1,'p_expressionTab','parser.py',330),
  ('expressionTab -> CHAR','expressionTab',1,'p_expressionTab','parser.py',331),
  ('factor -> ID LBRACKET expressionTab RBRACKET','factor',4,'p_array_access','parser.py',336),
  ('array_assignment -> ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLON','array_assignment',7,'p_array_assignment','parser.py',363),
  ('expression -> expression OR term','expression',3,'p_expression_el','parser.py',421),
  ('expression -> term','expression',1,'p_expression_el','parser.py',422),
  ('term -> term AND factor','term',3,'p_term_tl','parser.py',439),
  ('term -> factor','term',1,'p_term_tl','parser.py',440),
  ('factor -> ID','factor',1,'p_factor_fl','parser.py',460),
  ('factor -> TRUE','factor',1,'p_factor_fl','parser.py',461),
  ('factor -> FALSE','factor',1,'p_factor_fl','parser.py',462),
  ('factor -> NOT ID','factor',2,'p_factor_fl','parser.py',463),
  ('statement -> type ID EQUALS expression_arithmetique SEMICOLON','statement',5,'p_statement_assignmentArth','parser.py',601),
  ('statement -> ID EQUALS expression_arithmetique SEMICOLON','statement',4,'p_declarationArth','parser.py',606),
  ('expression_arithmetique -> expression_arithmetique PLUS term_arithmetique','expression_arithmetique',3,'p_expression_plus','parser.py',612),
  ('expression_arithmetique -> expression_arithmetique MINUS term_arithmetique','expression_arithmetique',3,'p_expression_minus','parser.py',617),
  ('expression_arithmetique -> term_arithmetique','expression_arithmetique',1,'p_expression_term','parser.py',621),
  ('term_arithmetique -> term_arithmetique MULTIPLY term_arithmetique','term_arithmetique',3,'p_term_multiply','parser.py',625),
  ('term_arithmetique -> term_arithmetique DIVIDE factor_arithmetique','term_arithmetique',3,'p_term_divide','parser.py',630),
  ('term_arithmetique -> factor_arithmetique','term_arithmetique',1,'p_term_factor','parser.py',634),
  ('factor_arithmetique -> ID','factor_arithmetique',1,'p_factor_id','parser.py',638),
  ('factor_arithmetique -> INTEGER','factor_arithmetique',1,'p_factor_id','parser.py',639),
  ('factor_arithmetique -> FLOAT','factor_arithmetique',1,'p_factor_id','parser.py',640),
  ('factor_arithmetique -> LPAREN expression_arithmetique RPAREN','factor_arithmetique',3,'p_factor_parens','parser.py',645),
  ('statement -> READ LPAREN ID RPAREN SEMICOLON','statement',5,'p_read_statement','parser.py',750),
  ('statement -> READ LPAREN ID LBRACKET INTEGER RBRACKET RPAREN SEMICOLON','statement',8,'p_read_statement','parser.py',751),
  ('statement -> WRITE LPAREN write_content RPAREN SEMICOLON','statement',5,'p_write_statement','parser.py',827),
  ('write_content -> write_item','write_content',1,'p_write_content_single','parser.py',832),
  ('write_content -> write_content COMMA write_item','write_content',3,'p_write_content_multiple','parser.py',836),
  ('write_item -> ID','write_item',1,'p_write_item','parser.py',840),
  ('write_item -> ID LBRACKET INTEGER RBRACKET','write_item',4,'p_write_item','parser.py',841),
  ('statement -> FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block','statement',9,'p_statement_FORloop','parser.py',879),
  ('initialisation -> ID EQUALS INTEGER','initialisation',3,'p_initialisation','parser.py',885),
  ('step -> INTEGER','step',1,'p_step','parser.py',894),
  ('step -> ID','step',1,'p_step','parser.py',895),
  ('BorneSup -> ID','BorneSup',1,'p_BorneSup','parser.py',903),
  ('block -> LBRACE statements RBRACE','block',3,'p_block','parser.py',908),
]
