
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND BOOL BOOL_TYPE CHAR CHAR_TYPE COLON COMMA COMMENT CONST DECLARATION DIVIDE ELSE EQ EQUALS FALSE FLOAT FLOAT_TYPE FOR GT GTE ID IF INSTRUCTION INTEGER INT_TYPE LBRACE LBRACKET LPAREN LT LTE MINUS MULTIPLY NEQ NOT NUMBER OR PLUS RBRACE RBRACKET READ RPAREN SEMICOLON STRING TRUE VAR_GLOBAL WRITEPROGRAMME : varGlobal declaration_ instructionvarGlobal : VAR_GLOBAL LBRACE declarations RBRACE declaration_ : DECLARATION LBRACE declarations RBRACEinstruction : INSTRUCTION LBRACE statements RBRACEdeclarations : declaration declarations\n| declaration\n| emptyempty :declaration : type declaration_list SEMICOLON\n| CONST type ID EQUALS expression SEMICOLONdeclaration_list : declaration\n| declaration COMMA declaration_listdeclaration : ID\n| ID EQUALS expressionstatement : ID EQUALS expression SEMICOLONtype : INT_TYPE\n| FLOAT_TYPE\n| BOOL_TYPE\n| CHAR_TYPEfactor : INTEGER\n| FLOAT\n| CHAR\n| BOOL\n| MINUS factor\n| PLUS factor\n| LPAREN INTEGER RPAREN\n| LPAREN FLOAT RPAREN\n| LPAREN MINUS FLOAT RPAREN\nstatements : statement\n| statement statements\n| empty statement : simple_assignment\n| array_assignment\ndeclaration : array_declarationTab\n| type declarationTab_listTab SEMICOLON\n| const_declarationTabsimple_assignment : ID EQUALS expressionTab SEMICOLONconst_declarationTab : CONST type ID EQUALS expressionTab SEMICOLONarray_declarationTab : type ID LBRACKET INTEGER RBRACKET SEMICOLONdeclarationTab_listTab : declarationTab\n| declarationTab COMMA declarationTab_listTabdeclarationTab : ID\n| ID EQUALS expressionTab\n| ID LBRACKET INTEGER RBRACKETexpressionTab : ID\n| FLOAT\n| INTEGER\n| CHARfactor : ID LBRACKET expressionTab RBRACKETarray_assignment : ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLONexpression : expression OR term\n| termterm : term AND factor \n| factorfactor : ID\n| TRUE\n| FALSE\n| NOT IDstatement : type ID EQUALS expression_arithmetique SEMICOLONstatement :  ID EQUALS expression_arithmetique SEMICOLONexpression_arithmetique : expression_arithmetique PLUS term_arithmetique  \n    expression_arithmetique : expression_arithmetique MINUS term_arithmetiqueexpression_arithmetique : term_arithmetiqueterm_arithmetique : term_arithmetique MULTIPLY term_arithmetiqueterm_arithmetique : term_arithmetique DIVIDE factor_arithmetiqueterm_arithmetique : factor_arithmetiquefactor_arithmetique : ID\n| INTEGER\n| FLOAT\nfactor_arithmetique : LPAREN expression_arithmetique RPARENstatement : READ LPAREN ID RPAREN SEMICOLON\n| READ LPAREN ID LBRACKET INTEGER RBRACKET RPAREN SEMICOLONstatement : WRITE LPAREN write_content RPAREN SEMICOLONwrite_content : write_itemwrite_content : write_content COMMA write_itemwrite_item : ID\n| ID LBRACKET INTEGER RBRACKET\n| STRINGstatement : FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block initialisation : ID EQUALS INTEGERstep : INTEGER\n| IDBorneSup : IDblock : LBRACE statements RBRACEstatement : IFTHEN \n| IFTHENELSEIFTHENELSE : IF LPAREN condition RPAREN block ELSE blockIFTHEN : IF LPAREN condition RPAREN blockconditionIF : IF LPAREN conditioncondition : ID EQ factor\n| ID NEQ factor\n| ID LT factor\n| ID LTE factor\n| ID GT factor\n| ID GTE factor\n| factor EQ factor\n| factor NEQ factor\n| factor LT factor\n| factor LTE factor\n| factor GT factor\n| factor GTE factor'
    
_lr_action_items = {'VAR_GLOBAL':([0,],[3,]),'$end':([1,7,68,],[0,-1,-4,]),'DECLARATION':([2,24,],[5,-2,]),'LBRACE':([3,5,8,155,209,218,],[6,9,22,191,191,191,]),'INSTRUCTION':([4,46,],[8,-3,]),'CONST':([6,9,11,13,15,16,17,18,19,20,21,47,48,51,54,55,56,57,58,59,60,61,65,66,90,91,95,127,128,129,130,168,170,171,172,173,],[14,14,14,14,-13,-34,-36,-16,-17,-18,-19,-9,-35,14,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-39,-10,-38,-49,-28,]),'ID':([6,9,11,13,15,16,17,18,19,20,21,22,31,32,34,37,38,39,43,44,47,48,50,51,52,54,55,56,57,58,59,60,61,62,63,65,66,67,70,71,73,74,75,76,86,87,88,89,90,91,95,104,110,122,127,128,129,130,132,133,134,135,136,137,138,139,151,153,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,179,180,181,183,190,191,208,211,215,216,217,219,],[15,15,15,28,-13,-34,-36,-16,-17,-18,-19,36,53,54,36,-32,-33,72,-85,-86,-9,-35,78,15,85,-55,-14,-52,-54,-20,-21,-22,-23,54,54,-56,-57,95,96,105,111,114,117,119,78,105,54,54,-24,-25,-58,143,143,105,-51,-53,-26,-27,-15,-60,143,143,-37,143,143,143,114,188,54,54,54,54,54,54,54,54,54,54,54,54,-39,-10,-38,-49,-28,105,-59,-71,-73,-88,36,214,-50,-87,-84,-72,-79,]),'RBRACE':([6,9,10,11,12,15,16,17,22,23,25,33,34,35,37,38,43,44,47,48,54,55,56,57,58,59,60,61,65,66,69,90,91,95,127,128,129,130,132,133,136,168,170,171,172,173,180,181,183,190,191,210,211,215,216,217,219,],[-8,-8,24,-6,-7,-13,-34,-36,-8,46,-5,68,-8,-31,-32,-33,-85,-86,-9,-35,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-30,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-88,-8,216,-50,-87,-84,-72,-79,]),'INT_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,43,44,47,48,51,54,55,56,57,58,59,60,61,65,66,90,91,95,127,128,129,130,132,133,136,168,170,171,172,173,180,181,183,190,191,211,215,216,217,219,],[18,18,18,18,18,-13,-34,-36,-16,-17,-18,-19,18,18,-32,-33,-85,-86,-9,-35,18,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-88,18,-50,-87,-84,-72,-79,]),'FLOAT_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,43,44,47,48,51,54,55,56,57,58,59,60,61,65,66,90,91,95,127,128,129,130,132,133,136,168,170,171,172,173,180,181,183,190,191,211,215,216,217,219,],[19,19,19,19,19,-13,-34,-36,-16,-17,-18,-19,19,19,-32,-33,-85,-86,-9,-35,19,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-88,19,-50,-87,-84,-72,-79,]),'BOOL_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,43,44,47,48,51,54,55,56,57,58,59,60,61,65,66,90,91,95,127,128,129,130,132,133,136,168,170,171,172,173,180,181,183,190,191,211,215,216,217,219,],[20,20,20,20,20,-13,-34,-36,-16,-17,-18,-19,20,20,-32,-33,-85,-86,-9,-35,20,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-88,20,-50,-87,-84,-72,-79,]),'CHAR_TYPE':([6,9,11,13,14,15,16,17,18,19,20,21,22,34,37,38,43,44,47,48,51,54,55,56,57,58,59,60,61,65,66,90,91,95,127,128,129,130,132,133,136,168,170,171,172,173,180,181,183,190,191,211,215,216,217,219,],[21,21,21,21,21,-13,-34,-36,-16,-17,-18,-19,21,21,-32,-33,-85,-86,-9,-35,21,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-24,-25,-58,-51,-53,-26,-27,-15,-60,-37,-39,-10,-38,-49,-28,-59,-71,-73,-88,21,-50,-87,-84,-72,-79,]),'COMMA':([15,16,17,28,29,30,47,48,54,55,56,57,58,59,60,61,65,66,78,79,80,81,82,85,90,91,95,105,107,108,109,112,113,114,115,121,127,128,129,130,168,170,171,172,173,184,204,207,],[-13,-34,-36,-13,51,52,-9,-35,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-45,-43,-21,-20,-22,-42,-24,-25,-58,-45,-46,-47,-48,151,-74,-76,-78,-44,-51,-53,-26,-27,-39,-10,-38,-49,-28,-75,-44,-77,]),'SEMICOLON':([15,16,17,26,27,28,29,30,47,48,54,55,56,57,58,59,60,61,65,66,78,79,80,81,82,83,84,85,90,91,95,96,97,98,99,100,101,102,103,105,107,108,109,121,124,125,127,128,129,130,143,145,146,147,148,150,168,170,171,172,173,174,175,176,177,178,204,205,212,],[-13,-34,-36,47,48,-13,-11,-40,-9,-35,-55,-14,-52,-54,-20,-21,-22,-23,-56,-57,-45,-43,-21,-20,-22,-12,-41,-42,-24,-25,-58,-45,132,133,136,-63,-21,-20,-66,-45,-46,-47,-48,168,170,171,-51,-53,-26,-27,-67,180,-68,-69,181,183,-39,-10,-38,-49,-28,-61,-62,-64,-65,-70,-44,211,217,]),'EQUALS':([15,28,36,53,72,85,117,144,],[32,50,70,86,110,122,154,179,]),'READ':([22,34,37,38,43,44,132,133,136,180,181,183,190,191,211,215,216,217,219,],[40,40,-32,-33,-85,-86,-15,-60,-37,-59,-71,-73,-88,40,-50,-87,-84,-72,-79,]),'WRITE':([22,34,37,38,43,44,132,133,136,180,181,183,190,191,211,215,216,217,219,],[41,41,-32,-33,-85,-86,-15,-60,-37,-59,-71,-73,-88,41,-50,-87,-84,-72,-79,]),'FOR':([22,34,37,38,43,44,132,133,136,180,181,183,190,191,211,215,216,217,219,],[42,42,-32,-33,-85,-86,-15,-60,-37,-59,-71,-73,-88,42,-50,-87,-84,-72,-79,]),'IF':([22,34,37,38,43,44,132,133,136,180,181,183,190,191,211,215,216,217,219,],[45,45,-32,-33,-85,-86,-15,-60,-37,-59,-71,-73,-88,45,-50,-87,-84,-72,-79,]),'LBRACKET':([28,36,54,78,85,96,111,114,119,],[49,71,87,87,123,87,149,152,87,]),'INTEGER':([32,49,50,62,63,64,70,71,76,86,87,88,89,104,110,122,123,134,135,137,138,139,149,152,153,154,156,157,158,159,160,161,162,163,164,165,166,167,179,],[58,77,81,58,58,92,102,108,58,81,108,58,58,140,146,108,169,146,146,146,146,146,182,185,187,189,58,58,58,58,58,58,58,58,58,58,58,58,108,]),'FLOAT':([32,50,62,63,64,70,71,76,86,87,88,89,94,104,110,122,134,135,137,138,139,156,157,158,159,160,161,162,163,164,165,166,167,179,],[59,80,59,59,93,101,107,59,80,107,59,59,131,141,147,107,147,147,147,147,147,59,59,59,59,59,59,59,59,59,59,59,59,107,]),'CHAR':([32,50,62,63,70,71,76,86,87,88,89,122,156,157,158,159,160,161,162,163,164,165,166,167,179,],[60,82,60,60,82,109,60,82,109,60,60,109,60,60,60,60,60,60,60,60,60,60,60,60,109,]),'BOOL':([32,50,62,63,70,76,86,88,89,156,157,158,159,160,161,162,163,164,165,166,167,],[61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,]),'MINUS':([32,50,62,63,64,70,76,86,88,89,96,98,100,101,102,103,104,140,141,142,143,145,146,147,156,157,158,159,160,161,162,163,164,165,166,167,174,175,176,177,178,],[62,62,62,62,94,62,62,62,62,62,-67,135,-63,-69,-68,-66,94,-68,-69,135,-67,135,-68,-69,62,62,62,62,62,62,62,62,62,62,62,62,-61,-62,-64,-65,-70,]),'PLUS':([32,50,62,63,70,76,86,88,89,96,98,100,101,102,103,140,141,142,143,145,146,147,156,157,158,159,160,161,162,163,164,165,166,167,174,175,176,177,178,],[63,63,63,63,63,63,63,63,63,-67,134,-63,-69,-68,-66,-68,-69,134,-67,134,-68,-69,63,63,63,63,63,63,63,63,63,63,63,63,-61,-62,-64,-65,-70,]),'LPAREN':([32,40,41,42,45,50,62,63,70,76,86,88,89,104,110,134,135,137,138,139,156,157,158,159,160,161,162,163,164,165,166,167,],[64,73,74,75,76,64,64,64,104,64,64,64,64,139,139,139,139,139,139,139,64,64,64,64,64,64,64,64,64,64,64,64,]),'TRUE':([32,50,62,63,70,76,86,88,89,156,157,158,159,160,161,162,163,164,165,166,167,],[65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,]),'FALSE':([32,50,62,63,70,76,86,88,89,156,157,158,159,160,161,162,163,164,165,166,167,],[66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,]),'NOT':([32,50,62,63,70,76,86,88,89,156,157,158,159,160,161,162,163,164,165,166,167,],[67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,]),'AND':([54,56,57,58,59,60,61,65,66,78,80,81,82,90,91,95,96,101,102,127,128,129,130,172,173,],[-55,89,-54,-20,-21,-22,-23,-56,-57,-55,-21,-20,-22,-24,-25,-58,-55,-21,-20,89,-53,-26,-27,-49,-28,]),'OR':([54,55,56,57,58,59,60,61,65,66,78,80,81,82,90,91,95,96,97,101,102,124,127,128,129,130,172,173,],[-55,88,-52,-54,-20,-21,-22,-23,-56,-57,-55,-21,-20,-22,-24,-25,-58,-55,88,-21,-20,88,-51,-53,-26,-27,-49,-28,]),'EQ':([54,58,59,60,61,65,66,90,91,95,119,120,129,130,172,173,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,-58,156,162,-26,-27,-49,-28,]),'NEQ':([54,58,59,60,61,65,66,90,91,95,119,120,129,130,172,173,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,-58,157,163,-26,-27,-49,-28,]),'LT':([54,58,59,60,61,65,66,90,91,95,119,120,129,130,172,173,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,-58,158,164,-26,-27,-49,-28,]),'LTE':([54,58,59,60,61,65,66,90,91,95,119,120,129,130,172,173,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,-58,159,165,-26,-27,-49,-28,]),'GT':([54,58,59,60,61,65,66,90,91,95,119,120,129,130,172,173,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,-58,160,166,-26,-27,-49,-28,]),'GTE':([54,58,59,60,61,65,66,90,91,95,119,120,129,130,172,173,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,-58,161,167,-26,-27,-49,-28,]),'RPAREN':([54,58,59,60,61,65,66,90,91,92,93,95,100,103,111,112,113,114,115,118,129,130,131,140,141,142,143,146,147,172,173,174,175,176,177,178,184,192,193,194,195,196,197,198,199,200,201,202,203,206,207,213,214,],[-55,-20,-21,-22,-23,-56,-57,-24,-25,129,130,-58,-63,-66,148,150,-74,-76,-78,155,-26,-27,173,129,130,178,-67,-68,-69,-49,-28,-61,-62,-64,-65,-70,-75,-90,-91,-92,-93,-94,-95,-96,-97,-98,-99,-100,-101,212,-77,218,-83,]),'STRING':([74,151,],[115,115,]),'RBRACKET':([77,105,106,107,108,109,126,169,182,185,],[121,-45,144,-46,-47,-48,172,204,206,207,]),'MULTIPLY':([96,100,101,102,103,140,141,143,146,147,174,175,176,177,178,],[-67,137,-69,-68,-66,-68,-69,-67,-68,-69,137,137,137,-65,-70,]),'DIVIDE':([96,100,101,102,103,140,141,143,146,147,174,175,176,177,178,],[-67,138,-69,-68,-66,-68,-69,-67,-68,-69,138,138,138,-65,-70,]),'COLON':([116,186,187,188,189,],[153,208,-81,-82,-80,]),'ELSE':([190,216,],[209,-84,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMME':([0,],[1,]),'varGlobal':([0,],[2,]),'declaration_':([2,],[4,]),'instruction':([4,],[7,]),'declarations':([6,9,11,],[10,23,25,]),'declaration':([6,9,11,13,51,],[11,11,11,29,29,]),'empty':([6,9,11,22,34,191,],[12,12,12,35,35,35,]),'type':([6,9,11,13,14,22,34,51,191,],[13,13,13,13,31,39,39,13,39,]),'array_declarationTab':([6,9,11,13,51,],[16,16,16,16,16,]),'const_declarationTab':([6,9,11,13,51,],[17,17,17,17,17,]),'declaration_list':([13,51,],[26,83,]),'declarationTab_listTab':([13,52,],[27,84,]),'declarationTab':([13,52,],[30,30,]),'statements':([22,34,191,],[33,69,210,]),'statement':([22,34,191,],[34,34,34,]),'simple_assignment':([22,34,191,],[37,37,37,]),'array_assignment':([22,34,191,],[38,38,38,]),'IFTHEN':([22,34,191,],[43,43,43,]),'IFTHENELSE':([22,34,191,],[44,44,44,]),'expression':([32,50,70,86,],[55,55,97,124,]),'term':([32,50,70,86,88,],[56,56,56,56,127,]),'factor':([32,50,62,63,70,76,86,88,89,156,157,158,159,160,161,162,163,164,165,166,167,],[57,57,90,91,57,120,57,57,128,192,193,194,195,196,197,198,199,200,201,202,203,]),'expressionTab':([50,70,71,86,87,122,179,],[79,99,106,125,126,79,205,]),'expression_arithmetique':([70,104,110,139,],[98,142,145,142,]),'term_arithmetique':([70,104,110,134,135,137,139,],[100,100,100,174,175,176,100,]),'factor_arithmetique':([70,104,110,134,135,137,138,139,],[103,103,103,103,103,103,177,103,]),'write_content':([74,],[112,]),'write_item':([74,151,],[113,184,]),'initialisation':([75,],[116,]),'condition':([76,],[118,]),'step':([153,],[186,]),'block':([155,209,218,],[190,215,219,]),'BorneSup':([208,],[213,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMME","S'",1,None,None,None),
  ('PROGRAMME -> varGlobal declaration_ instruction','PROGRAMME',3,'p_PROGRAMME','parser.py',99),
  ('varGlobal -> VAR_GLOBAL LBRACE declarations RBRACE','varGlobal',4,'p_varGlobal','parser.py',103),
  ('declaration_ -> DECLARATION LBRACE declarations RBRACE','declaration_',4,'p_declaration_','parser.py',120),
  ('instruction -> INSTRUCTION LBRACE statements RBRACE','instruction',4,'p_instruction','parser.py',127),
  ('declarations -> declaration declarations','declarations',2,'p_declarations','parser.py',132),
  ('declarations -> declaration','declarations',1,'p_declarations','parser.py',133),
  ('declarations -> empty','declarations',1,'p_declarations','parser.py',134),
  ('empty -> <empty>','empty',0,'p_empty','parser.py',142),
  ('declaration -> type declaration_list SEMICOLON','declaration',3,'p_statement_declaration','parser.py',153),
  ('declaration -> CONST type ID EQUALS expression SEMICOLON','declaration',6,'p_statement_declaration','parser.py',154),
  ('declaration_list -> declaration','declaration_list',1,'p_declaration_list','parser.py',196),
  ('declaration_list -> declaration COMMA declaration_list','declaration_list',3,'p_declaration_list','parser.py',197),
  ('declaration -> ID','declaration',1,'p_declaration','parser.py',207),
  ('declaration -> ID EQUALS expression','declaration',3,'p_declaration','parser.py',208),
  ('statement -> ID EQUALS expression SEMICOLON','statement',4,'p_statement_assignment','parser.py',220),
  ('type -> INT_TYPE','type',1,'p_type','parser.py',241),
  ('type -> FLOAT_TYPE','type',1,'p_type','parser.py',242),
  ('type -> BOOL_TYPE','type',1,'p_type','parser.py',243),
  ('type -> CHAR_TYPE','type',1,'p_type','parser.py',244),
  ('factor -> INTEGER','factor',1,'p_factor_number','parser.py',248),
  ('factor -> FLOAT','factor',1,'p_factor_number','parser.py',249),
  ('factor -> CHAR','factor',1,'p_factor_number','parser.py',250),
  ('factor -> BOOL','factor',1,'p_factor_number','parser.py',251),
  ('factor -> MINUS factor','factor',2,'p_factor_number','parser.py',252),
  ('factor -> PLUS factor','factor',2,'p_factor_number','parser.py',253),
  ('factor -> LPAREN INTEGER RPAREN','factor',3,'p_factor_number','parser.py',254),
  ('factor -> LPAREN FLOAT RPAREN','factor',3,'p_factor_number','parser.py',255),
  ('factor -> LPAREN MINUS FLOAT RPAREN','factor',4,'p_factor_number','parser.py',256),
  ('statements -> statement','statements',1,'p_statements','parser.py',277),
  ('statements -> statement statements','statements',2,'p_statements','parser.py',278),
  ('statements -> empty','statements',1,'p_statements','parser.py',279),
  ('statement -> simple_assignment','statement',1,'p_statement','parser.py',286),
  ('statement -> array_assignment','statement',1,'p_statement','parser.py',287),
  ('declaration -> array_declarationTab','declaration',1,'p_declaration_t','parser.py',290),
  ('declaration -> type declarationTab_listTab SEMICOLON','declaration',3,'p_declaration_t','parser.py',291),
  ('declaration -> const_declarationTab','declaration',1,'p_declaration_t','parser.py',292),
  ('simple_assignment -> ID EQUALS expressionTab SEMICOLON','simple_assignment',4,'p_simple_assignment','parser.py',295),
  ('const_declarationTab -> CONST type ID EQUALS expressionTab SEMICOLON','const_declarationTab',6,'p_const_declarationTab','parser.py',312),
  ('array_declarationTab -> type ID LBRACKET INTEGER RBRACKET SEMICOLON','array_declarationTab',6,'p_array_declarationTab','parser.py',319),
  ('declarationTab_listTab -> declarationTab','declarationTab_listTab',1,'p_declarationTab_listTab','parser.py',333),
  ('declarationTab_listTab -> declarationTab COMMA declarationTab_listTab','declarationTab_listTab',3,'p_declarationTab_listTab','parser.py',334),
  ('declarationTab -> ID','declarationTab',1,'p_declarationTab','parser.py',341),
  ('declarationTab -> ID EQUALS expressionTab','declarationTab',3,'p_declarationTab','parser.py',342),
  ('declarationTab -> ID LBRACKET INTEGER RBRACKET','declarationTab',4,'p_declarationTab','parser.py',343),
  ('expressionTab -> ID','expressionTab',1,'p_expressionTab','parser.py',356),
  ('expressionTab -> FLOAT','expressionTab',1,'p_expressionTab','parser.py',357),
  ('expressionTab -> INTEGER','expressionTab',1,'p_expressionTab','parser.py',358),
  ('expressionTab -> CHAR','expressionTab',1,'p_expressionTab','parser.py',359),
  ('factor -> ID LBRACKET expressionTab RBRACKET','factor',4,'p_array_access','parser.py',364),
  ('array_assignment -> ID LBRACKET expressionTab RBRACKET EQUALS expressionTab SEMICOLON','array_assignment',7,'p_array_assignment','parser.py',391),
  ('expression -> expression OR term','expression',3,'p_expression_el','parser.py',450),
  ('expression -> term','expression',1,'p_expression_el','parser.py',451),
  ('term -> term AND factor','term',3,'p_term_tl','parser.py',468),
  ('term -> factor','term',1,'p_term_tl','parser.py',469),
  ('factor -> ID','factor',1,'p_factor_fl','parser.py',489),
  ('factor -> TRUE','factor',1,'p_factor_fl','parser.py',490),
  ('factor -> FALSE','factor',1,'p_factor_fl','parser.py',491),
  ('factor -> NOT ID','factor',2,'p_factor_fl','parser.py',492),
  ('statement -> type ID EQUALS expression_arithmetique SEMICOLON','statement',5,'p_statement_assignmentArth','parser.py',634),
  ('statement -> ID EQUALS expression_arithmetique SEMICOLON','statement',4,'p_declarationArth','parser.py',639),
  ('expression_arithmetique -> expression_arithmetique PLUS term_arithmetique','expression_arithmetique',3,'p_expression_plus','parser.py',650),
  ('expression_arithmetique -> expression_arithmetique MINUS term_arithmetique','expression_arithmetique',3,'p_expression_minus','parser.py',655),
  ('expression_arithmetique -> term_arithmetique','expression_arithmetique',1,'p_expression_term','parser.py',659),
  ('term_arithmetique -> term_arithmetique MULTIPLY term_arithmetique','term_arithmetique',3,'p_term_multiply','parser.py',663),
  ('term_arithmetique -> term_arithmetique DIVIDE factor_arithmetique','term_arithmetique',3,'p_term_divide','parser.py',668),
  ('term_arithmetique -> factor_arithmetique','term_arithmetique',1,'p_term_factor','parser.py',672),
  ('factor_arithmetique -> ID','factor_arithmetique',1,'p_factor_id','parser.py',676),
  ('factor_arithmetique -> INTEGER','factor_arithmetique',1,'p_factor_id','parser.py',677),
  ('factor_arithmetique -> FLOAT','factor_arithmetique',1,'p_factor_id','parser.py',678),
  ('factor_arithmetique -> LPAREN expression_arithmetique RPAREN','factor_arithmetique',3,'p_factor_parens','parser.py',683),
  ('statement -> READ LPAREN ID RPAREN SEMICOLON','statement',5,'p_read_statement','parser.py',815),
  ('statement -> READ LPAREN ID LBRACKET INTEGER RBRACKET RPAREN SEMICOLON','statement',8,'p_read_statement','parser.py',816),
  ('statement -> WRITE LPAREN write_content RPAREN SEMICOLON','statement',5,'p_write_statement','parser.py',891),
  ('write_content -> write_item','write_content',1,'p_write_content_single','parser.py',896),
  ('write_content -> write_content COMMA write_item','write_content',3,'p_write_content_multiple','parser.py',900),
  ('write_item -> ID','write_item',1,'p_write_item','parser.py',906),
  ('write_item -> ID LBRACKET INTEGER RBRACKET','write_item',4,'p_write_item','parser.py',907),
  ('write_item -> STRING','write_item',1,'p_write_item','parser.py',908),
  ('statement -> FOR LPAREN initialisation COLON step COLON BorneSup RPAREN block','statement',9,'p_statement_FORloop','parser.py',939),
  ('initialisation -> ID EQUALS INTEGER','initialisation',3,'p_initialisation','parser.py',945),
  ('step -> INTEGER','step',1,'p_step','parser.py',954),
  ('step -> ID','step',1,'p_step','parser.py',955),
  ('BorneSup -> ID','BorneSup',1,'p_BorneSup','parser.py',963),
  ('block -> LBRACE statements RBRACE','block',3,'p_block','parser.py',968),
  ('statement -> IFTHEN','statement',1,'p_statementIf','parser.py',1064),
  ('statement -> IFTHENELSE','statement',1,'p_statementIf','parser.py',1065),
  ('IFTHENELSE -> IF LPAREN condition RPAREN block ELSE block','IFTHENELSE',7,'p_IFTHENELSE','parser.py',1072),
  ('IFTHEN -> IF LPAREN condition RPAREN block','IFTHEN',5,'p_IFTHEN','parser.py',1078),
  ('conditionIF -> IF LPAREN condition','conditionIF',3,'p_conditionIF','parser.py',1084),
  ('condition -> ID EQ factor','condition',3,'p_condition','parser.py',1090),
  ('condition -> ID NEQ factor','condition',3,'p_condition','parser.py',1091),
  ('condition -> ID LT factor','condition',3,'p_condition','parser.py',1092),
  ('condition -> ID LTE factor','condition',3,'p_condition','parser.py',1093),
  ('condition -> ID GT factor','condition',3,'p_condition','parser.py',1094),
  ('condition -> ID GTE factor','condition',3,'p_condition','parser.py',1095),
  ('condition -> factor EQ factor','condition',3,'p_condition','parser.py',1096),
  ('condition -> factor NEQ factor','condition',3,'p_condition','parser.py',1097),
  ('condition -> factor LT factor','condition',3,'p_condition','parser.py',1098),
  ('condition -> factor LTE factor','condition',3,'p_condition','parser.py',1099),
  ('condition -> factor GT factor','condition',3,'p_condition','parser.py',1100),
  ('condition -> factor GTE factor','condition',3,'p_condition','parser.py',1101),
]
