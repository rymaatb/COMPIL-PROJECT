import unittest
from lexer import lexer, analyser_lexical


# Exécution du test
if __name__ == "__main__":
    code_example = """
    VAR_GLOBAL {
    lex
    Analyseuer4r
    Anal5
    }
    DECLARATION {
        
    }
    INSTRUCTION {
        %% Ce commentaire sera ignore
       
    }
    """
    analyser_lexical(code_example)
