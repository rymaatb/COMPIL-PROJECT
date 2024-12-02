

from compiler import test_lexer


code = '''
        int a;
        char b[5];
        float c = 3.14;
        READ(c);
        WRITE(c);
        WRITE(b[2]);

        
'''



test_lexer(code)

