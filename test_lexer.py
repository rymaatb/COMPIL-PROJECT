

from compiler import test_lexer


code = '''
        %% Variable declarations
        int a;
        float b = 4.2;
        char c = 'X';
        int arr[2];
        const int MAX = 100;

        %% Assignments and expressions
        a = 10;
        b = a + 3.5;

        %% Read and write operations
        READ(a);
        WRITE(a, b, c);

        
'''



test_lexer(code)

