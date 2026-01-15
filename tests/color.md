# Color output (--color)

Setup:

    $ echo "hello there world" > test.txt

Color never - no escape sequences:

    $ zg hello --color never test.txt | cat -v
    test.txt:1:hello there world

Color always - output contains escape sequences (^[ shown by cat -v):

    $ zg hello --color always test.txt | cat -v | grep -q '\^' && echo "has escapes"
    has escapes
