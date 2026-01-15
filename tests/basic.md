# Basic search functionality

Setup test files:

    $ mkdir -p src
    $ echo "hello there world" > file.txt
    $ echo "foo bar" > src/code.txt
    $ echo "hello there again" > src/other.txt

Search for pattern (sorted for determinism):

    $ zg hello | sort
    file.txt:1:hello there world
    src/other.txt:1:hello there again

Search in specific path (no trailing slash to avoid double-slash):

    $ zg hello src
    src/other.txt:1:hello there again

No matches returns exit code 1:

    $ zg nonexistent
    [1]

Regex search:

    $ zg 'hel+o' | sort
    file.txt:1:hello there world
    src/other.txt:1:hello there again

Case insensitive:

    $ echo "HELLO UPPER" > upper.txt
    $ zg hello -i | sort
    file.txt:1:hello there world
    src/other.txt:1:hello there again
    upper.txt:1:HELLO UPPER
