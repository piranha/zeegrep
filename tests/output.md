# Output options: --quiet, --count, --abs, --sort

Setup:

    $ mkdir -p src
    $ echo "match one" > aaa.txt
    $ echo "match two" > src/bbb.txt
    $ echo "no hit" > src/ccc.txt

Quiet mode (-q): only exit code, no output

    $ zg match -q
    $ echo "exit: $?"
    exit: 0

    $ zg nonexistent -q
    [1]

Count mode (--count):

    $ zg match --count | sort
    aaa.txt:1
    src/bbb.txt:1

Multiple matches per file:

    $ echo "match match match" > multi.txt
    $ zg match --count multi.txt
    multi.txt:3

Absolute paths (--abs):

    $ zg match --abs aaa.txt | grep -c "^/"
    1

Sort (--sort) - deterministic output order:

    $ zg match --sort
    aaa.txt:1:match one
    multi.txt:1:match match match
    src/bbb.txt:1:match two
