# Debug mode (--debug)

Setup - create a binary file:

    $ echo "match text" > text.txt
    $ printf '\x00\x01\x02binary\x03' > binary.bin

Without --debug, binary silently skipped:

    $ zg match
    text.txt:1:match text

With --debug, shows skipped files:

    $ zg match --debug 2>&1 | grep -c "binary.bin"
    1

Debug output goes to stderr:

    $ zg match --debug 2>/dev/null
    text.txt:1:match text
