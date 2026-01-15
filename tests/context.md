# Context lines

Setup:

    $ cat > file.txt << 'EOF'
    > line 1
    > line 2
    > match here
    > line 4
    > line 5
    > EOF

Context after (-A):

    $ zg match -A 1 file.txt
    file.txt:3:match here
    file.txt-4-line 4

Context before (-B):

    $ zg match -B 1 file.txt
    file.txt-2-line 2
    file.txt:3:match here

Context both (-C):

    $ zg match -C 1 file.txt
    file.txt-2-line 2
    file.txt:3:match here
    file.txt-4-line 4

Setup multi.txt:

    $ cat > multi.txt << 'EOF'
    > match 1
    > line 2
    > line 3
    > line 4
    > match 2
    > EOF

Non-contiguous matches without context:

    $ zg match multi.txt
    multi.txt:1:match 1
    multi.txt:5:match 2

Non-contiguous matches with context:

    $ zg match -C 1 multi.txt
    multi.txt:1:match 1
    multi.txt-2-line 2
    --
    multi.txt-4-line 4
    multi.txt:5:match 2

Separator between files with context:

    $ echo "match" > file1.txt
    $ echo "match" > file2.txt
    $ zg match -C 0 --sort file1.txt file2.txt | cat
    file1.txt:1:match
    file2.txt:1:match
    $ zg match -C 1 --sort file1.txt file2.txt | cat
    file1.txt:1:match
    --
    file2.txt:1:match

Newlines still being output

    $ echo "match\n\nother string" > newline.txt
    $ zg match -C 1 newline.txt
    newline.txt:1:match
    newline.txt-2-
