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
