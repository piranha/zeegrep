# Option combinations

Setup:

    $ mkdir -p src .hidden
    $ echo "old value" > src/main.clj
    $ echo "old value" > src/test.clj
    $ echo "old value" > .hidden/secret.clj

Replace + quiet (modifies but no output):

    $ echo "foo" > quiet.txt
    $ zg foo -r bar -q quiet.txt
    $ cat quiet.txt
    bar

Replace + hidden (modifies hidden files):

    $ zg old -r new --hidden .hidden/
    1 files, 1 replacements
    $ cat .hidden/secret.clj
    new value

Multiple filters (-g and -x together, multiple -x):

    $ echo "match" > src/code.clj
    $ echo "match" > src/test_code.clj
    $ echo "match" > src/code.txt
    $ zg match -g .clj -x test src | sort
    src/code.clj:1:match

File names + hidden:

    $ zg secret -f --hidden | sort
    .hidden/secret.clj

Context + count (count ignores context):

    $ cat > ctx.txt << 'EOF'
    > line 1
    > match here
    > line 3
    > EOF
    $ zg match --count -C 2 ctx.txt
    ctx.txt:1

Replace dry-run + absolute paths:

    $ echo "old text" > abs.txt
    $ zg old -r new -n --abs abs.txt | head -1 | grep -q "^──.*/" && echo "has abs path"
    has abs path
