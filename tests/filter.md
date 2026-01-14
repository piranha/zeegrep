# Path filtering with -g and -x

Setup:

    $ mkdir -p src test
    $ echo "match" > src/code.clj
    $ echo "match" > src/code.cljs
    $ echo "match" > test/code.clj
    $ echo "match" > other.txt

Include filter (-g):

    $ zg match -g .clj | sort
    src/code.clj:1:match
    src/code.cljs:1:match
    test/code.clj:1:match

Exclude filter (-x):

    $ zg match -x test | sort
    other.txt:1:match
    src/code.clj:1:match
    src/code.cljs:1:match

Combine include and exclude:

    $ zg match -g .clj -x test | sort
    src/code.clj:1:match
    src/code.cljs:1:match
