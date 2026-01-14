# Hidden files (--hidden)

Setup:

    $ echo "match visible" > visible.txt
    $ echo "match hidden" > .hidden.txt
    $ mkdir -p .hiddendir
    $ echo "match in hidden dir" > .hiddendir/file.txt

Without --hidden, hidden files are skipped:

    $ zg match | sort
    visible.txt:1:match visible

With --hidden, hidden files are included:

    $ zg match --hidden | sort
    .hidden.txt:1:match hidden
    .hiddendir/file.txt:1:match in hidden dir
    visible.txt:1:match visible
