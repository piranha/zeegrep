# Replace functionality

Setup:

    $ echo "old value here" > test.txt
    $ cat << EOF > multi.txt
    > old and new
    > little and small
    > new and old
    > EOF

Dry run shows diff without modifying (sorted for determinism):

    $ zg old -r new -n --sort
    ── multi.txt:1 ──
    - old and new
    + new and new
    ── multi.txt:3 ──
    - new and old
    + new and new
    ── test.txt:1 ──
    - old value here
    + new value here
    2 files, 3 replacements

    $ cat test.txt
    old value here

Actual replace modifies files:

    $ zg old -r new
    2 files, 3 replacements
    $ cat test.txt
    new value here
    $ cat multi.txt
    new and new
    little and small
    new and new

Replace with capture groups:

    $ echo "foo123 and foo456" > capture.txt
    $ zg 'foo(\d+)' -r 'bar$1' capture.txt
    1 files, 2 replacements
    $ cat capture.txt
    bar123 and bar456
