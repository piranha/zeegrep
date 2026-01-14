# Multiline search and replace

Setup test files:

    $ cat << 'EOF' > multi.txt
    > function foo() {
    >     return 1;
    > }
    >
    > function bar() {
    >     return 2;
    > }
    > EOF

## Newline escape in pattern

Match pattern spanning two lines:

    $ zg 'return 1;\n\}' multi.txt
    ── multi.txt:2-3 ──
    2│     return 1;
    3│ }

Match function with body:

    $ zg 'foo\(\) \{\n    return' multi.txt
    ── multi.txt:1-2 ──
    1│ function foo() {
    2│     return 1;

## Dotall mode (-M)

Without -M, dot doesn't match newline:

    $ zg 'foo.*bar' multi.txt
    [1]

With -M, dot matches newline (finds content spanning lines):

    $ zg -M 'foo\(\).*?return' multi.txt
    ── multi.txt:1-2 ──
    1│ function foo() {
    2│     return 1;

## Multiline replacement

Replace pattern spanning lines:

    $ cat << 'EOF' > funcs.txt
    > old {
    >     body
    > }
    > EOF
    $ zg 'old \{\n    body\n\}' -r 'new { updated }' funcs.txt -n
    ── funcs.txt:1-3 ──
    - old {
    -     body
    - }
    + new { updated }
    1 files, 1 replacements
    $ zg 'old \{\n    body\n\}' -r 'new { updated }' funcs.txt
    1 files, 1 replacements
    $ cat funcs.txt
    new { updated }
    $ zg 'new { updated }' -r 'old {\n    body\n}' funcs.txt -n
    ── funcs.txt:1 ──
    - new { updated }
    + old {
    +     body
    + }
    1 files, 1 replacements
    $ zg 'new { updated }' -r 'old {\n    body\n}' funcs.txt
    1 files, 1 replacements
    $ cat funcs.txt
    old {
        body
    }

## Count with multiline

    $ zg '\n' --count multi.txt
    multi.txt:7
