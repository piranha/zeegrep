# Regex alternation

Setup:

    $ echo "clock_jitter" > data.txt
    $ echo "Clock Jitter" >> data.txt
    $ echo "clockJitter" >> data.txt
    $ echo "no match here" >> data.txt

Top-level alternation:

    $ zg 'clock_jitter|Clock Jitter|clockJitter' data.txt | sort
    data.txt:1:clock_jitter
    data.txt:2:Clock Jitter
    data.txt:3:clockJitter

Two-branch alternation:

    $ zg 'alpha|beta' data.txt
    [1]

    $ echo "alpha test" >> data.txt
    $ echo "beta test" >> data.txt
    $ zg 'alpha|beta' data.txt | sort
    data.txt:5:alpha test
    data.txt:6:beta test

Alternation inside group:

    $ zg '(alpha|beta) test' data.txt | sort
    data.txt:5:alpha test
    data.txt:6:beta test

Alternation with shared suffix:

    $ zg '(foo|bar)baz' data.txt
    [1]

    $ echo "foobaz" >> data.txt
    $ echo "barbaz" >> data.txt
    $ zg '(foo|bar)baz' data.txt | sort
    data.txt:7:foobaz
    data.txt:8:barbaz

Case-insensitive alternation:

    $ zg 'ALPHA|BETA' -i data.txt | sort
    data.txt:5:alpha test
    data.txt:6:beta test

Escaped pipe is literal, not alternation:

    $ echo "a|b" >> data.txt
    $ zg 'a\|b' data.txt
    data.txt:9:a|b
