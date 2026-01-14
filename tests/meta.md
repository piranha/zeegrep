# Version and help

Version output:

    $ zg --version 2>&1 | grep -q zeegrep && echo "has version"
    has version

    $ zg -V 2>&1 | grep -q zeegrep && echo "has version"
    has version

Help output:

    $ zg --help 2>&1 | grep -q Usage && echo "has usage"
    has usage

    $ zg -h 2>&1 | grep -q Usage && echo "has usage"
    has usage

No args shows usage:

    $ zg 2>&1 | grep -q Usage && echo "has usage"
    has usage
