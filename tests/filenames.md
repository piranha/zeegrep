# File name matching (-f)

Setup:

    $ mkdir -p src/components
    $ echo "content" > src/Button.tsx
    $ echo "content" > src/components/Modal.tsx
    $ echo "content" > readme.md

Match against file names:

    $ zg Button -f
    src/Button.tsx

Regex in file name:

    $ zg '\.tsx$' -f | sort
    src/Button.tsx
    src/components/Modal.tsx

Case insensitive file name:

    $ zg README -f -i
    readme.md

Combine with path filter:

    $ zg tsx -f -g components
    src/components/Modal.tsx
