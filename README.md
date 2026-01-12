# zeegrep

> — What’s zeegrep for?  
> — Protection.  
> — Protection from what, ze sed commands?

Fast search and replace from your terminal. Like ripgrep, but actually replaces things. Use at your own risk.

## Why

Because `rg pattern | xargs sed -i 's/pattern/replacement/g'` is not a workflow, it’s a cry for help.

ripgrep is amazing at searching. But when you need to replace, you’re back to piping, sed, and prayer. zeegrep does both in one tool with one syntax.

## Compile

```bash
# From source
zig build --release=fast
cp zig-out/bin/zg ~/.local/bin/
```

## Usage

```bash
# Search
zg pattern                    # search in current directory
zg pattern src/               # search in specific path
zg 'fn\s+\w+' src/            # regex search

# Replace
zg old -r new                 # replace in-place
zg old -r new -n              # dry-run, show diff
zg 'foo(\d+)' -r 'bar$1'      # with capture groups

# Filter files (simple substring matching, no globs!)
zg pattern -x test            # skip paths containing "test"
zg pattern -x log,dist,vendor # skip multiple
zg pattern -o clj             # only paths containing "clj"
zg pattern -o clj -x test     # combine: clj files, skip tests

# Options
zg pattern -i                 # case insensitive
zg pattern -C 3               # show 3 lines of context
zg pattern -q                 # quiet, exit code only
zg pattern --count            # just show match count
zg pattern --abs              # absolute paths in output
```

## File Filtering

Use substrings:

```bash
zg pattern -x node_modules    # skips anything with "node_modules" in path
zg pattern -x .log            # skips *.log, logs/, changelog.md, etc.
zg pattern -g .clj            # matches .clj, .cljs, .cljc
```

Or globs if you need more power. 

## Dry Run

Preview before replacing:

```bash
$ zg oldFunc -r newFunc -n

=== src/api.clj:42
- (defn oldFunc [x]
+ (defn newFunc [x]

=== src/core.clj:17
-   (oldFunc response)
+   (newFunc response)

2 files, 3 replacements
```

Standard unified diff format. Pipe to `patch` if you’re feeling adventurous.

## Respects Your Ignores

zeegrep reads `.gitignore` and `.ignore` files. Binaries are skipped automatically.

## Speed

Fast enough. Uses Boyer-Moore for literal patterns, compiles regexes once, parallelizes file search.

Will it beat ripgrep? No. Andrew Gallant has mass and reach, we don’t compete.

But it’s fast enough that you won’t notice, and it actually does the thing you needed.

## See Also

- [ripgrep](https://github.com/BurntSushi/ripgrep) — when you only need search
- [sd](https://github.com/chmln/sd) — simpler sed alternative (no search)
- [goreplace](https://github.com/piranha/goreplace) — spiritual predecessor