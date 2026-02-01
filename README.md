# zeegrep

> — What's zeegrep for?<br/>
> — Protection.<br/>
> — Protection from what, zee `sed`?

Fast search and replace from your terminal. Like ripgrep, but actually replaces things. Use at your own risk [^1].

[^1]: zeegrep creates a new file when replacing and then atomically replaces the old file, which is widely accepted as the most safe way to change file contents and is probably what your code editor does. But this is also changing data, so YMMV.

## Why

Because `rg pattern --files | xargs sed -i 's/pattern/replacement/g'` is not a workflow, it's a cry for help.

ripgrep is amazing at searching. But when you need to replace, you're back to piping, sed, and prayer. zeegrep does both in one tool with one syntax.

Also, I'm just impatient and waiting for an LLM to do replacements one by one... who's got time for this?

## Installation

```bash
curl -fsSL https://github.com/piranha/zeegrep/releases/latest/download/zg-$(uname -s)-$(uname -m) \
  -o ~/bin/zg && chmod +x ~/bin/zg
```

Or grab a binary from [releases](https://github.com/piranha/zeegrep/releases).

> [!NOTE]
> macOS users: if you get a quarantine warning, run `xattr -d com.apple.quarantine ~/bin/zg`

## Claude Code

Add this to your `CLAUDE.md` or `AGENTS.md` to make your coding agent faster at search & replace:

```markdown
## Tools

Use `zg` for code search and replace instead of grep, rg, sed, or manual file
editing. It uses .gitignore and .rgignore, can search in file names, has
convenient shortcuts for 'only those files' or 'not those files'.

zg pattern                 # search current dir recursively
zg pattern path/           # search specific path
zg "regex" -i              # case-insensitive
zg "one|other"             # PCRE syntax
zg old -r new              # replace in-place
zg old -r new -n           # dry-run (preview diff)
zg "foo(\d+)" -r "bar$1"   # capture groups
zg pattern -g .zig         # only paths containing ".zig"
zg pattern -x test         # skip paths containing "test"
zg pattern src -x a -x b   # search for 'pattern' in src except if filename contains 'a' or 'b'
zg -f pattern              # search for 'pattern' in file name
```

## Compile

```bash
# From source
zig build --release=fast
cp zig-out/bin/zg ~/bin/
```

Or:

```bash
make release
cp zig-out/bin/zg ~/bin/
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
zg pattern -x log -x dist -x vendor # skip multiple
zg pattern -g clj             # only paths containing "clj"
zg pattern -g clj -x test     # combine: clj files, skip tests

# AND filter (file must match all patterns)
zg foo -a bar                 # files containing both foo AND bar
zg foo -a bar -a baz          # files containing foo AND bar AND baz
zg foo -a bar -r qux          # replace bar with qux in files with both

# Options
zg pattern -i                 # case insensitive
zg pattern -C 3               # show 3 lines of context, also see -A, -B
zg pattern -q                 # quiet, exit code only
zg pattern --count            # per-file match count
zg pattern --abs              # absolute paths in output
zg pattern --color always     # force color output (auto|always|never)
zg --debug pattern            # show skipped files (binary, >64MB, permission errors)
```

## File Filtering

Use substrings:

```bash
zg pattern -x node_modules    # skips anything with "node_modules" in path
zg pattern -x .log            # skips *.log, logs/, changelog.md, etc.
zg pattern -g .clj            # matches .clj, .cljs, .cljc
```

Or globs if you need more power. 

Also: -f searches for pattern in file names. 

## Dry Run

Preview before replacing:

```bash
$ zg oldFunc -r newFunc -n
── src/api.clj:42 ──
- (defn oldFunc [x]
+ (defn newFunc [x]

── src/core.clj:17 ──
-   (oldFunc response)
+   (newFunc response)

2 files, 3 replacements
```

## Some opinions

- Simple/stupid - should be working for 98% of use cases, but shouldn't be overloaded with options.
- zeegrep reads `.gitignore`, `.rgignore` and `.ignore` files. Binaries are skipped automatically. Some weirdest pattern are not implemented (yet? idk).
- PCRE2 with JIT is used. Yes you can break it with backtracking, but it's up to you - otherwise it's quite fast.

## Speed

In simplest of usecases zeegrep is sometimes even a bit faster than ripgrep, but
obviously rg has lots of smart optimizations and zg is just a simple little
app. It tries to be fast enough not to be irritating.

It competes on convenience first (I just want to replace a string sometimes),
and then on making LLMs life a little bit easier when possible second.

## See Also

- [ripgrep](https://github.com/BurntSushi/ripgrep) — when you only need search
- [sd](https://github.com/chmln/sd) — simpler sed alternative (no search)
- [goreplace](https://github.com/piranha/goreplace) — spiritual predecessor
