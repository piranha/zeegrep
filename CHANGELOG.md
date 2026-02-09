# Changelog

## v0.7 (2026-02-06)

- Add `-a`/`--and` option for multi-pattern filtering
- Fix double slashes when target path has trailing slash
- Update opt.zig with `--` support

## v0.6 (2026-01-19)

- Extract opt.zig as separate library
- Major regex optimization overhaul
- Regex literal optimization for faster pattern matching
- Literal string matching fast path
- Code quality improvements in run.zig

## v0.5 (2026-01-15)

- Single-threaded sorted mode with buffered stdout
- Limit parallelism for better throughput
- Stream match lines for better memory efficiency
- Fix options parsing errors
- Fix output format separators and empty context lines

## v0.4 (2026-01-15)

- SIMD-optimized line counting and shared counters
- Expand `$0` with literal patterns
- Remove locking from sorting/output
- Only hardcode VCS dirs (.git, .hg, .svn) in default skip
- Unify match collection and formatting
- Move Options to run.zig, single source of truth
- Diff: fail explicitly on files >4096 lines

## v0.3 (2026-01-14)

- Multiline matching
- Faster ignore file processing

## v0.2 (2026-01-14)

- Integration tests
- Windows mmap via NtCreateSection/NtMapViewOfSection
- Stream output when not sorting (default)
- `--debug` flag to show skipped files
- Per-job arena allocators to reduce mutex contention
- Unify glob matching: ignore.zig uses glob.match
- Fix parsing trailing options
- Fix crash

## v0.1 (2026-01-13)

- Initial release
- Recursive file search with regex (PCRE2)
- In-place search and replace with capture groups
- Glob-based path filtering (`-g`, `-x`)
- Context lines (`-B`, `-A`, `-C`)
- File counting (`--count`)
- mmap-based file reading
- Cross-platform: Linux, macOS, Windows
- Ripgrep-like hidden file handling
- `.gitignore` support
