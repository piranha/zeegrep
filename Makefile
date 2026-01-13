.PHONY: build test release clean

build:
	zig build

test:
	zig build test

release:
	zig build --release=fast

clean:
	rm -rf .zig-cache zig-out
