.PHONY: build test itest release clean ciall

build:
	zig build

test:
	zig build test
	quizzig --bindir=zig-out/bin tests/*.md

release:
	zig build --release=fast

clean:
	rm -rf .zig-cache zig-out

ifndef VERSION
ciall:
	$(error VERSION is not set)
else
ciall:
	zig build --release=fast -Dversion=${VERSION} -Dtarget=x86_64-linux-musl  -Doutput=dist/zg-Linux-x86_64
	zig build --release=fast -Dversion=${VERSION} -Dtarget=aarch64-linux-musl -Doutput=dist/zg-Linux-aarch64
	zig build --release=fast -Dversion=${VERSION} -Dtarget=x86_64-macos       -Doutput=dist/zg-Darwin-x86_64
	zig build --release=fast -Dversion=${VERSION} -Dtarget=aarch64-macos      -Doutput=dist/zg-Darwin-arm64
	zig build --release=fast -Dversion=${VERSION} -Dtarget=x86_64-windows     -Doutput=dist/zg-Windows-x86_64.exe
endif
