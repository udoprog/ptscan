cargo +nightly build
mt.exe -nologo -manifest "src/main.exe.manifest" -outputresource:"target/debug/ptscan.exe;#1"
target/debug/ptscan.exe --process Steam.exe