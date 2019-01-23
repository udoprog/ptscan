cargo +nightly build --release
mt.exe -nologo -manifest "src/main.exe.manifest" -outputresource:"target/debug/ptscan.exe;#1"
target/debug/ptscan.exe --process GTA5.exe
# target/debug/ptscan.exe --process pttarget.exe