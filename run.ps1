cargo +nightly build --release
mt.exe -nologo -manifest "resources/ptscan.exe.manifest" -outputresource:"target/release/ptscan.exe;#1"
target/release/ptscan.exe --process GTA5.exe
# target/release/ptscan.exe --process pttarget.exe