cargo +nightly build --release
mt.exe -nologo -manifest "main.exe.manifest" -outputresource:"target/release/ptscan.exe;#1"
target/release/ptscan.exe --process GTA5.exe