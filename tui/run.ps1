cargo +nightly build --release
mt.exe -nologo -manifest "tui/resources/ptscan_tui.exe.manifest" -outputresource:"target/release/ptscan-tui.exe;#1"
target/release/ptscan-tui.exe