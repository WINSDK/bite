#!/bin/bash
# Creates macos bundle in ./target/bite.app.

set -e
mkdir -p ./target/bite.app/Contents/MacOS
mkdir -p ./target/bite.app/Contents/Resources
cp ./target/release/bite ./target/bite.app/Contents/MacOS/bite
cp ./assets/iconx256.icns ./target/bite.app/Contents/Resources/shortcut.icns
cat > ./target/bite.app/Contents/Info.plist <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
	<dict>
		<key>CFBundleExecutable</key>
		<string>bite</string>
		<key>CFBundleIconFile</key>
		<string>shortcut.icns</string>
		<key>CFBundleInfoDictionaryVersion</key>
		<string>1.0</string>
		<key>CFBundlePackageType</key>
		<string>APPL</string>
		<key>CFBundleSignature</key>
		<string>????</string>
		<key>CFBundleVersion</key>
		<string>1.0</string>
	</dict>
</plist>
EOF
