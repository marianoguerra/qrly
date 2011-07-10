#!/usr/bin/env sh

echo "compiling all"
./build.sh
cd ../ebin

function runTest {
 echo "testing $1"
	erl -pa ./ -run $1 test -run init stop -noshell
}

runTest qrly
runTest qrly_html
