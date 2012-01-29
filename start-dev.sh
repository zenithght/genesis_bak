#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin \
     -pa /Users/jack/Codes/ChicagoBoss-0.6.10/ebin \
     -pa /Users/jack/Codes/ChicagoBoss-0.6.10/deps/*/ebin \
     -boss developing_app genesis \
     -boot start_sasl -config boss -s reloader -s boss \
     -run server reset
     -sname wildbill
