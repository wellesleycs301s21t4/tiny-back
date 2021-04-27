# source this while in the top-level compiler directory
if [ -z "$TINY_HOME" ]
then
    export TINY_HOME="$(pwd)"
    export PATH="$(pwd)/bin:$PATH"
fi

test -e bin/tinyc || ./mill --no-server tinyc.script
