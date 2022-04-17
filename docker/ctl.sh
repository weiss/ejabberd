#!/bin/sh

# as_current_user) "$@" ;;
# exec_cmd "$ERL" ${S:--}name "$NODE" $ERLANG_OPTS "$@"
# exec_cmd "$IEX" -${S:--}name "$NODE" --erl "$ERLANG_OPTS" "$@"

CTLFILE=$(find /opt -name ejabberdctl)
CTLPATH=$(dirname $CTLFILE)
cd $CTLPATH
./ejabberdctl $*
