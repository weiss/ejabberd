#!/bin/bash

CTLFILE=$(find /opt -name ejabberdctl)
exec $CTLFILE "$@"
