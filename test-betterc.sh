#!/bin/sh
DFLAGS="-preview=fieldwise"
dmd $DFLAGS -unittest -g -betterC -I=src -i -run test_betterc.d
