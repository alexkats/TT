#!/bin/sh

stack clean

if ! stack build
then
    exit
fi

./run
