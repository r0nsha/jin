#!/bin/sh
tcc -c jinrt.c -ojinrt.o
tcc -ar libjinrt.a jinrt.o
rm jinrt.o
