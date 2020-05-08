#!/bin/bash

sbcl \
  --noinform \
  --disable-debugger \
  --eval '(ql:quickload "wolfram-image-test")' \
  --eval '(in-package :wolfram-image-test)' \
  --eval '(run-tests :all)' \
  --quit
