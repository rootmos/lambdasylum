#!/bin/sh

rlwrap \
    --extra-char-after-completion='' \
    --break-chars='.(){}' \
    -z ./rlwrap-replace-escaped-lambdas.pl \
    $*
