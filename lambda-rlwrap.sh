#!/bin/sh

rlwrap \
    --extra-char-after-completion='' \
    --break-chars='.(){}' \
    --filter ./rlwrap-replace-escaped-lambdas.pl \
    $*
