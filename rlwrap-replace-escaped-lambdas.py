#!/usr/bin/env python3

import sys
import os

if 'RLWRAP_FILTERDIR' in os.environ:
    sys.path.append(os.environ['RLWRAP_FILTERDIR'])
else:
    sys.path.append('.')

import re

lambda_re = "\\\\l(ambda|ambd|amb|am|a|)"
Lambda_re = "\\\\L(ambda|ambd|amb|am|a|)"
bottom_re = "\\\\b(ottom|otto|ott|ot|o|)"
forall_re = "\\\\f(orall|oral|ora|or|o|)"

lambda_end_re = lambda_re + "$"
Lambda_end_re = Lambda_re + "$"
bottom_end_re = bottom_re + "$"
forall_end_re = forall_re + "$"

def expand_symbols(i):
    i = re.sub(lambda_re,"λ",i)
    i = re.sub(Lambda_re,"Λ",i)
    i = re.sub(bottom_re,"⊥",i)
    i = re.sub(forall_re,"∀",i)
    return i

def complete_symbol(line, prefix, completions):
    if re.search(lambda_end_re, prefix):
        return [re.sub(lambda_end_re, 'λ', prefix)] + completions
    if re.search(Lambda_end_re, prefix):
        return [re.sub(Lambda_end_re, "Λ", prefix)] + completions
    if re.search(bottom_end_re, prefix):
        return [re.sub(bottom_end_re, "⊥", prefix)] + completions
    if re.search(forall_end_re, prefix):
        return [re.sub(forall_end_re, "∀", prefix)] + completions

    return completions

import rlwrapfilter

filter = rlwrapfilter.RlwrapFilter()
filter.input_handler = expand_symbols
filter.completion_handler = complete_symbol
filter.run()
