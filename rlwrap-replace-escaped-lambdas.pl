#!/usr/bin/env perl

use lib ($ENV{RLWRAP_FILTERDIR} or ".");
use RlwrapFilter;
use strict;

my $filter = new RlwrapFilter;

$filter -> input_handler(\&expand_symbols);
$filter -> history_handler(\&expand_symbols);
$filter -> completion_handler(\&complete_symbol);
$filter -> run;

sub expand_symbols {
    my ($i) = @_;

    my $lambda_re = qr/\\l(ambda|ambd|amb|am|a|)/;
    my $Lambda_re = qr/\\L(ambda|ambd|amb|am|a|)/;
    my $bottom_re = qr/\\b(ottom|otto|ott|ot|o|)/;
    my $forall_re = qr/\\f(orall|oral|ora|or|o|)/;

    $i =~ s/$lambda_re/λ/;
    $i =~ s/$Lambda_re/Λ/;
    $i =~ s/$bottom_re/⊥/;
    $i =~ s/$forall_re/∀/;

    return $i;
}

sub complete_symbol {
    my($line, $prefix, @completions) = @_;

    my $lambda_re = qr/\\l(ambda|ambd|amb|am|a|)/;
    my $Lambda_re = qr/\\L(ambda|ambd|amb|am|a|)/;
    my $bottom_re = qr/\\b(ottom|otto|ott|ot|o|)/;
    my $forall_re = qr/\\f(orall|oral|ora|or|o|)/;

    if ($prefix =~ /$lambda_re$/) {
        $prefix =~ s/$lambda_re$/λ/;
        unshift @completions, $prefix;
    }

    if ($prefix =~ /$Lambda_re$/) {
        $prefix =~ s/$Lambda_re$/Λ/;
        unshift @completions, $prefix;
    }

    if ($prefix =~ /$bottom_re$/) {
        $prefix =~ s/$bottom_re$/⊥/;
        unshift @completions, $prefix;
    }

    if ($prefix =~ /$forall_re$/) {
        $prefix =~ s/$forall_re$/∀/;
        unshift @completions, $prefix;
    }

    return @completions;
}
