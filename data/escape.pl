#!/usr/bin/perl
#
use strict;

while(<>) {
  unless(/^\*x\*/) {
    s/\./\\\./g;
    s/\,/\\\,/g;
    s/\`/\\\`/g;
    s/\'/\\\'/g;
    s/\:/\\\:/g;
    s/\;/\\\;/g;
    s/\#/\\\#/g;
    s/\|/\\\|/g;
    print;
  }
}
