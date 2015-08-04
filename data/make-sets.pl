#!/usr/bin/perl
#
use strict;

my $linenum = 0;
my $infile = $ARGV[0];
my $total = `wc -l $infile`;
chomp($total);
print "Total lines is $total\n";
my @random_lines = make_random($total);
print "Random lines are " . scalar(@random_lines) . "\n";
open(IN, $infile);
open(TRAIN, "> pos-train.txt");
open(TEST, "> pos-test.txt");
while(<IN>) {
  $linenum++;
  if(grep(/^$linenum$/,@random_lines)) {
    print TEST $_;
  } else {
    print TRAIN $_;
  }
}
close(TEST);
close(TRAIN);
close(IN);

sub make_random {
  my($i) = @_;
  my(@r);
  until(scalar(@r) > (0.1 * $i)) {
    my $rand = 1 + int(rand($i));
    unless(grep(/^$rand$/, @r)) {
      unshift(@r,$rand);
    }
  }
  return(@r);
}
