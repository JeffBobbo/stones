#!/usr/bin/perl

use warnings;
use strict;

sub printState
{
  my @s = @{shift()};
  return if (@s == 0);
  print '[' . join(', ', @s) . ']' . "\n";
}

sub take
{
  my $n = shift();
  my $p = shift();
  my @s = @{shift()};

  $s[$p] -= $n;
  if ($s[$p] == 0)
  {
    splice(@s, $p, 1);
    return \@s;
  }
  return $s[$p] >= 0 ? \@s : undef;
}

sub move
{
  my @state = @{shift()};
  my $n = shift();
  for (my $i = 0; $i < @state; ++$i)
  {
    my $s = take($n, $i, \@state);
    next if (!defined $s);
    printState($s);
    move($s, $n+1);
  }
}

move([3, 2, 1], 1);
#printState(take(1, 0, [3, 2, 1]));