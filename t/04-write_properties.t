#!perl

use Test::More tests => 1 + 6 + 6 + 8 + 8 + 10 + 8 + 8 + 10 + 7;

use Music::Gestalt;
use MIDI;
use File::Spec::Functions qw(catfile);

# ('note_on', I<start_time>, I<duration>, I<channel>, I<note>, I<velocity>)

# These test verify reading and modifying the properties of a gestalt.

my $score = [
    ['note', 50,  50, 1, 20,  10],
    ['note', 150, 50, 1, 50,  32],
    ['note', 250, 50, 1, 90,  96],
    ['note', 350, 50, 1, 120, 117]];

my $g = Music::Gestalt->new(score => $score);
is_deeply($g->AsScore(), $score);

# --- PitchRange ---
my $pr = $g->PitchRange();
is($g->PitchRange(0), 0);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[4] = 70; \@a; } @$score]);
is($g->PitchRange(127), 127);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 0,   10],
        ['note', 150, 50, 1, 19,  32],
        ['note', 250, 50, 1, 121, 96],
        ['note', 350, 50, 1, 127, 117]]);

is($g->PitchRange($pr), $pr);
is_deeply($g->AsScore(), $score);

# --- VelocityRange ---
$g = Music::Gestalt->new(score => $score);
my $vr = $g->VelocityRange();
is($g->VelocityRange(0), 0);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[5] = 64; \@a; } @$score]);
is($g->VelocityRange(127), 127);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,  0],
        ['note', 150, 50, 1, 50,  0],
        ['note', 250, 50, 1, 90,  127],
        ['note', 350, 50, 1, 120, 127]]);
is($g->VelocityRange($vr), $vr);
is_deeply($g->AsScore(), $score);

# --- PitchLowest ---
$g = Music::Gestalt->new(score => $score);
my $pl = $g->PitchLowest();
is($g->PitchLowest(121), 120);
is($g->PitchLowest(-5),  0);
is($g->PitchLowest(120), 120);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[4] = 120; \@a; } @$score]);
is($g->PitchLowest(0), 0);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 0,   10],
        ['note', 150, 50, 1, 36,  32],
        ['note', 250, 50, 1, 84,  96],
        ['note', 350, 50, 1, 120, 117]]);
is($g->PitchLowest($pl), $pl);
is_deeply($g->AsScore(), $score);

# --- PitchHighest ---
$g = Music::Gestalt->new(score => $score);
my $ph = $g->PitchHighest();
is($g->PitchHighest(19),  20);
is($g->PitchHighest(128), 127);
is($g->PitchHighest(20),  20);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[4] = 20; \@a; } @$score]);
is($g->PitchHighest(127), 127);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,  10],
        ['note', 150, 50, 1, 52,  32],
        ['note', 250, 50, 1, 95,  96],
        ['note', 350, 50, 1, 127, 117]]);
is($g->PitchHighest($ph), $ph);
is_deeply($g->AsScore(), $score);

# --- PitchMiddle ---
$g = Music::Gestalt->new(score => $score);
my $pm = $g->PitchMiddle();
is($g->PitchMiddle(-1),  0);
is($g->PitchMiddle(128), 127);
is($g->PitchMiddle(80),  80);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 30,  10],
        ['note', 150, 50, 1, 60,  32],
        ['note', 250, 50, 1, 100, 96],
        ['note', 350, 50, 1, 127, 117]]);
is($g->PitchMiddle(127), 127);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 77,  10],
        ['note', 150, 50, 1, 107, 32],
        ['note', 250, 50, 1, 127, 96],
        ['note', 350, 50, 1, 127, 117]]);
is($g->PitchMiddle(0), 0);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 0,  10],
        ['note', 150, 50, 1, 0,  32],
        ['note', 250, 50, 1, 20, 96],
        ['note', 350, 50, 1, 50, 117]]);
is($g->PitchMiddle($pm), $pm);
is_deeply($g->AsScore(), $score);

# --- VelocityLowest ---
$g = Music::Gestalt->new(score => $score);
my $vl = $g->VelocityLowest();
is($g->VelocityLowest(118), 117);
is($g->VelocityLowest(-5),  0);
is($g->VelocityLowest(117), 117);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[5] = 117; \@a; } @$score]);
is($g->VelocityLowest(0), 0);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,  0],
        ['note', 150, 50, 1, 50,  24],
        ['note', 250, 50, 1, 90,  94],
        ['note', 350, 50, 1, 120, 117]]);
is($g->VelocityLowest($vl), $vl);
is_deeply($g->AsScore(), $score);

# --- VelocityHighest ---
$g = Music::Gestalt->new(score => $score);
my $vh = $g->VelocityHighest();
is($g->VelocityHighest(9),   10);
is($g->VelocityHighest(128), 127);
is($g->VelocityHighest(10),  10);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[5] = 10; \@a; } @$score]);
is($g->VelocityHighest(127), 127);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,  10],
        ['note', 150, 50, 1, 50,  34],
        ['note', 250, 50, 1, 90,  104],
        ['note', 350, 50, 1, 120, 127]]);
is($g->VelocityHighest($vh), $vh);
is_deeply($g->AsScore(), $score);

# --- VelocityMiddle ---
$g = Music::Gestalt->new(score => $score);
my $vm = $g->VelocityMiddle();
is($g->VelocityMiddle(-1),  0);
is($g->VelocityMiddle(128), 127);
is($g->VelocityMiddle(73),  73);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,  20],
        ['note', 150, 50, 1, 50,  42],
        ['note', 250, 50, 1, 90,  106],
        ['note', 350, 50, 1, 120, 127]]);
is($g->VelocityMiddle(127), 127);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,   74],
        ['note', 150, 50, 1, 50,   96],
        ['note', 250, 50, 1, 90,  127],
        ['note', 350, 50, 1, 120, 127]]);
is($g->VelocityMiddle(0), 0);
is_deeply(
    $g->AsScore(),
    [
        ['note', 50,  50, 1, 20,  0],
        ['note', 150, 50, 1, 50,  0],
        ['note', 250, 50, 1, 90,  33],
        ['note', 350, 50, 1, 120, 54]]);
is($g->VelocityMiddle($vm), $vm);
is_deeply($g->AsScore(), $score);

# --- Duration ---
$g = Music::Gestalt->new(score => $score);
my $d = $g->Duration();
is($d, 400);
is($g->Duration(-1), 0);
is($g->Duration($d * 2), $d * 2);
is_deeply($g->AsScore(), [
    ['note', 100, 100, 1, 20,  10],
    ['note', 300, 100, 1, 50,  32],
    ['note', 500, 100, 1, 90,  96],
    ['note', 700, 100, 1, 120, 117]]);
is($g->Duration(0), 0);
is_deeply($g->AsScore(), [map { my @a = @$_; $a[1] = $a[2] = 0; \@a; } @$score]);
$g->Duration($d);
is_deeply($g->AsScore(), $score);