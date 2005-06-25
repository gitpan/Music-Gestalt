package Music::Gestalt;

use warnings;
use strict;
use fields
  qw (notes duration pitch_base pitch_extent velocity_base velocity_extent pitches pitches_count);
use 5.0061;

=head1 NAME

Music::Gestalt - Compose music using gestalts.

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.02';

=head1 SYNOPSIS

    use Music::Gestalt;
    
    # see below

=head1 DESCRIPTION

This module helps to compose music using musical gestalts (forms). A gestalt is
similar to a list in L<MIDI::Score> format, but so far it only supports note
events, and all parameters are expressed as values between 0 and 1. This allows
for more flexible transformations of the musical material.

=head1 CONSTRUCTOR

=head2 C<new>

  my $g = Music::Gestalt->new(score => $score);

Creates a new Music::Gestalt object. The optional score argument receives
a score in L<MIDI::Score> format.

=cut

sub new {
    my $class  = shift;
    my %params = @_;
    my $self   = fields::new($class);

    $self->{pitches_count} = 0;
    $self->_InitializeFromScore($params{score})
      if (ref $params{score} eq 'ARRAY');

    return $self;
}

# private methods

sub _InitializeFromScore {
    my ($self, $score) = @_;

    my ($max_time, $min_pitch, $max_pitch, $min_velocity, $max_velocity);

    # find min/max values
    # #0: 'note'
    # #1: start time
    # #2: duration
    # #3: channel
    # #4: note
    # #5: velocity
    foreach (@$score) {
        next unless $_->[0] eq 'note';

        # min_time is always 0
        $max_time = $_->[1] + $_->[2]
          if (!defined $max_time || $_->[1] + $_->[2] > $max_time);
        $min_pitch = $_->[4]
          if (!defined $min_pitch || $_->[4] < $min_pitch);
        $max_pitch = $_->[4]
          if (!defined $max_pitch || $_->[4] > $max_pitch);
        $min_velocity = $_->[5]
          if (!defined $min_velocity || $_->[5] < $min_velocity);
        $max_velocity = $_->[5]
          if (!defined $max_velocity || $_->[5] > $max_velocity);
    }

    $self->{pitch_base}   = $min_pitch;
    $self->{pitch_extent} = defined $min_pitch
      && defined $max_pitch ? $max_pitch - $min_pitch : undef;
    $self->{velocity_base}   = $min_velocity;
    $self->{velocity_extent} = defined $min_velocity
      && defined $max_velocity ? $max_velocity - $min_velocity : undef;
    $self->{duration} = $max_time || 0;
    $self->{notes}    = [];

    return if (!defined $max_time || $max_time == 0);

    my @notes           = ();
    my $pitch_extent    = ($max_pitch - $min_pitch);
    my $velocity_extent = ($max_velocity - $min_velocity);
    foreach (@$score) {
        next unless $_->[0] eq 'note';
        push @notes,
          [
            $_->[1] / $max_time,
            $_->[2] / $max_time,
            ($_->[3] - 1) / 15,
            $pitch_extent == 0    ? 0 : ($_->[4] - $min_pitch) / $pitch_extent,
            $velocity_extent == 0 ? 0 :
              ($_->[5] - $min_velocity) / $velocity_extent];
    }

    $self->{notes} = [@notes];
}

sub _min {
    my ($a, $b) = @_;

    return $a < $b ? $a : $b;
}

sub _max {
    my ($a, $b) = @_;

    return $a > $b ? $a : $b;
}

sub _CalcPitch {
    my ($self, $v) = @_;

    return _max(0,
        _min(int($self->{pitch_base} + $v * $self->{pitch_extent} + 0.5), 127));
}

sub _CalcVelocity {
    my ($self, $v) = @_;

    return _max(
        0,
        _min(
            int($self->{velocity_base} + $v * $self->{velocity_extent} + 0.5),
            127));
}

=head1 PROPERTIES

=head2 C<PitchLowest>

Sets (if you pass a value) and returns lowest pitch used in this gestalt.

=cut

sub PitchLowest {
    my ($self, $v) = @_;

    if (defined $v) {
        $v = _max(0, _min($self->{pitch_base} + $self->{pitch_extent}, $v));
        $self->{pitch_extent} =
          $self->{pitch_base} + $self->{pitch_extent} - $v;
        $self->{pitch_base} = $v;
    }

    return $self->{pitch_base};
}

=head2 C<PitchHighest>

Sets (if you pass a value) and returns highest pitch used in this gestalt.

=cut

sub PitchHighest {
    my ($self, $v) = @_;

    return undef
      unless defined $self->{pitch_base} && defined $self->{pitch_extent};

    if (defined $v) {
        $v = _min(127, _max($self->{pitch_base}, $v));
        $self->{pitch_extent} = $v - $self->{pitch_base};
    }

    return $self->{pitch_base} + $self->{pitch_extent};
}

=head2 C<PitchMiddle>

Sets (if you pass a value) and returns middle pitch of the range used in this gestalt.

=cut

sub PitchMiddle {
    my ($self, $v) = @_;

    return undef
      unless defined $self->{pitch_base} && defined $self->{pitch_extent};

    if (defined $v) {
        $v = _min(_max($v, 0), 127);
        my $pm_old = (2 * $self->{pitch_base} + $self->{pitch_extent}) / 2;
        $self->{pitch_base} += $v - $pm_old;
    }

    return (2 * $self->{pitch_base} + $self->{pitch_extent}) / 2;
}

=head2 C<PitchRange>

Returns the pitch range used in this gestalt, ie. the pitches that occur
will be between pitch middle +/- pitch range.

If you pass a value as parameter, the new pitch range will be calculated
around the current pitch middle.

=cut

sub PitchRange {
    my ($self, $value) = @_;

    return undef unless defined $self->{pitch_base};

    if (defined $value) {
        my $old_range = $self->{pitch_extent} / 2;
        $self->{pitch_extent} = 2 * $value;
        $self->{pitch_base} += $old_range - $value;
    }

    return undef unless defined $self->{pitch_extent};
    return $self->{pitch_extent} / 2;
}

=head2 C<VelocityLowest>

Sets (if you pass a value) and returns lowest velocity used in this gestalt.

=cut

sub VelocityLowest {
    my ($self, $v) = @_;

    if (defined $v) {
        $v =
          _max(0, _min($self->{velocity_base} + $self->{velocity_extent}, $v));
        $self->{velocity_extent} =
          $self->{velocity_base} + $self->{velocity_extent} - $v;
        $self->{velocity_base} = $v;
    }

    return $self->{velocity_base};
}

=head2 C<VelocityHighest>

Sets (if you pass a value) and returns highest velocity used in this gestalt.

=cut

sub VelocityHighest {
    my ($self, $v) = @_;

    return undef
      unless defined $self->{velocity_base} && defined $self->{velocity_extent};

    if (defined $v) {
        $v = _min(127, _max($self->{velocity_base}, $v));
        $self->{velocity_extent} = $v - $self->{velocity_base};
    }

    return $self->{velocity_base} + $self->{velocity_extent};
}

=head2 C<VelocityMiddle>

Sets (if you pass a value) and returns middle velocity of the range used in this gestalt.

=cut

sub VelocityMiddle {
    my ($self, $v) = @_;

    return undef
      unless defined $self->{velocity_base} && defined $self->{velocity_extent};

    if (defined $v) {
        $v = _min(_max($v, 0), 127);
        my $vm_old =
          (2 * $self->{velocity_base} + $self->{velocity_extent}) / 2;
        $self->{velocity_base} += $v - $vm_old;
    }

    return (2 * $self->{velocity_base} + $self->{velocity_extent}) / 2;
}

=head2 C<VelocityRange>

Returns the velocity range used in this gestalt, ie. the velocities that occur
will be between velocity middle +/- velocity range.

If you pass a value as parameter, the new velocity range will be calculated
around the current velocity middle.

=cut

sub VelocityRange {
    my ($self, $value) = @_;

    return undef unless defined $self->{velocity_base};

    if (defined $value) {
        my $old_range = $self->{velocity_extent} / 2;
        $self->{velocity_extent} = 2 * $value;
        $self->{velocity_base} += $old_range - $value;
    }

    return undef unless defined $self->{velocity_extent};
    return $self->{velocity_extent} / 2;
}

=head2 C<Duration>

Returns the duration of this gestalt.

=cut

sub Duration {
    my $self = shift;

    return $self->{duration} || 0;
}

=head2 C<Notes>

Returns the note list of this gestalt.

=cut

sub Notes {
    my $self = shift;

    return @{[]} unless ref $self->{notes} eq 'ARRAY';
    return @{$self->{notes}};
}

=head1 METHODS

=head2 C<AsScore>

Returns a structure representing the gestalt in L<MIDI::Score> format.

=cut

sub AsScore {
    my $self = shift;

    my @score = ();
    foreach (@{$self->{notes}}) {
        push @score,
          [
            'note',                      $_->[0] * $self->{duration},
            $_->[1] * $self->{duration}, int(($_->[2] * 15 + 0.5) + 1),
            $self->_CalcPitch($_->[3]),  $self->_CalcVelocity($_->[4])];
    }

    return [@score];
}

=head2 C<Append>

Appends other Music::Gestalt objects to this object

=cut

sub Append {
    my $self = shift;

    # 1. Find out lowest/highest pitch and velocity overall
    my $pitch_lowest     = $self->PitchLowest();
    my $pitch_highest    = $self->PitchHighest();
    my $velocity_lowest  = $self->VelocityLowest();
    my $velocity_highest = $self->VelocityHighest();
    my $duration         = $self->Duration() || 0;

    @_ = grep { UNIVERSAL::isa($_, 'Music::Gestalt') } @_;
    foreach (@_) {
        $pitch_lowest = $_->PitchLowest()
          if (!defined $pitch_lowest || $_->PitchLowest() < $pitch_lowest);
        $pitch_highest = $_->PitchHighest()
          if (!defined $pitch_highest || $_->PitchHighest() > $pitch_highest);
        $velocity_lowest = $_->VelocityLowest()
          if (!defined $velocity_lowest
            || $_->VelocityLowest() < $velocity_lowest);
        $velocity_highest = $_->VelocityHighest()
          if (!defined $velocity_highest
            || $_->VelocityHighest() > $velocity_highest);
        $duration += $_->Duration;
    }

    return
      if ( !defined $pitch_lowest
        || !defined $pitch_highest
        || !defined $velocity_lowest
        || !defined $velocity_highest);

    my $pitch_extent    = $pitch_highest - $pitch_lowest;
    my $velocity_extent = $velocity_highest - $velocity_lowest;

    # 2. Transform notes in this gestalt to new pitch
    foreach (@{$self->{notes}}) {

        # start time, duration, pitch, velocity
        $_->[0] = $_->[0] * $self->{duration} / $duration;
        $_->[1] = $_->[1] * $self->{duration} / $duration;

        # channel stays as it is
        $_->[3] =
          $pitch_extent == 0 ? 0 :
          (
            (
                $self->{pitch_base} - $pitch_lowest + $self->{pitch_extent} *
                  $_->[3]) / $pitch_extent);
        $_->[4] =
          $velocity_extent == 0 ? 0 :
          (
            (
                $self->{velocity_base} - $velocity_lowest +
                  $self->{velocity_extent} * $_->[4]) / $velocity_extent);
    }

    # 3. Transform and append notes in the gestalts to be appended
    my $time_pos = $self->{duration} || 0;

    foreach my $g (@_) {
        my $time_delta      = $time_pos / $duration;
        my $gpitchextent    = $g->PitchHighest() - $g->PitchLowest();
        my $gvelocityextent = $g->VelocityHighest() - $g->VelocityLowest();
        foreach ($g->Notes()) {
            my $dur = $g->Duration() / $duration;
            push @{$self->{notes}},
              [
                $time_delta + ($_->[0] * $dur),
                $_->[1] * $dur,
                $_->[2],
                $pitch_extent == 0 ? 0 :
                  (
                    (
                        $g->PitchLowest() - $pitch_lowest + $gpitchextent *
                          $_->[3]) / $pitch_extent),
                $velocity_extent == 0 ? 0 :
                  (
                    (
                        $g->VelocityLowest() - $velocity_lowest +
                          $gvelocityextent * $_->[4]) / $velocity_extent)];
        }
        $time_pos += $g->Duration();
    }

    # 4. Save new attributes in this gestalt
    $self->{pitch_base}      = $pitch_lowest;
    $self->{pitch_extent}    = $pitch_extent;
    $self->{velocity_base}   = $velocity_lowest;
    $self->{velocity_extent} = $velocity_extent;
    $self->{duration}        = $duration;
}

=head1 AUTHOR

Christian Renz, E<lt>crenz @ web42.comE<gt>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-music-gestalt@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Music-Gestalt>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

Please also consider adding a test case to your bug report (.t script).

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2005 Christian Renz, E<lt>crenz @ web42.comE<gt>, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

42;
