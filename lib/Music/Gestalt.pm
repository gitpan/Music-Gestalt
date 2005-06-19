package Music::Gestalt;

use warnings;
use strict;
use fields
  qw (notes duration pitch_base pitch_extent velocity_base velocity_extent);
use 5.0061;

=head1 NAME

Music::Gestalt - Compose music using gestalts.

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

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

    $self->_InitializeFromScore($params{score})
      if (ref $params{score} eq 'ARRAY');

    return $self;
}

# private methods

sub _InitializeFromScore {
    my ($self, $score) = @_;

    my $max_time     = 0;
    my $min_pitch    = 128;
    my $max_pitch    = -1;
    my $min_velocity = 128;
    my $max_velocity = -1;

    # find min/max values
    foreach (@$score) {
        next unless $_->[0] eq 'note';

        # min_time is always 0
        $max_time     = $_->[1] + $_->[2] if $_->[1] + $_->[2] > $max_time;
        $min_pitch    = $_->[4]           if $_->[4] < $min_pitch;
        $max_pitch    = $_->[4]           if $_->[4] > $max_pitch;
        $min_velocity = $_->[5]           if $_->[5] < $min_velocity;
        $max_velocity = $_->[5]           if $_->[5] > $max_velocity;
    }

    $self->{pitch_base}      = $min_pitch;
    $self->{pitch_extent}    = $max_pitch - $min_pitch;
    $self->{velocity_base}   = $min_velocity;
    $self->{velocity_extent} = $max_velocity - $min_velocity;
    $self->{duration}        = $max_time;
    $self->{notes}           = [];

    return if ($max_time == 0);

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

=head1 PROPERTIES

=head2 C<PitchLowest>

Returns lowest pitch used in this gestalt.

=cut

sub PitchLowest {
    my $self = shift;

    return $self->{pitch_base};
}

=head2 C<PitchHighest>

Returns highest pitch used in this gestalt.

=cut

sub PitchHighest {
    my $self = shift;

    return undef
      unless defined $self->{pitch_base} && $self->{pitch_extent};
    return $self->{pitch_base} + $self->{pitch_extent};
}

=head2 C<PitchMiddle>

Returns middle pitch of the range used in this gestalt.

=cut

sub PitchMiddle {
    my $self = shift;

    return undef
      unless defined $self->{pitch_base} && $self->{pitch_extent};
    return (2 * $self->{pitch_base} + $self->{pitch_extent}) / 2;
}

=head2 C<PitchRange>

Returns the pitch range used in this gestalt, ie. the pitches that occur
will be pitch middle +/- pitch range.

=cut

sub PitchRange {
    my $self = shift;

    return undef
      unless defined $self->{pitch_base} && $self->{pitch_extent};
    return ((2 * $self->{pitch_base} + $self->{pitch_extent}) / 2) -
      $self->{pitch_base};
}

=head2 C<VelocityLowest>

Returns lowest velocity used in this gestalt.

=cut

sub VelocityLowest {
    my $self = shift;

    return $self->{velocity_base};
}

=head2 C<VelocityHighest>

Returns highest velocity used in this gestalt.

=cut

sub VelocityHighest {
    my $self = shift;

    return undef
      unless defined $self->{velocity_base} && $self->{velocity_extent};
    return $self->{velocity_base} + $self->{velocity_extent};
}

=head2 C<VelocityMiddle>

Returns middle velocity of the range used in this gestalt.

=cut

sub VelocityMiddle {
    my $self = shift;

    return undef
      unless defined $self->{velocity_base} && $self->{velocity_extent};
    return (2 * $self->{velocity_base} + $self->{velocity_extent}) / 2;
}

=head2 C<VelocityRange>

Returns the velocity range used in this gestalt, ie. the velocityes that occur
will be velocity middle +/- velocity range.

=cut

sub VelocityRange {
    my $self = shift;

    return undef
      unless defined $self->{velocity_base} && $self->{velocity_extent};
    return ((2 * $self->{velocity_base} + $self->{velocity_extent}) / 2) -
      $self->{velocity_base};
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
              'note',
              $_->[0] * $self->{duration},
              $_->[1] * $self->{duration},
              int(($_->[2] * 15 + 0.5) + 1),
              int($self->{pitch_base} + $_->[3] * $self->{pitch_extent} + 0.5),
              int(
                  $self->{velocity_base} + $_->[4] * $self->{velocity_extent} +
                    0.5)];
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
            ($self->{pitch_base} - $pitch_lowest + $self->{pitch_extent} *
                $_->[3]) / $pitch_extent;
          $_->[4] =
            ($self->{velocity_base} - $velocity_lowest +
                $self->{velocity_extent} * $_->[4]) / $velocity_extent;
      }

      # 3. Transform and append notes in the gestalts to be appended
      my $time_pos = $self->{duration} || 0;

      foreach my $g (@_) {
          my $time_delta      = $time_pos / $duration;
          my $gpitchextent    = $g->PitchHighest() - $g->PitchLowest();
          my $gvelocityextent = $g->VelocityHighest() - $g->VelocityLowest();
          foreach ($g->Notes()) {
              push @{$self->{notes}},
                [
                  $time_delta + ($_->[0] * $g->Duration() / $duration),
                  $_->[1] * $g->Duration() / $duration,
                  $_->[2],
                  ($g->PitchLowest() - $pitch_lowest + $gpitchextent * $_->[3])
                    / $pitch_extent,
                  (
                      $g->VelocityLowest() - $velocity_lowest +
                        $gvelocityextent * $_->[4]) / $velocity_extent];
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
