#!/usr/bin/perl

use strict;
use warnings;
use feature qw(:5.10);

use X11::Protocol;
use X11::Protocol::WM;
use X11::GUITest;

my $X;

# returns an array of array refs. each array ref represents a physical
# display making up the default screen. each array ref contains:
#  [ $x, $y, $width, $height ]
# where $x, $y is the location of the display within the screen
sub get_display_sizes {
	my @v; # version of Xinerama
	my @m; # monitors making up this screen

	# try to find the Xinerama version
	eval {
		@v = $X->PanoramiXQueryVersion()
	};
	if (! $@) {
		# we got a version
		if (@v == 2 and $v[0] >= 1 and $v[1] >= 1) {
			# and its new enough
			if ($X->XineramaIsActive()) {
				# and Xinerama is active
				@m = $X->XineramaQueryScreens();
				if (@m >= 1) {
					# and we successfuly queried the monitors
					return @m;
				}
			}
		}
	}

	# we assume just one screen
	return [0, 0, $X->{width_in_pixels}, $X->{height_in_pixels}];
}

# move the display with the coordinates $x, $y to the front of the list
# (assuming there is such a display). this is where we will start
# displaying windows
#
# takes an array of displays (returned from get_display_sizes) and x, y
sub prefer_display_at(\@$$) {
	my($displays, $x, $y) = @_;

	# loop through each display till our coords are in it
	for (my $i = 0; $i < @{$displays}; $i++) {
		if ($x >= $displays->[$i]->[0] and
			$x <= ($displays->[$i]->[0] + $displays->[$i]->[2]) and
			$y >= $displays->[$i]->[1] and
			$y <= ($displays->[$i]->[1] + $displays->[$i]->[3])) {
			# this is our preferred monitor so put it first if it's not
			# already
			if ($i != 0) {
				# copy to front
				unshift(@{$displays}, $displays->[$i]);
				# and remove it from further down
				splice(@{$displays}, $i + 1, 1);
			}
			last;
		}
	}
}

# takes a window ID and returns the size of the WM decorations as
# (l, r, t, b)
sub wm_decorations($) {
	my($wid) = @_;

	my @borders = X11::Protocol::WM::get_net_frame_extents($X, $wid);

	if (@borders) {
		return @borders;
	} else {
		# invalid window or not supported by WM
		return (0, 0, 0, 0);
	}
}

# returns the number of windows of the given size that will fit on the
# display of a given size
#
# takes (display width, display height, window width, window height)
sub windows_on_display_at_fixed_size($$$$) {
	my($d_w, $d_h, $t_w, $t_h) = @_;

	# how many fit across
	my $cols = int($d_w / $t_w);

	# how many fit down
	my $rows = int($d_h / $t_h);

	return $cols * $rows;
}

# calculates the size of a window required so that we can get a given number
# of windows on the given display
#
# if the preferred size is too large then we will initially try to decrease
# the window height but if that's not enough we will reduce window width. will
# not go below the minimum values given however. if no solution can be found
# we return undef.
#
# takes (display width, display height, number of windows to be fitted,
# preferred terminal width, preferred terminal height, minimum terminal width,
# minimum terminal height).
#
# returns (width, height) or undef
sub dimensions_to_get_n_on_one_display($$$$$$$) {
	my($d_w, $d_h, $count, $t_w, $t_h, $m_w, $m_h) = @_;

	# start with the requested width
	my $w = $t_w;

	# see how many columns we can get at this width
	my $cols = int($d_w / $t_w);

	while (1) {

		# now see what height we could use to get enough windows on this
		# screen
		my $h = int($d_h / int(($count / $cols) + 0.999));

		if ($h >= $m_h) {
			# this would work
			if ($h > $t_h) {
				# its bigger than asked for so use the original value
				return ($w, $t_h);
			} else {
				# it's smaller than asked for but it fits
				return ($w, $h);
			}
		} else {
			# we need to reduce the width so we can get one more window in
			$w = int($d_w / ($cols + 1));
			if ($w < $m_w) {
				# no solution :-(
				return undef;
			} else {
				# update column calculation
				$cols++;
			}
		}
	}
}

# Determine the size of the biggest window (up to the preferred size) that
# will allow us to fit all our windows on the displays given.
#
# If there is no such size (i.e. even minimum_size is too big) then return
# preferred_size and the user will get big, but overlapped windows. Maybe we
# should return the minimum size?
#
# Starts by finding the maximum (capped to preferred) size that fits a fair
# share of windows on the first display. Then checks if that works for the
# other displays as well. It may not due to differing aspect ratios. If not
# it tries the same thing for each display in turn. Eventually it must start
# with the monitor that takes the least number of windows and so should choose
# a size that fits every where. This may not be the best fit if the aspect
# ration fo different monitors is very different - imagine an inifinitely long
# monitor that is less than the minimum height - but probably works for most
# reasonable setups.
#
# Take the same hash as window_plan so see there for details.
#
# Returns (width, height)
sub window_size(%) {
	my(%a) = @_;

	# find the total area covered by the screen (i.e. all the displays)
	my $total_area;
	foreach my $area (map {
		($_->[2] - $a{display_reserve}->[0] - $a{display_reserve}->[1]) *
		($_->[3] - $a{display_reserve}->[2] - $a{display_reserve}->[3])
		} (@{$a{displays}})) {
		$total_area += $area;
	}
	
	# find the preferred width and height of a terminal, taking into account
	# the "extras"
	my $p_w = $a{preferred_size}->[0] +
		$a{terminal_fixed}->[0] +
		$a{terminal_reserve}->[0] +
		$a{terminal_reserve}->[1];
	my $p_h = $a{preferred_size}->[1] +
		$a{terminal_fixed}->[1] +
		$a{terminal_reserve}->[2] +
		$a{terminal_reserve}->[3];

	# find the minimum width and height of a terminal, taking into account the
	# "extras"
	my $m_w = $a{minimum_size}->[0] +
		$a{terminal_fixed}->[0] +
		$a{terminal_reserve}->[0] +
		$a{terminal_reserve}->[1];
	my $m_h = $a{minimum_size}->[1] +
		$a{terminal_fixed}->[1] +
		$a{terminal_reserve}->[2] +
		$a{terminal_reserve}->[3];

	# now try to size windows for this display
	foreach my $d (@{$a{displays}}) {

		# effective width of this display
		my $eff_w = $d->[2] -
			$a{display_reserve}->[0] -
			$a{display_reserve}->[1];

		# effective height of this display
		my $eff_h = $d->[3] -
			$a{display_reserve}->[2] -
			$a{display_reserve}->[3];

		# now allocate a fraction of the windows to this display based on the
		# relative area covered. if there is an odd number we try to fit an
		# extra one in on this display
		my $target_window_count = int(($a{count} *
			$eff_w * $eff_h / $total_area)
			+ 0.999);

		# now see if that is possible
		my ($w, $h) = dimensions_to_get_n_on_one_display(
			$eff_w, $eff_h, $target_window_count, $p_w, $p_h, $m_w, $m_h
		);

		if (! defined($w)) {
			# didn't fit. we should now try with another terminal
			next;
		} else {
			# we have a potential size so we need to see if that works for
			# the other displays
			my $windows_that_fit = 0;
			foreach my $sub_d (@{$a{displays}}) {
				# effective width of this display
				my $sub_w = $sub_d->[2] -
					$a{display_reserve}->[0] -
					$a{display_reserve}->[1];

				# effective height of this display
				my $sub_h = $sub_d->[3] -
					$a{display_reserve}->[2] -
					$a{display_reserve}->[3];

				$windows_that_fit += windows_on_display_at_fixed_size(
					$sub_w, $sub_h, $w, $h
				);
			}
			if ($windows_that_fit >= $a{count}) {
				# we have a solution! our w and h contain the reserve though
				# (making maths easier) so we need to remove that before
				# returning so windows are created the correct size
				return ($w -
					$a{terminal_reserve}->[0] -
					$a{terminal_reserve}->[1],
					$h -
					$a{terminal_reserve}->[2] -
					$a{terminal_reserve}->[3]
				);
			}
			# otherwise we need to go around and try the next display
		}
	}
	
	# we didn't find a solution so just default to what was asked for
	return ($p_w, $p_h);
}

# Work out how big our windows can be and where to put them.
#
# Avoids displaying windows spread across multiple displays. Can tile across
# or down.
#
# If there is not enough room to fit everything then windows are placed on
# top of each other with a 40x40 offset
#
# Takes a hash:
#
# displays - ref to an array of the sizes of our displays (obtained from
# 	get_display_sizes()
# count - number of windows we need to place
# display_reserve - borders to leave around the edge of displays (array ref)
# 	[ l, r, t, b ]
# terminal_reserve - borders to leave around the edge of windows (array ref)
# 	[ l, r, t, b ]
# terminal_fixed - size of window decorations (array ref)
# 	[ w, h ]
# preferred_size - size we would like our windows to be if they would all fit
# 	(array ref)	[ w, h ]
# minimum_size - the minimum size we will accept our windows
# order - tile across or down. can be the literal letter a or d
#
# Returns a hash:
# 
# undecorated_term_dimensions - recommended window size to fit everything
# 	(array ref)	[ w, h ]
# locations - a ref to an array containing an array ref for each window. each
# 	member array is a [ x, y ] pair
#
sub window_plan(%) {
	my(%a) = @_;

	# find the size of our terminal windows
	my($w, $h) = window_size(%a);

	# coordinates to put windows
	my @locations;

	# now find the absolute coordinates for each
	DISP: foreach my $d (@{$a{displays}}) {
		# first coordinates are top left
		my $x = $d->[0] + $a{display_reserve}->[0];
		my $y = $d->[1] + $a{display_reserve}->[2];

		if ($a{order} eq 'a') {
			# loop till we get to the bottom
			while (($y + $h) < ($d->[3] + $d->[1])) {
				# loop till we get to the right hand side
				while (($x + $w) < ($d->[2] + $d->[0])) {
					# record this location for a window
					push(@locations, [ $x, $y ]);

					if (scalar(@locations) == $a{count}) {
						# we are done
						last DISP;
					}

					# move to the right for the next window
					$x += $a{terminal_reserve}->[0] +
						$a{terminal_reserve}->[1] +
						$w;
				}

				# move down a row
				$y += $a{terminal_reserve}->[2] +
					$a{terminal_reserve}->[3] +
				 	$h;
				
				# and back to the left
				$x = $d->[0] + $a{display_reserve}->[0];
			}
		} elsif ($a{order} eq 'd') {
			# loop till we get to the right hand side
			while ($x + $w < $d->[2] + $d->[0]) {
				# loop till we get to the bottom
				while ($y + $h < $d->[3] + $d->[1]) {
					# record this location for a window
					push(@locations, [ $x, $y ]);

					if (scalar(@locations) == $a{count}) {
						# we are done
						last DISP;
					}
					
					# move down a row
					$y += $a{terminal_reserve}->[2] +
						$a{terminal_reserve}->[3] +
						$h;
				}
				
				# move to the right for the next window
				$x += $a{terminal_reserve}->[0] +
					$a{terminal_reserve}->[1] +
					$w;

				# and back to the top
				$y = $d->[1] + $a{display_reserve}->[2];
			}
		} else {
			die("unknown order");
		}
	}

	# if we haven't managed to tile then lets double up with an offset
	if (scalar(@locations) != $a{count}) {
		warn("not all windows placed\n");
		for (my $i = 0, my $togo = ($a{count} - scalar(@locations));
				$i < $togo; $i++) {
			push(@locations, [
					$locations[$i]->[0] + 40,
					$locations[$i]->[1] + 40
				]);
		}
	}

	# we have been using the decorated dimensions of the terminal window but
	# we actually need to return the undecorated, internal, dimensions
	$w -= $a{terminal_fixed}->[0] +
		$a{terminal_reserve}->[0] +
		$a{terminal_reserve}->[1];
	$h -= $a{terminal_fixed}->[1] +
		$a{terminal_reserve}->[2] +
		$a{terminal_reserve}->[3];

	return (
		undecorated_term_dimensions => [ $w, $h ],
		locations => [ @locations ]
	);
}

# create a new window
#
# this is just for testing - we don't put anything in the window
#
# returns the window ID
sub open_window {

	my $window = $X->new_rsrc;
	$X->CreateWindow($window,
		$X->root, # parent window
		'InputOutput', # class
		0, # depth, copy from parent
		0, # visual, copy from parent
		0,0, # X,Y (window manager will override)
		300,100, # width,height
		0, # border width
		background_pixel => $X->black_pixel,
	);

	$X->MapWindow($window);

	return $window;
}

MAIN: {
	$X = X11::Protocol->new;
	eval {
		$X->init_extension('XINERAMA') or
			die("No XINERAMA");
	};
	if ($@) {
		die("Xinerama not supported so go back to the old way\n");
	}

	# create n windows for testing
	my $count = $ARGV[0];
	my @wids;
	for (my $i = 0; $i < $count; $i++) {
		push(@wids, open_window());
	}

	# get the size of our monitors
	my @displays = get_display_sizes();

	# start placing windows on the display with the mouse - sort of what is
	# asked for in https://github.com/duncs/clusterssh/issues/31
	my ($mouse_x, $mouse_y) = X11::GUITest::GetMousePos();
	prefer_display_at(@displays, $mouse_x, $mouse_y);

	# find the window decoration sizes
	my @wm_borders = wm_decorations($wids[0]);

	# get a plan for how big windows should be and where they go
	my %plan = window_plan(
		# diplays from get_display_sizes
		displays => \@displays,
		# number of windows required
		count => $count,
		# borders to leave around the edge of displays
		display_reserve => [ 1, 1, 10, 1 ],
		# borders to leave around the edge of terminals
		terminal_reserve => [ 1, 1, 1, 1 ],
		# space taken by window decorations of terminals
		terminal_fixed => [
			$wm_borders[0] + $wm_borders[1],
			$wm_borders[2] + $wm_borders[3]
		],
		# the size we would like our windows to be
		preferred_size => [ 642, 410 ],
		# the minimum size we will accept
		minimum_size => [ 128, 64 ],
		# support tiling across ('a') or down ('d')
		# https://github.com/duncs/clusterssh/issues/40
		order => 'a',
	);

	# implement our plan by resizing and moving
	for (my $i = 0; $i < $count; $i++) {
		X11::GUITest::ResizeWindow($wids[$i],
			$plan{undecorated_term_dimensions}->[0],
			$plan{undecorated_term_dimensions}->[1]) or warn;
		X11::GUITest::MoveWindow($wids[$i], $plan{locations}->[$i]->[0],
			$plan{locations}->[$i]->[1]) or warn;
	}

	# wait long enough that we can verify our windows are correctly placed
	sleep(10);
}
