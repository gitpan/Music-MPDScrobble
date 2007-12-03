package Music::MPDScrobble;
our $VERSION = 0.02;

# Copyright (c) 2007 Edward J. Allen III
# Some code and inspiration from Audio::MPD Copyright (c) 2005 Tue Abrahamsen, Copyright (c) 2006 Nicholas J. Humfrey, Copyright (c) 2007 Jerome Quelin
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the Artistic License, or
# the GNU Public License.  Both are distributed with Perl.
#

=pod

=head1 NAME

Music::MPDScrobble - Module providing routines to submit songs to last.fm from MPD 

=head1 SYNOPSIS

use Music::MPDScrobble
my $mpds = Music::MPDScrobble->new(\%options); 
$mpds->monitor_mpd();

=head1 AUTHOR

Edward Allen, ealleniii _at_ cpan _dot_ org

=head1 DESCRIPTION

The motiviation behind this was to provide a convenient method for fixing broken tags.

This module is a wrapper module, which calls various plugin modules to find information about a music file and write it back into the tag. 

=cut

#use Music::Audioscrobbler;
use File::Spec;
use Digest::MD5 qw(md5_hex);
use Encode qw(encode);
use IO::Socket;
use IO::File;
use Config::Options;

sub _default_options {
    {  lastfm_username => undef,
       lastfm_password => undef,
       mdb_opts        => {},
       musicdb         => 0,
       musictag        => 0,
       quiet           => 0,
       ANSIColor       => 0,
       verbose         => 1,
       monitor         => 1,
       daemonize       => 0,
       timeout         => 15,      # Set low to prevent missing a scrobble.  Rather retry submit.
       pidfile            => "/var/run/musicmpdscrobble.pid",
       logfile            => undef,
       default_cache_time => 86400,
       mpd_password       => undef,
       mpd_server         => $ENV{MPD_HOST} || 'localhost',
       mpd_port           => $ENV{MPD_PORT} || 6600,
       music_directory    => "/mnt/media/music/MP3s",
       scrobble_queue     => $ENV{HOME} . "/.musicaudioscrobbler_queue",
       optionfile     => [ "/etc/musicmpdscrobble.conf", $ENV{HOME} . "/.musicmpdscrobble.conf" ],
       runonstart     => [],
       runonsubmit    => [],
	   lastfm_client_id => "tst",
	   lastfm_client_version => "1.0",
       music_tag_opts => { quiet     => 1,
                           verbose   => 0,
                           ANSIColor => 0,
                         },
    };
}

=pod

=over 4

=head1 METHODS

=item new()

my $mpds = Music::MPDScrobble->new($options);

=cut

sub new {
    my $class   = shift;
    my $options = shift || {};
    my $self    = {};
    bless $self, $class;
    $self->options( $self->_default_options );
    $self->options->fromfile_perl( $self->options->{optionfile} );
    $self->options($options);
    $self->{scrobble_ok} = 1;

    unless ( $self->options('lastfm_md5password') ) {
        if ( $self->options('lastfm_password') ) {
            $self->options->{lastfm_md5password} =
              Digest::MD5::md5_hex( $self->options->{lastfm_password} );
            delete $self->options->{lastfm_password};
        }
    }
    return $self;
}

=pod

=item monitor_mpd()

Starts the main loop. 

=cut

sub monitor_mpd {
    my $self = shift;
    $self->status( 1, "Starting Music::MPDScrobble version $VERSION" );
    while (1) {
        if ( $self->is_connected ) {
            $self->update_info();
            sleep 1;
        }
        else {
            $self->connect;
            sleep 4;
        }
        unless ( $self->{scrobble_ok} ) {
            if ( ( time - $self->{lastscrobbled} ) > 600 ) {
                $self->{scrobble_ok}   = $self->process_scrobble_queue();
                $self->{lastscrobbled} = time;
            }
        }
    }
}

=pod

=item options()

Get or set options via hash.  Here is a list of available options:

=over 4

=item optionfile		    

Perl file used to get options from

=item lastfm_username		

lastfm username

=item lastfm_password		

lastfm password.  Not needed if lastfm_md5password is set.

=item lastfm_md5password 

MD5 hash of lastfm password. 

=item lastfm_client_id

Client ID provided by last.fm.  Defaults to "tst", which is valid for testing only.

=item lastfm_client_version

Set to the version of your program when setting a valid client_id.  Defaults to "1.0"

=item mpd_server			

hostname of mpd_server

=item mpd_port

port for mpd_server

=item mpd_password		

mpd password

=item verbose				

Set verbosity level (1 throuch 4)

=item logfile				

File to output loginfo to

=item scrobblequue		

Path to file to queue info to

=item music_directory		

Root to MP3 files

=item runonsubmit			

Array of commands to run after submit

=item runonstart			

Array of commands to run on start of play

=item monitor				

True if monitor should be turned on

=item musictag			

True if you want to use Music::Tag to get info from file

=item musicdb				

True to use MusicDB plugin for Music::Tag

=item music_tag_opts		

Options for Music::Tag 

=back

=cut

sub options {
    my $self = shift;
    if ( exists $self->{_options} ) {
        return $self->{_options}->options(@_);
    }
    else {
        $self->{_options} = Config::Options->new();
        return $self->{_options}->options(@_);
    }
}

=pod

=head1 INTERNAL METHODS (for reference)


=item mpdsock()

returns open socket to mpd program.

=cut

sub mpdsock {
    my $self = shift;
    my $new  = shift;
    if ($new) {
        $self->{mpdsock} = $new;
    }
    unless ( exists $self->{mpdsock} ) {
        $self->{mpdsock} = undef;
    }
    return $self->{mpdsock};
}

=pod

=item connect()

Connect to MPD if necessary

=cut

sub connect {
    my $self = shift;
    if ( ( $self->mpdsock ) && ( $self->is_connected ) ) {
        $self->status( 3, "Already connected just fine." );
        return 1;
    }
    $self->mpdsock(
                    IO::Socket::INET->new( PeerAddr => $self->options("mpd_server"),
                                           PeerPort => $self->options("mpd_port"),
                                           Proto    => 'tcp',
                                         )
                  );

    unless ( ( $self->mpdsock ) && ( $self->mpdsock->connected ) ) {
        $self->status( 1, "Could not create socket to mpd: $!" );
        return 0;
    }

    if ( $self->mpdsock->getline() =~ /^OK MPD (.+)$/ ) {
        $self->{mpd_sever_version} = $1;
    }
    else {
        $self->status( 1, "Bad response from mpd ($!)" );
        return 0;
    }
    if ( $self->options->{mpd_password} ) {

    }
    $self->send_password if $self->options(mpd_password);
    return 1;
}

=pod

=item is_connected()

Return true if connected to mpd.

=cut

sub is_connected {
    my $self = shift;
    if ( ( $self->mpdsock ) && ( $self->mpdsock->connected ) ) {
        $self->mpdsock->print("ping\n");
        return ( $self->mpdsock->getline() =~ /^OK/ );
    }
    return undef;
}

=pod

=item process_feedback

Process response from mpd.

=cut

sub process_feedback {
    my $self = shift;
    my @output;
    if ( ( $self->mpdsock ) && ( $self->mpdsock->connected ) ) {
        while ( my $line = $self->mpdsock->getline() ) {
            chomp($line);

            # Did we cause an error? Save the data!
            if ( $line =~ /^ACK \[(\d+)\@(\d+)\] {(.*)} (.+)$/ ) {
                $self->{ack_error_id}         = $1;
                $self->{ack_error_command_id} = $2;
                $self->{ack_error_command}    = $3;
                $self->{ack_error}            = $4;
                $self->status( 1, "Error sent to MPD: $line" );
                return undef;
            }
            last if ( $line =~ /^OK/ );
            push( @output, $line );
        }
    }

    # Let's return the output for post-processing
    return @output;
}

=pod

=item send_command($command)

send a commnd to mpd.

=cut

sub send_command {
    my $self = shift;
    if ( $self->is_connected ) {
        $self->mpdsock->print( @_, "\n" );
        return $self->process_feedback;
    }
}

=pod

=item send_command($command)

send password to mpd.

=cut

sub send_password {
    my $self = shift;
    $self->send_command( "password ", $self->{password} );
}

=pod

=item get_info($command)

Send mpd a command and parse the output if output is a column seperated list.

=cut

sub get_info {
    my $self    = shift;
    my $command = shift;
    my $ret     = {};
    foreach ( $self->send_command($command) ) {
        if (/^(.[^:]+):\s(.+)$/) {
            $ret->{$1} = $2;
        }
    }
    return $ret;
}

=pod

=item get_status($command)


get_status command. Returns hashref with:

    *  volume: (0-100)
    * repeat: (0 or 1)
    * random: (0 or 1)
    * playlist: (31-bit unsigned integer, the playlist version number)
    * playlistlength: (integer, the length of the playlist)
    * playlistqueue: (integer, the temporary fifo playlist version number)
    * xfade: <int seconds> (crossfade in seconds)
    * state: ("play", "stop", or "pause")
    * song: (current song stopped on or playing, playlist song number)
    * songid: (current song stopped on or playing, playlist songid)
    * time: <int elapsed>:<time total> (of current playing/paused song)
    * bitrate: <int bitrate> (instantaneous bitrate in kbps)
    * audio: <int sampleRate>:<int bits>:<int channels>
    * updating_db: <int job id>
    * error: if there is an error, returns message here 

=cut

sub get_status {
    my $self = shift;
    $self->get_info("status");
}

=pod

=item get_current_song_info($command)

get_status command. Returns hashref with:

    file: albums/bob_marley/songs_of_freedom/disc_four/12.bob_marley_-_could_you_be_loved_(12"_mix).flac
    Time: 327
    Album: Songs Of Freedom - Disc Four
    Artist: Bob Marley
    Title: Could You Be Loved (12" Mix)
    Track: 12
    Pos: 11
    Id: 6601

=cut

sub get_current_song_info {
    my $self = shift;
    $self->get_info("currentsong");
}

=pod

=item status($level, @message)

Print to log.

=cut

sub status {
    my $self  = shift;
    my $level = shift;
    if ( $level <= $self->options->{verbose} ) {
        my $out = $self->logfileout;
        print $out scalar localtime(), " ", @_, "\n";
    }
}

=pod

=item logfileout 

returns filehandle to log.

=cut

sub logfileout {
    my $self = shift;
    my $fh   = shift;
    if ($fh) {
        $self->{logfile} = $fh;
    }
    unless ( $self->options->{logfile} ) {
        return \*STDERR;
    }
    unless ( ( exists $self->{logfile} ) && ( $self->{logfile} ) ) {
        my $fh = IO::File->new( $self->options->{logfile}, ">>" );
        $fh->autoflush(1);
        unless ($fh) {
            print STDERR "Error opening log, using STDERR: $!";
            return \*STDERR;
        }
        $self->{logfile} = $fh;
    }
    return $self->{logfile};
}

#sub mas {
#	my $self = shift;
#	unless ((exists $self->{mas}) && (ref $self->{mas})) {
#		$self->{mas} = Music::Audioscrobbler->new($self->options);
#		$self->{mas}->logfileout($self->logfileout);
#	}
#	return $self->{mas};
#}

=pod

=item new_info($cinfo)

reset current song info.

=cut

sub new_info {
    my $self  = shift;
    my $cinfo = shift;
    $self->{current_song} = $cinfo->{file};
    if ( $self->{current_song} =~ /^http/i ) {
        $self->{current_file} = undef;
    }
    elsif ( -e File::Spec->rel2abs( $self->{current_song}, $self->options->{music_directory} ) ) {
        $self->{current_file} =
          File::Spec->rel2abs( $self->{current_song}, $self->options->{music_directory} );
    }
    else {
        $self->{current_file} = 0;
    }
    $self->{info} = $self->info_to_hash(
                                         { album    => $cinfo->{Album},
                                           artist   => $cinfo->{Artist},
                                           title    => $cinfo->{Title},
                                           secs     => $cinfo->{Time},
                                           filename => $self->{current_file},
                                         }
                                       );

    #Prevent excessive calls to info_to_hash
    delete $self->{info}->{filename};

    $self->{song_duration}     = $cinfo->{Time};
    $self->{current_id}        = $cinfo->{Id};
    $self->{running_time}      = 0;
    $self->{last_running_time} = undef;
    $self->{state}             = "";
    $self->{started_at}        = time;
    $self->status( 1, "New Song: ", $self->{current_id}, " - ", $self->{current_file} );
}

=pod

=item song_change($cinfo)

Run on song change

=cut

sub song_change {
    my $self  = shift;
    my $cinfo = shift;
    if ( ( $self->{current_file} )
         && (    ( $self->{running_time} >= 240 )
              or ( $self->{running_time} >= ( $self->{song_duration} / 2 ) ) )
      ) {
        $self->scrobble();
        $self->run_commands( $self->options->{runonsubmit} );
    }
    else {
        $self->status( 4, "Not scrobbling ",
                       $self->{current_file}, " with run time of ",
                       $self->{running_time} );
    }
    my $state = $self->{state};
    $self->new_info($cinfo);
    if ( ( defined $self->{current_file} ) && ( $cinfo->{Time} ) && ( $state eq "play" ) ) {
        $self->status( 4, "Announcing start of play for: ", $self->{current_file} );
        $self->now_playing( $self->{info} );
        $self->run_commands( $self->options->{runonstart} );
    }
    else {
        $self->status( 4, "Not announcing start of play for: ", $self->{current_file} );
    }
}

=pod

=item update_info()

Run on poll

=cut

sub update_info {
    my $self   = shift;
    my $status = $self->get_status;
    my $cinfo  = $self->get_current_song_info();
    $self->{state} = $status->{state};
    my ( $so_far, $total ) = split( /:/, $status->{'time'} );
    my $time = time;
    if ( $self->{state} eq "play" ) {
        unless ( $cinfo->{Id} eq $self->{current_id} ) {
            $self->song_change($cinfo);
        }
        unless ( defined $self->{last_running_time} ) {
            $self->{last_running_time} = $so_far;
        }
        unless ( defined $self->{last_update_time} ) {
            $self->{last_update_time} = $time;
        }
        my $run_since_update = ( $so_far - $self->{last_running_time} );
        my $time_since_update =
          ( $time - $self->{last_update_time} ) + 5;    # Adding 5 seconds for rounding fudge
        if ( ( $run_since_update > 0 ) && ( $run_since_update <= $time_since_update ) ) {
            $self->{running_time} += $run_since_update;
        }
        elsif ($run_since_update) {
            $self->status( 3, "Skip detected, ignoring time change." );
        }
        $self->{last_running_time} = $so_far;
        $self->{last_update_time}  = $time;
    }
    elsif ( ( $self->{state} eq "stop" ) && ( $self->{running_time} ) ) {
        $self->song_change($cinfo);
    }
    if ( $self->options->{monitor} ) {
        $self->monitor();
    }
}

=pod

=item monitor()

print current status to STDERR

=cut

sub monitor {
    my $self = shift;
    printf STDERR "%5s ID: %4s  TIME: %5s             \r", $self->{state}, $self->{current_id},
      $self->{running_time};
}

=pod

=item scrobble()

Scrobble current song

=cut

sub scrobble {
    my $self = shift;
    if ( defined $self->{current_file} ) {
        $self->status( 2, "Adding ", $self->{current_file}, " to scrobble queue" );
        $self->{scrobble_ok} = $self->submit( [ $self->{info}, $self->{started_at} ] );
        $self->{lastscrobbled} = time;
    }
    else {
        $self->status( 3, "Skipping stream: ", $self->{current_file} );
    }
}

=pod

=item run_commands()

Fork and run list of commands.

=cut

sub run_commands {
    my $self     = shift;
    my $commands = shift;
    return unless ( ( ref $commands ) && ( scalar @{$commands} ) );
    my $pid = fork;
    if ($pid) {
        $self->status( 4, "Forked to run commands\n" );
    }
    elsif ( defined $pid ) {
		if ( $self->options->{logfile} ) {
			my $out = $self->logfileout;
			open STDOUT, ">&", $out;
			select STDOUT;
			$| = 1;
			open STDERR, ">&", $out;
			select STDERR;
			$| = 1;
		}
        foreach my $c ( @{$commands} ) {
            $c =~ s/\%f/$self->{current_file}/e;
            $c =~ s/\%a/$self->{info}->{artist}/e;
            $c =~ s/\%b/$self->{info}->{album}/e;
            $c =~ s/\%t/$self->{info}->{title}/e;
            $c =~ s/\%l/$self->{info}->{secs}/e;
            $c =~ s/\%n/$self->{info}->{track}/e;
            $c =~ s/\%m/$self->{info}->{mbid}/e;
            my $s = system($c);
            delete $self->{fh};

            if ($s) {
                $self->status( 0, "Failed to run command: ${c}: $!" );
            }
            else {
                $self->status( 2, "Command ${c} successful" );
            }
        }
        exit;
    }
    else {
        $self->status( 0, "Failed to fork for commands: $!" );
    }
}

###
#  These routines are part of Music::Audioscrobbler.  They have been integrated in for now and may be pulled out later!
###

use LWP::UserAgent;
use Tie::File;

sub ua {
    my $self = shift;
    my $ua   = shift;
    unless ( ( exists $self->{ua} ) && ( ref $self->{ua} ) ) {
        $self->{ua} = LWP::UserAgent->new();
        $self->{ua}->agent( 'scrobbler-helper/1.0 ' . $self->{ua}->_agent() );
        $self->{ua}->timeout( $self->options->{timeout} );
    }
    unless ( $self->{'ua'} ) {
        die 'Could not create an LWP UserAgent object?!?';
    }
    return $self->{'ua'};
}

sub URLEncode($) {
    my $theURL = encode( "utf-8", $_[0] );
    $theURL =~ s/([^a-zA-Z0-9_\.])/'%' . uc(sprintf("%2.2x",ord($1)));/eg;
    return $theURL;
}

sub makequery {
    my $self  = shift;
    my @query = @_;
    my $q     = "";
    for ( my $i = 0 ; $i < @query ; $i += 2 ) {
        if ($q) { $q .= "&" }
        $q .= $query[$i] . "=" . URLEncode( $query[ $i + 1 ] );
    }
    return $q;
}

sub info_to_hash {
    my $self = shift;
    my $info = shift;
    if ( ref $info eq "HASH" ) {
        if ( exists $info->{filename} ) {
            eval {
                my $extra = $self->get_info_from_file( $info->{filename} );
                while ( my ( $k, $v ) = each %{$extra} ) {
					next if (($k eq "secs") && (exists $info->{secs}) && ($info->{secs} > 30));
                    $info->{$k} = $v;
                }
            };    # eval'd to protect from a bad Music::Tag plugin causing trouble.
            if ($@) { $self->status( 0, "Error with Music::Tag: ", $@ ) }
        }
        foreach (qw(artist title secs album track mbid tracknum)) {
            unless ( exists $info->{$_} ) {
                $info->{$_} = "";
            }
            if ( exists $info->{mb_trackid} ) {
                $info->{mbid} = $info->{mb_trackid};
            }
            if ( exists $info->{length} ) {
                $info->{secs} = $info->{length};
            }
            unless ( $info->{secs} ) {
                $info->{secs} = 300;
            }
        }
        return $info;
    }
    elsif ( ref $info ) {
        my $ret = {};
        $ret->{artist}   = $info->artist;
        $ret->{title}    = $info->title;
        $ret->{secs}     = int($info->secs) || 300;
        $ret->{album}    = $info->album || "";
        $ret->{tracknum} = $info->track || "";
        $ret->{mbid}     = $info->mb_trackid || "";
        return $ret;
    }
    elsif ( -f $info ) {
        return $self->get_info_from_file($info);
    }
    $self->status( 0, "Hash or Music::Tag object or filename required!" );
    return undef;
}

sub music_tag_opts {
    my $self    = shift;
    my $options = shift || {};
    my $mt_opts = { ( %{ $self->options->{music_tag_opts} }, %{$options} ) };
    return $mt_opts;
}

sub get_info_from_file {
    my $self = shift;
    my $file = shift;
    return unless ( $self->options->{musictag} );
    require Music::Tag;
    $self->status( 3, "Filename $file detected" );
    my $minfo = Music::Tag->new( $file, $self->music_tag_opts() );
    if ($minfo) {
        if ( $self->options->{musicdb} ) {
            $minfo->add_plugin("MusicDB");
        }
        $minfo->get_tag;
        $self->status( 4, "Filename $file is really " . $minfo->title );
        return $self->info_to_hash($minfo);
    }
}

sub scrobble_queue {
    my $self = shift;
    unless ( ( exists $self->{scrobble_queue} ) && ( $self->{scrobble_queue} ) ) {
        my @q;
        tie @q, 'Tie::File', $self->options("scrobble_queue")
          or die "Couldn't tie array to scrobble_queue: " . $self->options("scrobble_queue");
        $self->{scrobble_queue} = \@q;
    }
    return $self->{scrobble_queue};
}

sub handshake {
    my $self      = shift;
    my $timestamp = time;
    my $auth      = md5_hex( $self->options->{lastfm_md5password} . $timestamp );
    my @query = ( 'hs' => "true",
                  'p'  => "1.2",
                  'c'  => $self->options->{lastfm_client_id},
                  'v'  => $self->options->{lastfm_client_version},
                  'u'  => $self->options->{lastfm_username},
                  't'  => $timestamp,
                  'a'  => $auth
                );
    my $q = $self->makequery(@query);

    $self->status( 2, "Performing Handshake with query: $q\n" );

    my $req = new HTTP::Request( 'GET', "http://post.audioscrobbler.com/?$q" );
    unless ($req) {
        die 'Could not create the handshake request object';
    }
    my $resp = $self->ua->request($req);
    $self->status( 2, "Response to handshake is: ",
                   $resp->content, " and success is ",
                   $resp->status_line );
    unless ( $resp->is_success ) {
        $self->status( 0, "Response failed: ", $resp->status_line );
        return 0;
    }

    my @lines = split /[\r\n]+/, $resp->content;

    my $status = shift @lines;
    if ( $status eq "OK" ) {
        $self->{session_id}     = shift @lines;
        $self->{nowplaying_url} = shift @lines;
        $self->{submission_url} = shift @lines;
        $self->{timestamp}      = $timestamp;
        return $self->{session_id};
    }
    elsif ( $status eq "FAILED" ) {
        $self->status( 0, "Temporary Failure: ", @lines );
        return 0;
    }
    elsif ( $status eq "BADAUTH" ) {
        $self->status( 0, "Bad authorization code: ", @lines );
        return undef;
    }
    elsif ( $status eq "BADTIME" ) {
        $self->status( 0, "Bad time stamp: ", @lines );
        return undef;
    }
    else {
        $self->status( 0, "Unknown Error: ", $status, " ", @lines );
        return undef;
    }
}

sub now_playing {
    my $self = shift;
    my $info = shift;
    my $h    = $self->info_to_hash($info);
    return unless ( defined $h );
    unless ( $self->{session_id} && ( ( time - $self->{timestamp} ) < 3600 ) ) {
        my $h = $self->handshake();
        unless ($h) { return $h; }
    }
    my @sub = ();
    push @sub, "s", $self->{session_id};
    push @sub, "a", $h->{artist};
    push @sub, "t", $h->{title};
    push @sub, "b", $h->{album};
    push @sub, "l", $h->{secs};
    push @sub, "n", $h->{track};
    push @sub, "m", $h->{mbid};
    my $q = $self->makequery(@sub);
    my $req = HTTP::Request->new( 'POST', $self->{nowplaying_url} );

    unless ($req) {
        die 'Could not create the submission request object';
    }
    $self->status( 2,
                   "Notifying nowplaying info to ",
                   $self->{nowplaying_url},
                   " with query: $q\n" );
    $req->content_type('application/x-www-form-urlencoded; charset="UTF-8"');
    $req->content($q);
    my $resp = $self->ua->request($req);
    $self->status( 2, "Response to submission is: ",
                   $resp->content, " and success is ",
                   $resp->is_success );
    my @lines = split /[\r\n]+/, $resp->content;
    my $status = shift @lines;

    if ( $status eq "OK" ) {
        $self->status( 1, "Notification OK" );
        return 1;
    }
    elsif ( $status eq "BADSESSION" ) {
        $self->status( 0, "Bad session code: ", @lines );
        $self->{session_id} = 0;
        return 0;
    }
    else {
        $self->status( 0, "Unknown Error: ", $status, " ", @lines );
        return undef;
    }
}

sub do_submit {
    my $self = shift;
    unless ( $self->{session_id} && ( ( time - $self->{timestamp} ) < 3600 ) ) {
        my $h = $self->handshake();
        unless ($h) { return $h; }
    }
    my @sub = ();
    push @sub, "s", $self->{session_id};
    my $n = 0;
    foreach my $s (@_) {
        my ( $info, $timestamp ) = @{$s};
        my $h = $self->info_to_hash($info);
        next unless ( defined $h );
        push @sub, "a[$n]", $h->{artist};
        push @sub, "t[$n]", $h->{title};
        push @sub, "i[$n]", $timestamp;
        push @sub, "o[$n]", "P";            # Nothing but P supported yet.
        push @sub, "r[$n]", "";             # Not supported yet.
        push @sub, "l[$n]", $h->{secs};
        push @sub, "b[$n]", $h->{album};
        push @sub, "n[$n]", $h->{track};
        push @sub, "m[$n]", $h->{mbid};
        $self->status( 1, "Submitting: ", scalar localtime($timestamp),
                       " ", $h->{artist}, " - ", $h->{title} );
        $n++;
    }
    my $q = $self->makequery(@sub);
    my $req = HTTP::Request->new( 'POST', $self->{submission_url} );
    unless ($req) {
        die 'Could not create the submission request object';
    }
    $self->status( 2, "Performing submission to ", $self->{submission_url}, " with query: $q\n" );
    $req->content_type('application/x-www-form-urlencoded; charset="UTF-8"');
    $req->content($q);
    my $resp = $self->ua->request($req);
    $self->status( 2, "Response to submission is: ",
                   $resp->content, " and success is ",
                   $resp->is_success );

    my @lines = split /[\r\n]+/, $resp->content;

    my $status = shift @lines;
    if ( $status eq "OK" ) {
        $self->status( 1, "Submission OK" );
        return 1;
    }
    elsif ( $status eq "BADSESSION" ) {
        $self->status( 0, "Bad session code: ", @lines );
        $self->{session_id} = 0;
        return 0;
    }
    else {
        $self->status( 0, "Unknown Error: ", $status, " ", @lines );
        return undef;
    }
}

sub serialize_info {
    my $self = shift;
    my ( $h, $timestamp ) = @_;
    my $ret = join( "\0", timestamp => $timestamp, %{$h} );
}

sub deserialize_info {
    my $self = shift;
    my $in   = shift;
    my %ret  = split( "\0", $in );
    return ( \%ret, $ret{timestamp} );
}

sub submit {
    my $self = shift;
    foreach my $s (@_) {
        my ( $info, $timestamp ) = @{$s};
        my $h = $self->info_to_hash($info);
        if ($h) {
            push @{ $self->scrobble_queue }, $self->serialize_info( $h, $timestamp );
        }
    }
    $self->process_scrobble_queue;
}

# Process up to 50 files from scrobble_queue. Recursivly calls itself if necessary / possible to empty scrobble_queue
sub process_scrobble_queue {
    my $self = shift;
    return -1 unless scalar @{ $self->scrobble_queue };
    my @submit = ();
    foreach ( @{ $self->scrobble_queue } ) {
        push @submit, [ $self->deserialize_info($_) ];
        if ( scalar @submit >= 50 ) {
            last;
        }
    }
    my $ok = $self->do_submit(@submit);
    if ($ok) {
        foreach (@submit) {
            shift @{ $self->scrobble_queue };
        }
        if ( scalar @{ $self->scrobble_queue } ) {
            $self->process_scrobble_queue;
        }
    }
    return $ok;
}

=head1 SEE ALSO

L<mpdscrobble.pl>

=head1 COPYRIGHT

Copyright (c) 2007 Edward J. Allen III
Some code and inspiration from L<Audio::MPD> Copyright (c) 2005 Tue Abrahamsen, Copyright (c) 2006 Nicholas J. Humfrey, Copyright (c) 2007 Jerome Quelin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.


=cut

1;
