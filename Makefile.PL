use ExtUtils::MakeMaker;
WriteMakefile( NAME         => "Music::MPDScrobble",
               VERSION_FROM => "lib/Music/MPDScrobble.pm",
               ABSTRACT     => 'MPD Audioscrobbler Routines',
               AUTHOR       => 'Edward Allen (edwardallenthethird@hotmail.com)',
               PREREQ_PM    => {
                              'File::Spec'      => 3,
                              'Digest::MD5'     => 2,
                              'Encode'          => 2,
                              'IO::Socket'      => 1,
                              'IO::File'        => 1,
                              'Config::Options' => 0.01,
                              'LWP::UserAgent'  => 2,
                              'Tie::Hash'       => 1,
                              'Proc::Daemon'    => .03,
                            },
               EXE_FILES => ['bin/musicmpdscrobble'],
             );
