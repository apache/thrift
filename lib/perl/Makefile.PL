use ExtUtils::MakeMaker;
WriteMakefile( 'NAME' => 'Thrift',
               'VERSION_FROM' => 'lib/Thrift.pm',
               'PREREQ_PM'              => {
                   'Bit::Vector' => 0,
                   'Class::Accessor' => 0
               },
               ($] >= 5.005 ?
 (                AUTHOR     => 'T Jake Luciani <jakers@gmail.com>') : ()),
               );
