/*********************************/
 * Thrift Library with Symfony 2 *
/*********************************/

I have rewrite the Thrift PHP Generator to have a real Symfony 2 Namespace compliant code.
Install it as usual.
Don't forget to enable the thrift_protocol.so in your php.ini.

The command is "thrift -r --gen php:sf2 {STRUCT}.thrift"

(Add php:server if you want to generate the Processor stuff)






And now, to use it with Symfony 2:

- Add to deps:
    [OverblogThrift]
        git=git@github.com:ebuzzing/thrift.git
        target=/thrift
        version=origin/0.8.x

- Add to app/autoload.php
    $loader->registerNamespaces(array(
        ...
        'Thrift'           => __DIR__.'/../vendor/thrift/lib/phpsf2/src',
    ));

That's it !