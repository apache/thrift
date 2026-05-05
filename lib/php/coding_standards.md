## PHP Coding Standards

Please follow:
 * [Thrift General Coding Standards](/doc/coding_standards.md)
 * [PSR-12](https://www.php-fig.org/psr/psr-12/)

### Naming

In addition to PSR-12, Apache Thrift's PHP code does not use leading or trailing
underscores on identifiers (properties, methods, constants). Earlier versions of
the library carried a Google-C++-style trailing underscore convention on
private/protected members; this is no longer accepted. Use plain camelCase for
properties and methods, with PHP visibility keywords (`private`, `protected`,
`public`) to convey intent.

The phpcs configuration in `phpcs.xml.dist` enforces PSR-12 across
`lib/php/lib/` and `lib/php/test/`.
