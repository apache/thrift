<?php

use Thrift\ClassLoader\ThriftClassLoader;

require_once __DIR__ . '/../../../vendor/autoload.php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Basic', __DIR__ . '/Resources/packages/php');
$loader->registerNamespace('Validate', __DIR__ . '/Resources/packages/phpv');
$loader->registerNamespace('ValidateOop', __DIR__ . '/Resources/packages/phpvo');
$loader->registerNamespace('Json', __DIR__ . '/Resources/packages/phpjs');

#do not load this namespace here, it will be loaded in ClassLoaderTest
//$loader->registerNamespace('Server', __DIR__ . '/Resources/packages/phpcm');

$loader->register();
