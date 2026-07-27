--TEST--
A T_STRING field is read at the length the wire declares
--SKIPIF--
<?php
if (!extension_loaded('thrift_protocol')) {
    echo "skip thrift_protocol extension not loaded";
}
if (!is_file(__DIR__ . '/../../../../lib/Type/TType.php')) {
    echo "skip Thrift PHP library not found";
}
?>
--INI--
memory_limit=512M
--FILE--
<?php
use Thrift\Transport\TMemoryBuffer;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Type\TType;

// \Thrift\<Subns>\<Class> lives in lib/php/lib/<Subns>/<Class>.php
spl_autoload_register(function ($class) {
    $prefix = 'Thrift\\';
    if (strpos($class, $prefix) === 0) {
        $file = __DIR__ . '/../../../../lib/'
              . str_replace('\\', '/', substr($class, strlen($prefix))) . '.php';
        if (is_file($file)) {
            require $file;
        }
    }
});

class Payload
{
    public static $tspec = [
        1 => ['var' => 'x', 'type' => TType::STRING],
    ];
    public static $isValidate = false;
    public $x = null;

    public function __construct()
    {
    }
}

function read_string_field($declared_size, $data)
{
    $buf  = pack('C', TType::STRING);   // field type
    $buf .= pack('n', 1);               // field id
    $buf .= pack('N', $declared_size);  // string length
    $buf .= $data;
    $buf .= pack('C', TType::STOP);

    $protocol = new TBinaryProtocol(new TMemoryBuffer($buf));

    return thrift_protocol_read_binary_after_message_begin($protocol, 'Payload', true);
}

$object = read_string_field(5, 'hello');
var_dump($object->x);

// A length far beyond what fits on the stack still has to be read as data
$size = 0x01000000;
$object = read_string_field($size, str_repeat('A', $size));
var_dump(strlen($object->x));
var_dump(substr($object->x, 0, 4));

// A length the sender does not back with data ends the read, and costs no more
// than the bytes that did arrive - including at UINT32_MAX, where length + 1
// would wrap
try {
    read_string_field(0xFFFFFFFF, 'AAAA');
    echo "no exception\n";
} catch (Throwable $e) {
    echo get_class($e), "\n";
}
?>
--EXPECT--
string(5) "hello"
int(16777216)
string(4) "AAAA"
Thrift\Exception\TTransportException
