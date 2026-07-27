--TEST--
Nested structs are read only as deep as the recursion limit allows
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

// A field id the spec does not declare, so reading it takes the skip path
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

// A struct that contains itself, so reading it recurses by the spec
class Node
{
    public static $tspec = [
        1 => ['var' => 'child', 'type' => TType::STRUCT, 'class' => 'Node'],
    ];
    public static $isValidate = false;
    public $child = null;

    public function __construct()
    {
    }
}

function nested_struct($depth, $field_id)
{
    return str_repeat(pack('C', TType::STRUCT) . pack('n', $field_id), $depth)
         . pack('C', TType::STOP)
         . str_repeat(pack('C', TType::STOP), $depth);
}

function read_nested($depth, $class, $field_id)
{
    $protocol = new TBinaryProtocol(new TMemoryBuffer(nested_struct($depth, $field_id)));

    try {
        thrift_protocol_read_binary_after_message_begin($protocol, $class, true);
        echo "read to the end\n";
    } catch (Throwable $e) {
        echo get_class($e), ": ", $e->getMessage(), "\n";
    }
}

// unknown field -> skip path
read_nested(8, 'Payload', 7);
read_nested(100000, 'Payload', 7);

// declared field of a self-referencing struct -> spec path
read_nested(8, 'Node', 1);
read_nested(100000, 'Node', 1);
?>
--EXPECT--
read to the end
Thrift\Exception\TProtocolException: Maximum recursion depth exceeded
read to the end
Thrift\Exception\TProtocolException: Maximum recursion depth exceeded
