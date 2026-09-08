--TEST--
A container element whose declared type has no matching spec is rejected, not dereferenced
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

// struct Holder { 1: list<string> items }
// The element spec is just ['type' => STRING]; it carries no nested
// 'elem'/'key'/'val' entry, because a string element needs none.
class Holder
{
    public static $tspec = [
        1 => ['var' => 'items', 'type' => TType::LST,
              'etype' => TType::STRING,
              'elem'  => ['type' => TType::STRING]],
    ];
    public static $isValidate = false;
    public $items = null;
    public function __construct()
    {
    }
}

// Field 1 is declared T_LIST on the wire, matching the spec, so it is read
// rather than skipped. The list's element type, also off the wire, is a
// nested container. Reading that element looks up the container's element
// spec inside ['type' => STRING], which has no such entry.
function read_element($elem_type, $inner_header)
{
    $payload = pack('C', TType::LST) . pack('n', 1)   // field 1, T_LIST
             . pack('C', $elem_type) . pack('N', 1)   // element type + count 1
             . $inner_header                           // the element's own header
             . pack('C', TType::STOP);

    $protocol = new TBinaryProtocol(new TMemoryBuffer($payload));
    try {
        thrift_protocol_read_binary_after_message_begin($protocol, 'Holder', true);
        echo "read to the end\n";
    } catch (Throwable $e) {
        echo get_class($e), ": ", $e->getMessage(), "\n";
    }
}

// A nested list: reads its element type + count, then wants an 'elem' spec.
read_element(TType::LST, pack('C', TType::BYTE) . pack('N', 0));
// A nested map: reads key + value type + count, then wants a 'key' spec.
read_element(TType::MAP, pack('C', TType::BYTE) . pack('C', TType::BYTE) . pack('N', 0));
// A nested set: reads its element type + count, then wants an 'elem' spec.
read_element(TType::SET, pack('C', TType::BYTE) . pack('N', 0));
?>
--EXPECT--
Thrift\Exception\TProtocolException: no elem type in spec
Thrift\Exception\TProtocolException: no key type in spec
Thrift\Exception\TProtocolException: no elem type in spec
