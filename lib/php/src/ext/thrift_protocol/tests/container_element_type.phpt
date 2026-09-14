--TEST--
A container whose wire element type consumes no bytes (T_VOID/T_STOP) is rejected
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
memory_limit=64M
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

class ListHolder
{
    public static $tspec = [
        1 => ['var' => 'items', 'type' => TType::LST,
              'etype' => TType::STRING, 'elem' => ['type' => TType::STRING]],
    ];
    public static $isValidate = false;
    public $items = null;
    public function __construct() {}
}

class SetHolder
{
    public static $tspec = [
        1 => ['var' => 'items', 'type' => TType::SET,
              'etype' => TType::STRING, 'elem' => ['type' => TType::STRING]],
    ];
    public static $isValidate = false;
    public $items = null;
    public function __construct() {}
}

class MapHolder
{
    public static $tspec = [
        1 => ['var' => 'm', 'type' => TType::MAP,
              'ktype' => TType::STRING, 'vtype' => TType::STRING,
              'key' => ['type' => TType::STRING], 'val' => ['type' => TType::STRING]],
    ];
    public static $isValidate = false;
    public $m = null;
    public function __construct() {}
}

// The element type is read from the wire. T_VOID and T_STOP consume no bytes,
// so a huge declared count with such an element type would otherwise drive an
// unbounded loop with no matching input. The 0x7fffffff count below is never
// reached: the element type is rejected first.
$count = pack('N', 0x7fffffff);

function run($class, $payload)
{
    $protocol = new TBinaryProtocol(new TMemoryBuffer($payload));
    try {
        thrift_protocol_read_binary_after_message_begin($protocol, $class, true);
        echo "read to the end\n";
    } catch (Throwable $e) {
        echo get_class($e), ": ", $e->getMessage(), "\n";
    }
}

// list<string> field, wire element type T_VOID  -> binary_deserialize()
run('ListHolder',
    pack('C', TType::LST) . pack('n', 1) . pack('C', TType::VOID) . $count
    . pack('C', TType::STOP));

// set<string> field, wire element type T_VOID   -> binary_deserialize()
run('SetHolder',
    pack('C', TType::SET) . pack('n', 1) . pack('C', TType::VOID) . $count
    . pack('C', TType::STOP));

// map<string,string> field, wire key type T_VOID -> binary_deserialize()
run('MapHolder',
    pack('C', TType::MAP) . pack('n', 1) . pack('C', TType::VOID) . pack('C', TType::STRING) . $count
    . pack('C', TType::STOP));

// A field absent from the spec is skipped: skip_element() must reject it too.
run('ListHolder',
    pack('C', TType::LST) . pack('n', 99) . pack('C', TType::VOID) . $count
    . pack('C', TType::STOP));
?>
--EXPECT--
Thrift\Exception\TProtocolException: Invalid container element type
Thrift\Exception\TProtocolException: Invalid container element type
Thrift\Exception\TProtocolException: Invalid container element type
Thrift\Exception\TProtocolException: Invalid container element type
