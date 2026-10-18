--TEST--
A struct set element or map key with no string form is reported as a catchable error
--SKIPIF--
<?php
if (!extension_loaded('thrift_protocol')) {
    echo "skip thrift_protocol extension not loaded";
}
if (!is_file(__DIR__ . '/../../../../lib/Type/TType.php')) {
    echo "skip Thrift PHP library not found";
}
?>
--FILE--
<?php
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;
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

// A struct with one i32 field and no __toString(): it has no string form, so
// using it as a set element or a map key makes the reader convert it.
class Item
{
    public static $tspec = [
        1 => ['var' => 'id', 'type' => TType::I32],
    ];
    public static $isValidate = false;
    public $id = null;
    public function __construct() {}
}

class SetHolder
{
    public static $tspec = [
        1 => ['var' => 'items', 'type' => TType::SET,
              'etype' => TType::STRUCT, 'elem' => ['type' => TType::STRUCT, 'class' => 'Item']],
    ];
    public static $isValidate = false;
    public $items = null;
    public function __construct() {}
}

class MapHolder
{
    public static $tspec = [
        1 => ['var' => 'm', 'type' => TType::MAP,
              'ktype' => TType::STRUCT, 'vtype' => TType::I32,
              'key' => ['type' => TType::STRUCT, 'class' => 'Item'], 'val' => ['type' => TType::I32]],
    ];
    public static $isValidate = false;
    public $m = null;
    public function __construct() {}
}

// One Item on the wire: i32 field id 1 = 42, then the struct STOP.
function item_bytes()
{
    return pack('C', TType::I32) . pack('n', 1) . pack('N', 42) . pack('C', TType::STOP);
}

// The reader buffers the whole message up front, so a byte left after the
// outer struct's STOP is still buffered when the transport is torn down. That
// trailing byte is what the destructor would otherwise put back.
$trailing = pack('C', 0x99);

// set<Item> in field 1: one struct element, the holder STOP, then the byte.
$set_wire =
    pack('C', TType::SET) . pack('n', 1) .
    pack('C', TType::STRUCT) . pack('N', 1) . item_bytes() .
    pack('C', TType::STOP) .
    $trailing;

// map<Item,i32> in field 1: one entry (struct key, i32 value), STOP, byte.
$map_wire =
    pack('C', TType::MAP) . pack('n', 1) .
    pack('C', TType::STRUCT) . pack('C', TType::I32) . pack('N', 1) .
    item_bytes() . pack('N', 7) .
    pack('C', TType::STOP) .
    $trailing;

function run($class, $wire)
{
    $protocol = new TBinaryProtocol(new TMemoryBuffer($wire));
    try {
        thrift_protocol_read_binary_after_message_begin($protocol, $class, true);
        echo "$class: read to the end\n";
    } catch (Throwable $e) {
        echo "$class: ", get_class($e), "\n";
    }
}

run('SetHolder', $set_wire);
run('MapHolder', $map_wire);
echo "done\n";
?>
--EXPECT--
SetHolder: Error
MapHolder: Error
done
