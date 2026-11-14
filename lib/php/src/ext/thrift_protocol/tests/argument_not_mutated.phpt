--TEST--
Serializing a struct leaves the caller's field values and their types unchanged
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
        1 => ['var' => 'flag', 'type' => TType::BOOL],
        2 => ['var' => 'small', 'type' => TType::BYTE],
        3 => ['var' => 'mid', 'type' => TType::I16],
        4 => ['var' => 'num', 'type' => TType::I32],
        5 => ['var' => 'big', 'type' => TType::I64],
        6 => ['var' => 'real', 'type' => TType::DOUBLE],
        7 => ['var' => 'text', 'type' => TType::STRING],
        8 => [
            'var' => 'items', 'type' => TType::LST,
            'etype' => TType::I32, 'elem' => ['type' => TType::I32],
        ],
        9 => [
            'var' => 'table', 'type' => TType::MAP,
            'ktype' => TType::STRING, 'vtype' => TType::I32,
            'key' => ['type' => TType::STRING], 'val' => ['type' => TType::I32],
        ],
        // containers holding a scalar, which the extension wraps in an array
        10 => [
            'var' => 'scalarList', 'type' => TType::LST,
            'etype' => TType::STRING, 'elem' => ['type' => TType::STRING],
        ],
        11 => [
            'var' => 'scalarSet', 'type' => TType::SET,
            'etype' => TType::I32, 'elem' => ['type' => TType::I32],
        ],
        12 => [
            'var' => 'scalarMap', 'type' => TType::MAP,
            'ktype' => TType::I32, 'vtype' => TType::STRING,
            'key' => ['type' => TType::I32], 'val' => ['type' => TType::STRING],
        ],
    ];
    public static $isValidate = false;
    public $flag = null;
    public $small = null;
    public $mid = null;
    public $num = null;
    public $big = null;
    public $real = null;
    public $text = null;
    public $items = null;
    public $table = null;
    public $scalarList = null;
    public $scalarSet = null;
    public $scalarMap = null;
}

$p = new Payload();
$p->flag = 1;
$p->small = '7';
$p->mid = '300';
$p->num = '100';
$p->big = '5000000000';
$p->real = '1.5';
$p->text = 100;
$p->items = ['1', 2.0];
$p->table = ['a' => '1'];
$p->scalarList = '5';
$p->scalarSet = '5';
$p->scalarMap = '5';

$before = var_export($p, true);
thrift_protocol_write_binary(new TBinaryProtocol(new TMemoryBuffer()), 'test', 1, $p, 0, true);
echo $before === var_export($p, true) ? "unchanged\n" : "CHANGED\n";
foreach (get_object_vars($p) as $name => $value) {
    echo $name, ': ', get_debug_type($value), "\n";
}
// Elements of a nested array keep their types too.
echo get_debug_type($p->items[0]), ' ', get_debug_type($p->items[1]), ' ', get_debug_type($p->table['a']), "\n";
?>
--EXPECT--
unchanged
flag: int
small: string
mid: string
num: string
big: string
real: string
text: int
items: array
table: array
scalarList: string
scalarSet: string
scalarMap: string
string float string
