--TEST--
A message that fails partway through a container element retains no memory across reads
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

// A 256 KiB element, large enough that one retained copy per message dwarfs the
// chosen threshold.
const SLEN = 262144;
$BIG = str_repeat('A', SLEN);

// A binary-protocol string: 4-byte length prefix then that many bytes.
function wstring($b) { return pack('N', strlen($b)) . $b; }
// A string whose length is declared but whose body is absent: the reader asks for
// the bytes, the buffer is empty, and the transport raises TTransportException.
function trunc_string($len) { return pack('N', $len); }

class MapHolder {
    public static $tspec = [1 => ['var' => 'm', 'type' => TType::MAP,
        'ktype' => TType::STRING, 'vtype' => TType::STRING,
        'key' => ['type' => TType::STRING], 'val' => ['type' => TType::STRING]]];
    public static $isValidate = false;
    public $m = null;
    public function __construct() {}
}
class ListHolder {
    public static $tspec = [1 => ['var' => 'l', 'type' => TType::LST,
        'etype' => TType::STRING, 'elem' => ['type' => TType::STRING]]];
    public static $isValidate = false;
    public $l = null;
    public function __construct() {}
}
class ListListHolder {
    public static $tspec = [1 => ['var' => 'l', 'type' => TType::LST,
        'etype' => TType::LST,
        'elem' => ['type' => TType::LST, 'etype' => TType::STRING,
                   'elem' => ['type' => TType::STRING]]]];
    public static $isValidate = false;
    public $l = null;
    public function __construct() {}
}
class SetListHolder {
    public static $tspec = [1 => ['var' => 's', 'type' => TType::SET,
        'etype' => TType::LST,
        'elem' => ['type' => TType::LST, 'etype' => TType::STRING,
                   'elem' => ['type' => TType::STRING]]]];
    public static $isValidate = false;
    public $s = null;
    public function __construct() {}
}
class TwoStringHolder {
    public static $tspec = [1 => ['var' => 'a', 'type' => TType::STRING],
                            2 => ['var' => 'b', 'type' => TType::STRING]];
    public static $isValidate = false;
    public $a = null;
    public $b = null;
    public function __construct() {}
}

// map<string,string>, one entry, key present, value body absent: the key temporary
// is built and the value read then fails.
function msg_map($BIG) {
    return pack('C', TType::MAP) . pack('n', 1)
         . pack('C', TType::STRING) . pack('C', TType::STRING) . pack('N', 1)
         . wstring($BIG) . trunc_string(100);
}
// list<string> of N complete elements followed by a truncated one: the N complete
// elements are held in the field's array when the next read fails.
function msg_list($BIG, $n) {
    $s = pack('C', TType::LST) . pack('n', 1) . pack('C', TType::STRING) . pack('N', $n + 1);
    for ($i = 0; $i < $n; $i++) $s .= wstring($BIG);
    return $s . trunc_string(100);
}
// list<list<string>>, one inner list that is partially filled when the inner read fails:
// the outer list loop holds the partial inner list as its element temporary.
function msg_listlist($BIG) {
    return pack('C', TType::LST) . pack('n', 1) . pack('C', TType::LST) . pack('N', 1)
         . pack('C', TType::STRING) . pack('N', 2) . wstring($BIG) . trunc_string(100);
}
// set<list<string>>, one element: the inner list is partially filled when the read fails.
function msg_set($BIG) {
    return pack('C', TType::SET) . pack('n', 1) . pack('C', TType::LST) . pack('N', 1)
         . pack('C', TType::STRING) . pack('N', 2) . wstring($BIG) . trunc_string(100);
}
// struct{string a, string b}: a is read in full, b is truncated, so the whole result
// object (carrying a) is pending when the read fails.
function msg_struct($BIG) {
    return pack('C', TType::STRING) . pack('n', 1) . wstring($BIG)
         . pack('C', TType::STRING) . pack('n', 2) . trunc_string(100);
}

// Reads the (malformed) struct body N times through the server entry point and reports
// the memory retained per read. A reader that releases its partially-read temporaries
// retains nothing; one that orphans them retains about one element per read.
function retained_per_read($class, $bytes, $n = 8, $warm = 2) {
    $base = 0;
    for ($i = 0; $i < $n + $warm; $i++) {
        $proto = new TBinaryProtocol(new TMemoryBuffer($bytes));
        $res = null;
        try {
            $res = thrift_protocol_read_binary_after_message_begin($proto, $class, true);
        } catch (Throwable $e) {
            unset($e);
        }
        unset($res, $proto);
        if ($i == $warm - 1) {
            $base = memory_get_usage();
        }
    }
    return (int)((memory_get_usage() - $base) / $n);
}

function report($label, $class, $bytes) {
    $per = retained_per_read($class, $bytes);
    // Fixed readers retain 0; an unfixed reader retains >= one 256 KiB element.
    echo $label, ': ', ($per < 16384 ? 'no retention' : "retained $per B/read"), "\n";
}

report('map<string,string>',     'MapHolder',       msg_map($BIG));
report('list<string> x8',        'ListHolder',      msg_list($BIG, 8));
report('list<list<string>>',     'ListListHolder',  msg_listlist($BIG));
report('set<list<string>>',      'SetListHolder',   msg_set($BIG));
report('struct earlier field',   'TwoStringHolder', msg_struct($BIG));
echo "done\n";
?>
--EXPECT--
map<string,string>: no retention
list<string> x8: no retention
list<list<string>>: no retention
set<list<string>>: no retention
struct earlier field: no retention
done
