--TEST--
A string field longer than the protocol's maximum string size is refused
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
use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TMessageType;
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

// Counts how many of its bytes the extension has taken, so that a field it
// refuses can be told from one it reads.
class CountingBuffer extends TMemoryBuffer
{
    private $total;

    public function __construct($buf)
    {
        parent::__construct($buf);
        $this->total = strlen($buf);
    }

    public function taken()
    {
        return $this->total - $this->available();
    }
}

// A struct of one string field: type, id, declared length, bytes, stop.
function struct_bytes($length, $field_id = 1)
{
    return pack('C', TType::STRING) . pack('n', $field_id) . pack('N', $length)
         . str_repeat('A', $length) . pack('C', TType::STOP);
}

// A strict message header in front of the struct, for the client entry point.
function message_bytes($length)
{
    return pack('N', 0x80010000 | TMessageType::REPLY) . pack('N', 4) . 'ping'
         . pack('N', 1) . struct_bytes($length);
}

function report($label, $max, $bytes, $entry)
{
    $trans = new CountingBuffer($bytes);
    $protocol = new TBinaryProtocolAccelerated($trans, false, true, $max);
    try {
        $result = $entry($protocol);
        printf("%s: string of %d bytes, %d of %d bytes taken\n",
            $label, strlen((string) $result->x), $trans->taken(), strlen($bytes));
    } catch (Throwable $e) {
        printf("%s: %s code %d, %d of %d bytes taken\n",
            $label, get_class($e), $e->getCode(), $trans->taken(), strlen($bytes));
    }
}

$server = function ($protocol) {
    return thrift_protocol_read_binary_after_message_begin($protocol, 'Payload', true);
};
$client = function ($protocol) {
    return thrift_protocol_read_binary($protocol, 'Payload', true);
};

// The extension is reachable with any object that hands out a transport; one
// without a maximum reads whatever the wire declares.
class PlainProtocol
{
    private $trans;

    public function __construct($trans)
    {
        $this->trans = $trans;
    }

    public function getTransport()
    {
        return $this->trans;
    }
}

$mib = 1024 * 1024;

report('server, 1 MiB field, maximum 1024', 1024, struct_bytes($mib), $server);
report('server, 1 MiB field, no maximum', 0, struct_bytes($mib), $server);
report('client, 1 MiB field, maximum 1024', 1024, message_bytes($mib), $client);
report('server, field at the maximum', 1024, struct_bytes(1024), $server);
report('server, field one over the maximum', 1024, struct_bytes(1025), $server);
report('server, field under the maximum', 1024, struct_bytes(1000), $server);
report('server, skipped field over the maximum', 1024, struct_bytes($mib, 99), $server);

$trans = new CountingBuffer(struct_bytes($mib));
$result = thrift_protocol_read_binary_after_message_begin(new PlainProtocol($trans), 'Payload', true);
printf("protocol without a maximum: string of %d bytes, %d of %d bytes taken\n",
    strlen((string) $result->x), $trans->taken(), $mib + 8);
?>
--EXPECT--
server, 1 MiB field, maximum 1024: Thrift\Exception\TProtocolException code 3, 7 of 1048584 bytes taken
server, 1 MiB field, no maximum: string of 1048576 bytes, 1048584 of 1048584 bytes taken
client, 1 MiB field, maximum 1024: Thrift\Exception\TProtocolException code 3, 23 of 1048600 bytes taken
server, field at the maximum: string of 1024 bytes, 1032 of 1032 bytes taken
server, field one over the maximum: Thrift\Exception\TProtocolException code 3, 7 of 1033 bytes taken
server, field under the maximum: string of 1000 bytes, 1008 of 1008 bytes taken
server, skipped field over the maximum: Thrift\Exception\TProtocolException code 3, 7 of 1048584 bytes taken
protocol without a maximum: string of 1048576 bytes, 1048584 of 1048584 bytes taken
