--TEST--
A message that is refused releases the exception object it raised
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
zend.exception_ignore_args=1
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

// A struct of one string field whose declared length is past the maximum, so
// the read is refused on the length before any body byte is read (server path).
function struct_oversize()
{
    return pack('C', TType::STRING) . pack('n', 1) . pack('N', 0x00100000);
}

// The same struct behind a strict REPLY header (client path).
function reply_oversize()
{
    return pack('N', 0x80010000 | TMessageType::REPLY) . pack('N', 4) . 'ping'
         . pack('N', 1) . struct_oversize();
}

// A strict EXCEPTION header and an empty (T_STOP) application-exception body:
// the body reads to the end, then the client raises the application exception.
function exception_clean()
{
    return pack('N', 0x80010000 | TMessageType::EXCEPTION) . pack('N', 4) . 'ping'
         . pack('N', 1) . pack('C', TType::STOP);
}

function run_once($bytes, $max, $server)
{
    $trans = new TMemoryBuffer($bytes);
    $protocol = new TBinaryProtocolAccelerated($trans, false, true, $max);
    try {
        if ($server) {
            thrift_protocol_read_binary_after_message_begin($protocol, 'Payload', true);
        } else {
            thrift_protocol_read_binary($protocol, 'Payload', true);
        }
    } catch (Throwable $e) {
        unset($e);
    }
    unset($protocol, $trans);
}

// Bytes still held after the call returns, averaged over many refused messages.
function per_msg($bytes, $max, $server, $n)
{
    run_once($bytes, $max, $server); // settle one-time allocations
    gc_collect_cycles();
    $before = memory_get_usage();
    for ($i = 0; $i < $n; ++$i) {
        run_once($bytes, $max, $server);
    }
    gc_collect_cycles();
    return (memory_get_usage() - $before) / $n;
}

// Size of one result object, used as the server-path threshold below.
$mark = memory_get_usage();
$keep = [];
for ($i = 0; $i < 1000; ++$i) {
    $keep[] = new Payload();
}
$result_object = (memory_get_usage() - $mark) / 1000;
unset($keep);

$n = 4000;
$client_size = per_msg(reply_oversize(), 1024, false, $n);
$client_exc  = per_msg(exception_clean(), 0, false, $n);
$server_size = per_msg(struct_oversize(), 1024, true, $n);

printf("client SIZE_LIMIT: %s\n",
    $client_size < 256 ? 'released' : sprintf('retains %.0f B/msg', $client_size));
printf("client T_EXCEPTION: %s\n",
    $client_exc < 256 ? 'released' : sprintf('retains %.0f B/msg', $client_exc));
printf("server SIZE_LIMIT: %s\n",
    $server_size < $result_object + 512 ? 'within one result object' : sprintf('retains %.0f B/msg', $server_size));
?>
--EXPECT--
client SIZE_LIMIT: released
client T_EXCEPTION: released
server SIZE_LIMIT: within one result object
