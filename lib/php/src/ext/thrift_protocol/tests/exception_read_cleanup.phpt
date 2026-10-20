--TEST--
A T_EXCEPTION reply whose body fails to read releases the exception object
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

// A strict-binary EXCEPTION envelope (version|type, name "x", seqid 0). The
// accelerator builds a TApplicationException and then reads its body; the bytes
// that follow decide how that read ends.
function envelope()
{
    return pack('N', 0x80010000 | TMessageType::EXCEPTION)
         . pack('N', 1) . 'x' . pack('N', 0);
}

// A string field (id 1, the exception's 'message') carrying a length prefix.
function string_field($len)
{
    return pack('C', TType::STRING) . pack('n', 1) . pack('N', $len);
}

// Body whose string declares a length past maxStringSize: the read is refused on
// the length (SIZE_LIMIT) inside binary_deserialize_spec, in the T_EXCEPTION branch.
function exception_oversize()
{
    return envelope() . string_field(0x7fffffff);
}

// Body whose string has a sane length but no bytes on the wire: the read hits the
// end of the buffer and the transport throws, again inside that branch.
function exception_truncated()
{
    return envelope() . string_field(16);
}

// The same SIZE_LIMIT, but raised on the envelope's name length -- before any
// application-exception object has been built. Used to net out the refusal
// exception itself, so only the half-built TApplicationException is measured.
function envelope_oversize()
{
    return pack('N', 0x80010000 | TMessageType::EXCEPTION) . pack('N', 0x7fffffff);
}

function run_once($bytes)
{
    $trans = new TMemoryBuffer($bytes);
    $protocol = new TBinaryProtocolAccelerated($trans, false, true);
    try {
        thrift_protocol_read_binary($protocol, 'X', true);
    } catch (Throwable $e) {
        unset($e);
    }
    unset($protocol, $trans);
}

// Bytes still held after the call returns, averaged over many refused messages.
function per_msg($bytes, $n)
{
    run_once($bytes); // settle one-time allocations
    gc_collect_cycles();
    $before = memory_get_usage();
    for ($i = 0; $i < $n; ++$i) {
        run_once($bytes);
    }
    gc_collect_cycles();
    return (memory_get_usage() - $before) / $n;
}

// Size of one TApplicationException, the release threshold (the orphan is larger).
$mark = memory_get_usage();
$keep = [];
for ($i = 0; $i < 2000; ++$i) {
    $keep[] = new \Thrift\Exception\TApplicationException();
}
$threshold = (memory_get_usage() - $mark) / 2000;
unset($keep);

$n = 4000;

// (1) End-of-buffer: the transport exception is handed over cleanly, so the
//     half-built application exception is the only object whose fate differs.
$eof = per_msg(exception_truncated(), $n);

// (2) SIZE_LIMIT: nets out the refusal exception measured at the envelope.
$size = per_msg(exception_oversize(), $n) - per_msg(envelope_oversize(), $n);

printf("end-of-buffer body: %s\n",
    $eof < $threshold ? 'released' : sprintf('retains %.0f B/msg', $eof));
printf("size-limit body: %s\n",
    $size < $threshold ? 'released' : sprintf('retains %.0f B/msg', $size));
?>
--EXPECT--
end-of-buffer body: released
size-limit body: released
