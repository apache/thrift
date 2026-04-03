<?php

class Handler implements \ThriftTest\ThriftTestIf
{
    public function testVoid()
    {
        return;
    }

    public function testString($thing)
    {
        return $thing;
    }

    public function testBool($thing)
    {
        return $thing;
    }

    public function testByte($thing)
    {
        return $thing;
    }

    public function testI32($thing)
    {
        return $thing;
    }

    public function testI64($thing)
    {
        return $thing;
    }

    public function testDouble($thing)
    {
        return $thing;
    }

    public function testBinary($thing)
    {
        return $thing;
    }

    public function testUuid($thing)
    {
        return $thing;
    }

    public function testStruct(\ThriftTest\Xtruct $thing)
    {
        return $thing;
    }

    public function testNest(\ThriftTest\Xtruct2 $thing)
    {
        return $thing;
    }

    public function testMap(array $thing)
    {
        return $thing;
    }

    public function testStringMap(array $thing)
    {
        return $thing;
    }

    public function testSet(array $thing)
    {
        return $thing;
    }

    public function testList(array $thing)
    {
        return $thing;
    }

    public function testEnum($thing)
    {
        return $thing;
    }

    public function testTypedef($thing)
    {
        return $thing;
    }

    public function testMapMap($hello)
    {
        return [
            -4 => [
                -4 => -4,
                -3 => -3,
                -2 => -2,
                -1 => -1,
            ],
            4 => [
                4 => 4,
                3 => 3,
                2 => 2,
                1 => 1,
            ],
        ];
    }

    public function testInsanity(\ThriftTest\Insanity $argument)
    {
        $looney = new \ThriftTest\Insanity();

        return [
            1 => [
                \ThriftTest\Numberz::TWO => $argument,
                \ThriftTest\Numberz::THREE => $argument,
            ],
            2 => [
                \ThriftTest\Numberz::SIX => $looney,
            ],
        ];
    }

    public function testMulti($arg0, $arg1, $arg2, array $arg3, $arg4, $arg5)
    {
        $result = new \ThriftTest\Xtruct();
        $result->string_thing = 'Hello2';
        $result->byte_thing = $arg0;
        $result->i32_thing = $arg1;
        $result->i64_thing = $arg2;

        return $result;
    }

    public function testException($arg)
    {
        if ($arg === 'Xception') {
            $exception = new \ThriftTest\Xception();
            $exception->errorCode = 1001;
            $exception->message = $arg;
            throw $exception;
        }

        if ($arg === 'TException') {
            throw new \Thrift\Exception\TException('This is a TException');
        }
    }

    public function testMultiException($arg0, $arg1)
    {
        if ($arg0 === 'Xception') {
            $exception = new \ThriftTest\Xception();
            $exception->errorCode = 1001;
            $exception->message = 'This is an Xception';
            throw $exception;
        }

        if ($arg0 === 'Xception2') {
            $exception = new \ThriftTest\Xception2();
            $exception->errorCode = 2002;
            $exception->struct_thing = new \ThriftTest\Xtruct();
            $exception->struct_thing->string_thing = 'This is an Xception2';
            throw $exception;
        }

        $result = new \ThriftTest\Xtruct();
        $result->string_thing = $arg1;

        return $result;
    }

    public function testOneway($secondsToSleep)
    {
        // Keep the oneway test quick so the cross-test matrix measures fire-and-forget behavior,
        // not a full second of handler blocking in the single-threaded PHP test server.
        usleep($secondsToSleep * 300000);
    }
}
