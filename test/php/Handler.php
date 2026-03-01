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
        return $hello;
    }

    public function testInsanity(\ThriftTest\Insanity $argument)
    {
        return $argument;
    }

    public function testMulti($arg0, $arg1, $arg2, array $arg3, $arg4, $arg5)
    {
        // TODO: Implement testMulti() method.
    }

    public function testException($arg)
    {
        throw new \Exception($arg);
    }

    public function testMultiException($arg0, $arg1)
    {
        throw new \Exception($arg0, $arg1);
    }

    public function testOneway($secondsToSleep)
    {
        sleep($secondsToSleep);
    }
}
