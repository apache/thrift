/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.thrift.TException;

import thrift.test.Insanity;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;
import thrift.test.Numberz;

public class TestHandler implements ThriftTest.Iface {

    public byte testByte(byte thing) throws TException {
        return thing;
    }

    public double testDouble(double thing) throws TException {
        return thing;
    }

    public Numberz testEnum(Numberz thing) throws TException {
        return thing;
    }

    public void testException(String arg) throws Xception, TException {
        throw new Xception(1,arg);       
    }

    public int testI32(int thing) throws TException {
        return thing;
    }

    public long testI64(long thing) throws TException {
        return thing;
    }

    public Map<Long, Map<Numberz, Insanity>> testInsanity(Insanity argument) throws TException {
        Map<Long, Map<Numberz, Insanity>> result = new HashMap<Long, Map<Numberz,Insanity>>();
        
        result.put(Long.valueOf(1), new HashMap<Numberz,Insanity>());
        result.get(Long.valueOf(1)).put(Numberz.ONE, argument);
        
        result.put(Long.valueOf(2), new HashMap<Numberz,Insanity>());
        result.get(Long.valueOf(2)).put(Numberz.ONE, argument);
        
        return result;
    }

    public List<Integer> testList(List<Integer> thing) throws TException {
        return thing;
    }

    public Map<Integer, Integer> testMap(Map<Integer, Integer> thing) throws TException {
        return thing;
    }

    public Map<Integer, Map<Integer, Integer>> testMapMap(int hello) throws TException {
        Map<Integer, Map<Integer,Integer>> result = new HashMap<Integer, Map<Integer,Integer>>();
        
        result.put(Integer.valueOf(1), new HashMap<Integer,Integer>());
        result.get(Integer.valueOf(1)).put(Integer.valueOf(1), Integer.valueOf(1));
        result.get(Integer.valueOf(1)).put(Integer.valueOf(2), Integer.valueOf(2));
        result.get(Integer.valueOf(2)).put(Integer.valueOf(1), Integer.valueOf(1));
    
        return result;
    }

    public Xtruct testMulti(byte arg0, int arg1, long arg2, Map<Short, String> arg3, Numberz arg4, long arg5) throws TException {
        Xtruct xtr = new Xtruct();
        
        xtr.byte_thing = arg0;
        xtr.i32_thing  = arg1;
        xtr.i64_thing  = arg2;
        xtr.string_thing = "server string";
        
        return xtr;
    }

    public Xtruct testMultiException(String arg0, String arg1) throws Xception, Xception2, TException {
        Xtruct xtr = new Xtruct();
        xtr.setString_thing(arg0);
        throw new Xception2(1,xtr);
    }

    public Xtruct2 testNest(Xtruct2 thing) throws TException {
       return thing;
    }

    public void testOneway(int secondsToSleep) throws TException {
        try{
            Thread.sleep(secondsToSleep * 1000);
        }catch(InterruptedException e){
            
        }
    }

    public Set<Integer> testSet(Set<Integer> thing) throws TException {
        return thing;
    }

    public String testString(String thing) throws TException {
        return thing;
    }

    public Xtruct testStruct(Xtruct thing) throws TException {
        return thing;
    }

    public long testTypedef(long thing) throws TException {
        return thing;
    }

    public void testVoid() throws TException {
        
    }
}
