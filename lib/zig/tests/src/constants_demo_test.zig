// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

const std = @import("std");
const thrift = @import("thrift");
const cd = @import("constants_demo");

const t = std.testing;

test "ConstantsDemo scalar constants" {
    try t.expectEqual(@as(cd.myIntType, 3), cd.MY_INT);
    try t.expectEqual(@as(i32, 0x1F), cd.HEX_CONST);
    try t.expectEqual(@as(i32, -0x1F), cd.NEGATIVE_HEX_CONSTANT);
    try t.expectEqual(@as(i32, -3523553), cd.GEN_ME);
    try t.expectEqual(@as(f64, 325.532), cd.G_EN_D_U_B);
    try t.expectEqual(@as(f64, 85.2355), cd.G_EN_D_U);
    try t.expectEqual(@as(f64, 1e10), cd.E10);
    try t.expectEqual(@as(f64, -1e10), cd.E11);
}

test "ConstantsDemo string constant" {
    var gen_string = try cd.ConstGENSTRING.const_value(t.allocator);
    defer gen_string.deinit();
    try t.expectEqualStrings("asldkjasfd", gen_string.contents);
}

test "ConstantsDemo uuid constants" {
    try t.expect(cd.GEN_UUID.eql(cd.MY_UUID));
    try t.expect(cd.GEN_GUID.eql(cd.MY_GUID));
}

test "ConstantsDemo list constant" {
    var gen_list = try cd.ConstGENLIST.const_value(t.allocator);
    defer gen_list.deinit();

    try t.expectEqual(@as(usize, 3), gen_list.count());
    const items = gen_list.items();
    try t.expectEqual(@as(i32, 235235), items[0]);
    try t.expectEqual(@as(i32, 23598352), items[1]);
    try t.expectEqual(@as(i32, 3253523), items[2]);
}

test "ConstantsDemo map constants" {
    var gen_map = try cd.ConstGENMAP.const_value(t.allocator);
    defer gen_map.deinit();

    try t.expectEqual(@as(?i32, 233), gen_map.get(35532));
    try t.expectEqual(@as(?i32, 853), gen_map.get(43523));

    var gen_map2 = try cd.ConstGENMAP2.const_value(t.allocator);
    defer gen_map2.deinit();

    try t.expectEqual(@as(?i32, 233), gen_map2.get(.initFromBorrowed("hello")));
    try t.expectEqual(@as(?i32, 853), gen_map2.get(.initFromBorrowed("lkj98d")));
    try t.expectEqual(@as(?i32, 98325), gen_map2.get(.initFromBorrowed("lkjsdf")));

    var gen_mapmap = try cd.ConstGENMAPMAP.const_value(t.allocator);
    defer gen_mapmap.deinit();

    const inner = gen_mapmap.get(235) orelse return error.TestExpectedEqual;
    try t.expectEqual(@as(?i32, 53255), inner.get(532));
    try t.expectEqual(@as(?i32, 235), inner.get(235));
}

test "ConstantsDemo set constants" {
    var gen_set = try cd.ConstGENSET.const_value(t.allocator);
    defer gen_set.deinit();

    try t.expectEqual(@as(usize, 2), gen_set.count());
    try t.expect(gen_set.contains(235));
    try t.expect(gen_set.contains(53235));

    var guid_set = try cd.ConstGUIDSET.const_value(t.allocator);
    defer guid_set.deinit();

    try t.expectEqual(@as(usize, 2), guid_set.count());
    try t.expect(guid_set.contains(cd.GEN_GUID));
    try t.expect(guid_set.contains(cd.GEN_UUID));
}

test "ConstantsDemo struct constant" {
    var gen_thing = try cd.ConstGENTHING.const_value(t.allocator);
    defer gen_thing.deinit();

    try t.expectEqual(@as(i32, 325), gen_thing.hello);
    try t.expectEqual(@as(i32, 325352), gen_thing.goodbye);
    try t.expect(gen_thing.id.eql(cd.GEN_GUID));
    try t.expect(gen_thing.my_id.eql(cd.GEN_UUID));
    const optional_id = gen_thing.my_optional_id orelse return error.TestExpectedEqual;
    try t.expect(optional_id.eql(cd.GEN_UUID));

    var gen_what = try cd.ConstGENWHAT.const_value(t.allocator);
    defer gen_what.deinit();

    const nested = gen_what.get(35) orelse return error.TestExpectedEqual;
    try t.expectEqual(@as(i32, 325), nested.hello);
    try t.expectEqual(@as(i32, 325352), nested.goodbye);
    try t.expect(nested.id.eql(cd.GEN_UUID));
    try t.expect(nested.my_id.eql(cd.GEN_UUID));
    const nested_optional = nested.my_optional_id orelse return error.TestExpectedEqual;
    try t.expect(nested_optional.eql(cd.GEN_UUID));
}
