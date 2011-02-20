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


package thrift

func CompareInt(i, j int) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareInt16(i, j int16) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareInt32(i, j int32) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareInt64(i, j int32) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareStringArray(i, j []string) int {
  if cmp := CompareInt(len(i), len(j)); cmp != 0 {
    return cmp
  }
  size := len(i)
  for k := 0; k < size; k++ {
    if cmp := CompareString(i[k], j[k]); cmp != 0 {
      return cmp
    }
  }
  return 0
}

func CompareString(i, j string) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareFloat(i, j float32) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareDouble(i, j float64) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareByte(i, j byte) int {
  if i > j {
    return 1
  }
  if i < j {
    return -1
  }
  return 0
}

func CompareBool(i, j bool) int {
  if i {
    if j {
      return 0
    }
    return 1
  }
  if j {
    return -1
  }
  return 0
}
