/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.thrift;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

public class TBaseHelper {

  private static final Comparator comparator = new NestedStructureComparator();

  public static int compareTo(boolean a, boolean b) {
    return Boolean.valueOf(a).compareTo(b);
  }

  public static int compareTo(byte a, byte b) {
    if (a < b) {
      return -1;
    } else if (b < a) {
      return 1;
    } else {
      return 0;
    }
  }

  public static int compareTo(short a, short b) {
    if (a < b) {
      return -1;
    } else if (b < a) {
      return 1;
    } else {
      return 0;
    }
  }

  public static int compareTo(int a, int b) {
    if (a < b) {
      return -1;
    } else if (b < a) {
      return 1;
    } else {
      return 0;
    }
  }

  public static int compareTo(long a, long b) {
    if (a < b) {
      return -1;
    } else if (b < a) {
      return 1;
    } else {
      return 0;
    }
  }

  public static int compareTo(double a, double b) {
    if (a < b) {
      return -1;
    } else if (b < a) {
      return 1;
    } else {
      return 0;
    }
  }

  public static int compareTo(String a, String b) {
    return a.compareTo(b);
  }

  public static int compareTo(byte[] a, byte[] b) {
    int sizeCompare = compareTo(a.length, b.length);
    if (sizeCompare != 0) {
      return sizeCompare;
    }
    for (int i = 0; i < a.length; i++) {
      int byteCompare = compareTo(a[i], b[i]);
      if (byteCompare != 0) {
        return byteCompare;
      }
    }
    return 0;
  }

  public static int compareTo(Comparable a, Comparable b) {
    return a.compareTo(b);
  }

  public static int compareTo(List a, List b) {
    int lastComparison = compareTo(a.size(), b.size());
    if (lastComparison != 0) {
      return lastComparison;
    }
    for (int i = 0; i < a.size(); i++) {
      lastComparison = comparator.compare(a.get(i), b.get(i));
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }

  public static int compareTo(Set a, Set b) {
    int lastComparison = compareTo(a.size(), b.size());
    if (lastComparison != 0) {
      return lastComparison;
    }
    SortedSet sortedA = new TreeSet(comparator);
    sortedA.addAll(a);
    SortedSet sortedB = new TreeSet(comparator);
    sortedB.addAll(b);

    Iterator iterA = sortedA.iterator();
    Iterator iterB = sortedB.iterator();

    // Compare each item.
    while (iterA.hasNext() && iterB.hasNext()) {
      lastComparison = comparator.compare(iterA.next(), iterB.next());
      if (lastComparison != 0) {
        return lastComparison;
      }
    }

    return 0;
  }

  public static int compareTo(Map a, Map b) {
    int lastComparison = compareTo(a.size(), b.size());
    if (lastComparison != 0) {
      return lastComparison;
    }

    // Sort a and b so we can compare them.
    SortedMap sortedA = new TreeMap(comparator);
    sortedA.putAll(a);
    Iterator<Map.Entry> iterA = sortedA.entrySet().iterator();
    SortedMap sortedB = new TreeMap(comparator);
    sortedB.putAll(b);
    Iterator<Map.Entry> iterB = sortedB.entrySet().iterator();

    // Compare each item.
    while (iterA.hasNext() && iterB.hasNext()) {
      Map.Entry entryA = iterA.next();
      Map.Entry entryB = iterB.next();
      lastComparison = comparator.compare(entryA.getKey(), entryB.getKey());
      if (lastComparison != 0) {
        return lastComparison;
      }
      lastComparison = comparator.compare(entryA.getValue(), entryB.getValue());
      if (lastComparison != 0) {
        return lastComparison;
      }
    }

    return 0;
  }

  /**
   * Comparator to compare items inside a structure (e.g. a list, set, or map).
   */
  private static class NestedStructureComparator implements Comparator {
    public int compare(Object oA, Object oB) {
      if (oA == null && oB == null) {
        return 0;
      } else if (oA == null) {
        return -1;
      } else if (oB == null) {
        return 1;
      } else if (oA instanceof List) {
        return compareTo((List)oA, (List)oB);
      } else if (oA instanceof Set) {
        return compareTo((Set)oA, (Set)oB);
      } else if (oA instanceof Map) {
        return compareTo((Map)oA, (Map)oB);
      } else if (oA instanceof byte[]) {
        return compareTo((byte[])oA, (byte[])oB);
      } else {
        return compareTo((Comparable)oA, (Comparable)oB);
      }
    }
  }

}
