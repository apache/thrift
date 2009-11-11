package org.apache.thrift;

import java.util.List;

public class TBaseHelper {
  
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
      Object oA = a.get(i);
      Object oB = b.get(i);
      if (oA instanceof List) {
        lastComparison = compareTo((List)oA, (List)oB);
      } else {
        lastComparison = compareTo((Comparable)oA, (Comparable)oB);
      }
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }
}
