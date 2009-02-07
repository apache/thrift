// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.fb303;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

public abstract class FacebookBase implements FacebookService.Iface {

  private String name_;

  private long alive_;

  private final ConcurrentHashMap<String,Long> counters_ =
    new ConcurrentHashMap<String, Long>();

  private final ConcurrentHashMap<String,String> options_ =
    new ConcurrentHashMap<String, String>();

  protected FacebookBase(String name) {
    name_ = name;
    alive_ = System.currentTimeMillis() / 1000;
  }

  public String getName() {
    return name_;
  }

  public abstract int getStatus();

  public String getStatusDetails() {
    return "";
  }

  public void deleteCounter(String key) {
    counters_.remove(key);
  }

  public void resetCounter(String key) {
    counters_.put(key, 0L);
  }

  public long incrementCounter(String key) {
    long val = getCounter(key) + 1;
    counters_.put(key, val);
    return val;
  }

  public AbstractMap<String,Long> getCounters() {
    return counters_;
  }

  public long getCounter(String key) {
    Long val = counters_.get(key);
    if (val == null) {
      return 0;
    }
    return val.longValue();
  }

  public void setOption(String key, String value) {
    options_.put(key, value);
  }

  public String getOption(String key) {
    return options_.get(key);
  }

  public AbstractMap<String,String> getOptions() {
    return options_;
  }

  public long aliveSince() {
    return alive_;
  }

  public String getCpuProfile() {
    return "";
  }

  public void reinitialize() {}

  public void shutdown() {}

}
