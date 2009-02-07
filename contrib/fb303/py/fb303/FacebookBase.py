#!/usr/bin/env python
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

import time
import FacebookService
import thrift.reflection.limited
from ttypes import fb_status

class FacebookBase(FacebookService.Iface):

  def __init__(self, name):
    self.name = name
    self.alive = int(time.time())
    self.counters = {}

  def getName(self, ):
    return self.name

  def getVersion(self, ):
    return ''

  def getStatus(self, ):
    return fb_status.ALIVE

  def getCounters(self):
    return self.counters

  def resetCounter(self, key):
    self.counters[key] = 0

  def getCounter(self, key):
    if self.counters.has_key(key):
      return self.counters[key]
    return 0

  def incrementCounter(self, key):
    self.counters[key] = self.getCounter(key) + 1

  def setOption(self, key, value):
    pass

  def getOption(self, key):
    return ""

  def getOptions(self):
    return {}

  def getOptions(self):
    return {}

  def aliveSince(self):
    return self.alive

  def getCpuProfile(self, duration):
    return ""

  def getLimitedReflection(self):
    return thrift.reflection.limited.Service()

  def reinitialize(self):
    pass

  def shutdown(self):
    pass
