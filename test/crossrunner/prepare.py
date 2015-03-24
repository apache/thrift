#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

import os
import subprocess

from crossrunner.collect import collect_testlibs


def prepare(config_dict, testdir, server_match, client_match):
  libs, libs2 = collect_testlibs(config_dict, server_match, client_match)
  libs.extend(libs2)

  def prepares():
    for lib in libs:
      pre = lib.get('prepare')
      if pre:
        yield pre, lib['workdir']

  def files():
    for lib in libs:
      workdir = os.path.join(testdir, lib['workdir'])
      for c in lib['command']:
        if not c.startswith('-'):
          p = os.path.join(workdir, c)
          if not os.path.exists(p):
            yield os.path.split(p)

  def make(p):
    d, f = p
    with open(os.devnull, 'w') as devnull:
      return subprocess.Popen(['make', f], cwd=d, stderr=devnull)

  for pre, d in prepares():
    subprocess.Popen(pre, cwd=d).wait()

  for p in list(map(make, set(files()))):
    p.wait()
  return True
