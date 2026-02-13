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

final File buildLog = new File(basedir, 'build.log')
assert buildLog.isFile()

final String logText = buildLog.getText('UTF-8')
assert !logText.contains('not marked as thread-safe') :
    'Build log contains thread-safety warning; plugin should be marked thread-safe.'

assert new File(basedir, 'module-a/target/generated-sources/thrift/shared/SharedService.java').isFile()
assert new File(basedir, 'module-b/target/generated-sources/thrift/tutorial/Calculator.java').isFile()

return true
