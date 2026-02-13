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

import java.security.MessageDigest

final File outputRoot = new File(basedir, 'target/generated-test-sources/thrift')
assert outputRoot.isDirectory()

final Map<String, String> expected = loadChecksums(new File(basedir, 'expected-java-private-members.sha256'))
final Map<String, String> actual = computeChecksums(outputRoot)

assert expected == actual : "Generated thrift test output did not match baseline checksums."

Map<String, String> loadChecksums(File checksumsFile) {
  assert checksumsFile.isFile()
  final Map<String, String> checksums = new TreeMap<String, String>()
  checksumsFile.eachLine('UTF-8') { line ->
    final String trimmed = line.trim()
    if (trimmed.isEmpty() || trimmed.startsWith('#')) {
      return
    }

    final int separator = trimmed.indexOf('  ')
    assert separator > 0 : "Invalid checksums line: ${trimmed}"
    final String hash = trimmed.substring(0, separator)
    final String relativePath = trimmed.substring(separator + 2)
    checksums.put(relativePath, hash)
  }
  return checksums
}

Map<String, String> computeChecksums(File root) {
  final Map<String, String> checksums = new TreeMap<String, String>()
  root.eachFileRecurse { file ->
    if (file.isFile() && file.name.endsWith('.java')) {
      final String relativePath = root.toPath().relativize(file.toPath()).toString().replace(File.separatorChar, '/' as char)
      checksums.put(relativePath, sha256(file))
    }
  }
  return checksums
}

String sha256(File file) {
  final MessageDigest digest = MessageDigest.getInstance('SHA-256')
  digest.update(file.bytes)
  final byte[] hash = digest.digest()
  return hash.collect { String.format('%02x', it) }.join('')
}

return true
