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
package org.apache.thrift.maven;

import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.cli.CommandLineException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class TestThriftOutputRegression {

    private File testRootDir;
    private File idlDir;
    private String thriftExecutable;

    @Before
    public void setup() throws Exception {
        testRootDir = Files.createTempDirectory("thrift-output-regression-").toFile();
        idlDir = new File("src/test/resources/idl");

        thriftExecutable = System.getProperty("thriftExecutable", "thrift");
        if (!(new File(thriftExecutable).exists())) {
            thriftExecutable = "thrift";
        }
    }

    @Test
    public void testJavaOutputMatchesBaseline() throws Exception {
        assertOutputMatchesBaseline(
            "java",
            new LinkedHashSet<String>(Arrays.asList("shared.thrift", "tutorial.thrift")),
            "/expected/java.sha256");
    }

    @Test
    public void testJavaPrivateMembersOutputMatchesBaseline() throws Exception {
        assertOutputMatchesBaseline(
            "java:private_members",
            new LinkedHashSet<String>(Arrays.asList("shared.thrift")),
            "/expected/java_private_members.sha256");
    }

    private void assertOutputMatchesBaseline(String generator,
                                             Set<String> thriftFiles,
                                             String expectedChecksumsResource) throws CommandLineException, Exception {
        final Thrift.Builder builder = new Thrift.Builder(thriftExecutable, testRootDir)
            .setGenerator(generator)
            .addThriftPathElement(idlDir);
        for (String thriftFile : thriftFiles) {
            builder.addThriftFile(new File(idlDir, thriftFile));
        }

        final Thrift thrift = builder.build();
        assertEquals("thrift compile failed: " + thrift.getError(), 0, thrift.compile());

        final Map<String, String> expectedChecksums = loadChecksums(expectedChecksumsResource);
        final Map<String, String> actualChecksums = computeChecksums(testRootDir.toPath());

        assertEquals(
            "Generated sources do not match baseline checksums from " + expectedChecksumsResource
                + ". If this is an intentional thrift compiler output change, regenerate this manifest.",
            expectedChecksums,
            actualChecksums);
    }

    private Map<String, String> loadChecksums(String checksumsResource) throws Exception {
        final InputStream inputStream = getClass().getResourceAsStream(checksumsResource);
        assertNotNull("Missing checksums resource " + checksumsResource, inputStream);

        final Map<String, String> checksums = new TreeMap<String, String>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                final String trimmed = line.trim();
                if (trimmed.isEmpty() || trimmed.startsWith("#")) {
                    continue;
                }

                final int separator = trimmed.indexOf("  ");
                assertTrue("Invalid checksums line format: " + trimmed, separator > 0);
                final String hash = trimmed.substring(0, separator);
                final String relativePath = trimmed.substring(separator + 2);
                checksums.put(relativePath, hash);
            }
        }
        return checksums;
    }

    private Map<String, String> computeChecksums(Path root) throws Exception {
        final Map<String, String> checksums = new TreeMap<String, String>();
        try (Stream<Path> stream = Files.walk(root)) {
            stream
                .filter(path -> Files.isRegularFile(path) && path.toString().endsWith(".java"))
                .forEach(path -> {
                    try {
                        final String relativePath = root.relativize(path).toString().replace(File.separatorChar, '/');
                        checksums.put(relativePath, sha256(path));
                    } catch (Exception e) {
                        throw new RuntimeException("Failed to hash " + path, e);
                    }
                });
        }
        return checksums;
    }

    private String sha256(Path file) throws Exception {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        final byte[] bytes = Files.readAllBytes(file);
        final byte[] hash = digest.digest(bytes);

        final StringBuilder builder = new StringBuilder(hash.length * 2);
        for (byte b : hash) {
            builder.append(String.format("%02x", b));
        }
        return builder.toString();
    }

    @After
    public void cleanup() throws Exception {
        if (testRootDir != null && testRootDir.exists()) {
            FileUtils.deleteDirectory(testRootDir);
        }
    }
}
