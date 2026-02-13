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

import org.junit.Test;
import org.w3c.dom.Document;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;
import java.io.File;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class TestThreadSafePluginDescriptor {

    @Test
    public void testGoalsAreMarkedThreadSafe() throws Exception {
        final File pluginDescriptor = new File("target/classes/META-INF/maven/plugin.xml");
        assertTrue("Expected generated plugin descriptor: " + pluginDescriptor, pluginDescriptor.isFile());

        final DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        documentBuilderFactory.setNamespaceAware(false);
        final Document document = documentBuilderFactory.newDocumentBuilder().parse(pluginDescriptor);

        final XPath xPath = XPathFactory.newInstance().newXPath();
        assertEquals("true", xPath.evaluate("/plugin/mojos/mojo[goal='compile']/threadSafe/text()", document));
        assertEquals("true", xPath.evaluate("/plugin/mojos/mojo[goal='testCompile']/threadSafe/text()", document));
    }
}
