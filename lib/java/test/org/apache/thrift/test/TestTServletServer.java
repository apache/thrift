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

package org.apache.thrift.test;

import org.apache.catalina.core.StandardContext;
import org.apache.catalina.startup.Tomcat;
import org.apache.catalina.startup.Tomcat.FixContextListener;


/**
 * run tomcat for test TServlet
 */
public class TestTServletServer {

  static final int port = 9090;

  public static void main(String [] args) throws Exception{
    Tomcat tomcat = new Tomcat();
    tomcat.setPort( port );
    tomcat.setBaseDir(System.getProperty("user.dir")+"\\build");
    tomcat.getHost().setAutoDeploy( false );

    String contextPath = "/test";
    StandardContext context = new StandardContext();
    context.setPath( contextPath );
    context.addLifecycleListener( new FixContextListener() );
    tomcat.getHost().addChild( context );

    tomcat.addServlet( contextPath, "testServlet", new TestServlet() );
    context.addServletMappingDecoded( "/service", "testServlet");
    tomcat.start();
    tomcat.getServer().await();
  }
  
}
