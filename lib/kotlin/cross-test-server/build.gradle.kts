/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

plugins {
    kotlin("jvm") version "1.5.31"
    id("com.ncorti.ktfmt.gradle") version "0.4.0"
    java
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(platform("org.jetbrains.kotlin:kotlin-bom"))
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    // https://mvnrepository.com/artifact/org.jetbrains.kotlinx/kotlinx-coroutines-jdk8
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-jdk8:1.6.1")
    // https://mvnrepository.com/artifact/org.apache.thrift/libthrift
    implementation("org.apache.thrift:libthrift:INCLUDED")
    // https://mvnrepository.com/artifact/ch.qos.logback/logback-classic
    implementation("ch.qos.logback:logback-classic:1.3.0-alpha14")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
    testImplementation("org.jetbrains.kotlin:kotlin-test-junit")
}

tasks {
    application {
        applicationName = "TestServer"
        mainClass.set("org.apache.thrift.test.TestServerKt")
    }

    ktfmt {
        kotlinLangStyle()
    }

    task<Exec>("compileThrift") {
        val thriftBin = if (hasProperty("thrift.compiler")) {
            file(property("thrift.compiler"))
        } else {
            project.rootDir.resolve("../../compiler/cpp/thrift")
        }
        val outputDir = layout.buildDirectory.dir("generated-sources")
        doFirst {
            mkdir(outputDir)
        }
        commandLine = listOf(
            thriftBin.absolutePath,
            "-gen",
            "kotlin",
            "-out",
            outputDir.get().toString(),
            project.rootDir.resolve("../../test/ThriftTest.thrift").absolutePath
        )
        group = LifecycleBasePlugin.BUILD_GROUP
    }

    compileKotlin {
        dependsOn("compileThrift")
    }
}

sourceSets["main"].java {
    srcDir(layout.buildDirectory.dir("generated-sources"))
}
