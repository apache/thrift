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
    kotlin("jvm")
    java
    application
    id("com.ncorti.ktfmt.gradle")
}

repositories {
    mavenCentral()
}

val slf4jVersion: String by project
val httpcoreVersion: String by project
val logbackVersion: String by project
val kotlinxCoroutinesJdk8Version: String by project
val cliktVersion: String by project

dependencies {
    implementation(platform("org.jetbrains.kotlin:kotlin-bom"))
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    // clikt is used to drive command line parsing and validation
    implementation("com.github.ajalt.clikt:clikt:$cliktVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-jdk8:$kotlinxCoroutinesJdk8Version")
    implementation("org.apache.thrift:libthrift:INCLUDED")
    implementation("org.slf4j:slf4j-api:$slf4jVersion")
    implementation("org.apache.httpcomponents:httpcore:$httpcoreVersion")
    implementation("ch.qos.logback:logback-classic:$logbackVersion")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
    testImplementation("org.jetbrains.kotlin:kotlin-test-junit")
}

kotlin {
    jvmToolchain {
        (this as JavaToolchainSpec).languageVersion.set(JavaLanguageVersion.of(8))
    }
}

tasks {
    application {
        applicationName = "TestServer"
        mainClass.set("org.apache.thrift.test.TestServerKt")
    }

    if (JavaVersion.current().isJava11Compatible) {
        ktfmt {
            kotlinLangStyle()
        }
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
