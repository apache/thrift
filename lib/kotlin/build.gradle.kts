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
import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm")
    id("com.ncorti.ktfmt.gradle")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(platform("org.jetbrains.kotlin:kotlin-bom"))
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-jdk8:1.10.1")
    implementation("org.apache.thrift:libthrift:INCLUDED")
    testImplementation(kotlin("test"))
}

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}

tasks.withType<KotlinCompile> {
    compilerOptions {
        jvmTarget = JvmTarget.JVM_1_8
        freeCompilerArgs = listOf("-Xjdk-release=1.8")
    }
}

tasks {
    if (JavaVersion.current().isJava11Compatible) {
        ktfmt {
            kotlinLangStyle()
        }
    }

    test {
        useJUnitPlatform()
    }

    task<Exec>("compileThrift") {
        val thriftBin = if (hasProperty("thrift.compiler")) {
            file(property("thrift.compiler")!!)
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
            layout.projectDirectory.file("src/test/resources/AnnotationTest.thrift").asFile.absolutePath
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
