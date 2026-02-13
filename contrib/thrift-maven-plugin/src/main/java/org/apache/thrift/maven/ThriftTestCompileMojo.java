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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.apache.maven.artifact.Artifact;
import com.google.common.collect.ImmutableList;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 */
@Mojo(
        name = "testCompile",
        defaultPhase = LifecyclePhase.GENERATE_TEST_SOURCES,
        requiresDependencyResolution = ResolutionScope.TEST,
        threadSafe = true
)
public final class ThriftTestCompileMojo extends AbstractThriftMojo {

    /**
     * The source directories containing the sources to be compiled.
     *
     */
    @Parameter(defaultValue = "${basedir}/src/test/thrift", required = true)
    private File thriftTestSourceRoot;

    /**
     * This is the directory into which the {@code .java} will be created.
     *
     */
    @Parameter(defaultValue = "${project.build.directory}/generated-test-sources/thrift", required = true)
    private File outputDirectory;

    @Override
    protected void attachFiles() {
        project.addTestCompileSourceRoot(outputDirectory.getAbsolutePath());
        projectHelper.addTestResource(project, thriftTestSourceRoot.getAbsolutePath(),
        		ImmutableList.of("**/*.thrift"), null);
    }

    @Override
    protected List<Artifact> getDependencyArtifacts() {
        return new ArrayList<Artifact>(project.getArtifacts());
    }

    @Override
    protected File getOutputDirectory() {
        return outputDirectory;
    }

    @Override
    protected File getThriftSourceRoot() {
        return thriftTestSourceRoot;
    }

    /**
     * Set the local Maven repository path. Exposed only to allow testing outside of Maven itself.
     *
     * @param localRepositoryPath local Maven repository path
     */
    public void setLocalRepositoryPath(final String localRepositoryPath) {
        this.localRepositoryPath = localRepositoryPath;
    }

    /**
     * Set the option to hash dependent JAR paths. Exposed only to allow testing outside of Maven itself.
     *
     * @param hashDependentPaths whether or not to hash paths to dependent JARs
     */
    public void setHashDependentPaths(final boolean hashDependentPaths) {
        this.hashDependentPaths = hashDependentPaths;
    }
}
