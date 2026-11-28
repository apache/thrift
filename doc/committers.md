<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

## Process used by committers to review and submit patches

1. Make sure that there is an issue for the patch(s) you are about to commit in our [Jira issue tracker](http://issues.apache.org/jira/browse/THRIFT)

1. Check out the latest version of the source code

  * git clone https://github.com/apache/thrift.git thrift

1. Apply the patch

  * curl https://issues.apache.org/jira/... |git apply --ignore-space-change

  or

  * curl https://github.com/<GitHub User>/thrift/commit/<Commit ID>.patch |git apply --ignore-space-change


1. Inspect the applied patch to ensure that all [Legal aspects on Submission of Contributions (Patches)](http://www.apache.org/licenses/LICENSE-2.0.html#contributions) are met

1. Run the necessary unit tests and cross-language test cases to verify the patch

1. Commit the patch

        git --config user.name "Your Name"
        git --config user.email "YourApacheID@apache.org"
        git add -A
        git commit

1. The commit message should be in the format:

       THRIFT-####:<Jira description>
       Client: <component>
       Patch: <Name of person contributing the patch>

       Description of what was fixed or addressed.

       If this is a github pull request then add the below comment to automatically close the GitHub request, 
	   where #NNNN is the PR number:
       
        This closes #NNNN


1. Double check the patch committed and that nothing was missed then push the patch

       git status
       git show HEAD
       git push origin master


1. Resolve the Jira issue and set the following for the changelog

  * Component the patch is for
  * fixVersion to the current version on master
