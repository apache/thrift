## How to contribute

 1. Make sure your issue is not all ready in the [Jira issue tracker](http://issues.apache.org/jira/browse/THRIFT)
 1. If not, create a ticket describing the change you're proposing in the [Jira issue tracker](http://issues.apache.org/jira/browse/THRIFT)
 1. Contribute your patch using one of the two methods below
 
### Contributing via a patch
 
1. Check out the latest version of the source code
	
	* git clone https://git-wip-us.apache.org/repos/asf/thrift.git thrift 

1. Modify the source to include the improvement/bugfix
	
	* Verify that you follow the same CodingStyle you see within the language you are working on
	* Verify that your change works by adding a unit test.

1. Create a patch from project root directory (e.g. you@dev:~/thrift $ ):
	
	* git diff > ../thrift-XXX-my-new-feature.patch

1. Attach the newly generated patch to the issue
1. Wait for other contributors or committers to review your new addition
1. Wait for a committer to commit your patch
 
### Contributing via GitHub pull requests

1. Create a fork for http://github.com/apache/thrift
1. Create a branch with the jira ticket number you are working on
1. Modify the source to include the improvement/bugfix
	
	* Verify that you follow the same CodingStyle you see within the language you are working on
	* Verify that your change works by adding a unit test. 

1. Issue a pull request for your new feature
1. Wait for other contributors or committers to review your new addition
1. Wait for a committer to commit your patch

### More info
 
 Plenty of information on why and how to contribute is available on the Apache Software Foundation (ASF) web site. In particular, we recommend the following:
 
 * [Contributors Tech Guide](http://www.apache.org/dev/contributors)
 * [Get involved!](http://www.apache.org/foundation/getinvolved.html)
 * [Legal aspects on Submission of Contributions (Patches)](http://www.apache.org/licenses/LICENSE-2.0.html#contributions)
