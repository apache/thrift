## Process used by committers to review and submit patches

1. Make sure that there is an issue for the patch(s) you are about to commit in our [Jira issue tracker]({{ conf.jira_url }})
 
1. Check out the latest version of the source code
	
	* git clone https://git-wip-us.apache.org/repos/asf/thrift.git thrift 

1. Apply the patch
	
	* curl https://issues.apache.org/jira/... |git apply --ignore-space-change
	
	or
	
	* curl https://github.com/<GitHub User>/thrift/commit/<Commit ID>.patch |git apply --ignore-space-change
	
	
1. Inspect the applied patch to ensure that all [Legal aspects on Submission of Contributions (Patches)](http://www.apache.org/licenses/LICENSE-2.0.html#contributions) are met

1. Run the necessary unit tests and cross language test cases to verify the patch

1. Commit the patch

		git --config user.name "Your Name"
		git --config user.email "YourApacheID@apache.org"
		git add -A
		git commit
		
		
1. The commit message should be in the format:
	
		THRIFT-###:<Jira description>
		Client: <component>
		Patch: <Name of person contributing the patch>
		
		Description of what was fixed or addressed.
		
		<%
			if this is a github pull request then copy the below block 
			from the GitHub email that came to dev@ list, this will 
			automatically close the GitHub pull request 
		%>
		Github Pull Request: This closes #XX
		----
		commit 1234567
		Author: docbrown <docbrown@example.com>
		Date:   1985-06-03T01:21:00Z

    		fix for THRIFT-1234

    		fix for THRIFT-1234 fixes the flux capacitor


1. Double check the patch committed and that nothing was missed then push the patch

		git status
		git show HEAD
		git push origin master

		
1. Resolve the jira issue and set the following for the changelog

	* Component the patch is for  
	* fixVersion to the current version on master
	

 
