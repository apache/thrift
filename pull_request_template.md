#### Instructions for Apache Thrift Pull Requests ####

1. All code changes require an [Apache Jira THRIFT Issue](http://issues.apache.org/jira/browse/THRIFT) ticket.

1. All pull requests should contain a single commit per issue, or we will ask you to squash it.
1. The pull request title must begin with the Jira THRIFT ticket identifier, for example:

        THRIFT-9999: an example pull request title

1. Commit messages must follow this pattern for code changes (deviations will not be merged):

        THRIFT-9999: [summary of fix, one line if possible]
        Client: [language(s) affected, comma separated, use lib/ directory names please]

1. Remove these instructions from any pull request description.

For more information about committing, see [CONTRIBUTING](CONTRIBUTING.md).