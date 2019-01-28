### Pull Request Guidance ###

Review the following items to ensure a smooth pull request experience.

- [ ] Did you make a breaking change?  If so:

  - [ ] Add (or reference) an [Apache Jira](https://issues.apache.org/jira/projects/THRIFT/issues/) THRIFT ticket.
  - [ ] Add a `Breaking-Change` label to the Jira ticket.
  - [ ] Add a note to the `lib/<language>/README.md` file.
  - [ ] Add a line to the `CHANGES.md` file.

- [ ] Is this change significant enough to be in release notes?

    All release-note worthy changes require an [Apache Jira](https://issues.apache.org/jira/projects/THRIFT/issues/) THRIFT ticket.
    For example:
    - Breaking Changes
    - New, Deprecated, or Removed Languages
    - Security Fix
    - Significant Refactoring

- [ ] If there is an [Apache Jira](https://issues.apache.org/jira/projects/THRIFT/issues/) ticket: 

  - [ ] Is the [Apache Jira](https://issues.apache.org/jira/projects/THRIFT/issues/) THRIFT ticket identifier in the PR title?

        THRIFT-9999: an example pull request title

  - [ ] Is the [Apache Jira](https://issues.apache.org/jira/projects/THRIFT/issues/) THRIFT ticket identifier and affected languages in the commit message?

        THRIFT-9999: [summary of fix, one line if possible]
        Client: [language(s) affected, comma separated, use lib/ directory names please]

- [ ] Did you squash your changes to a single commit?

        Committers can squash when they merge, but sometimes we forget, and it makes the history
        pretty dirty.  Please squash your pull requests to a single commit if you can.

For more information about committing, see CONTRIBUTING.md
