<!-- Explain the changes in the pull request below: -->
  


<!-- We recommend you review the checklist before submitting a pull request. -->

- [ ] Did you squash your changes to a single commit?

- [ ] Do you need an [Apache Jira](https://issues.apache.org/jira/projects/THRIFT/issues/) ticket?<details><summary>Expand for guidance...</summary>
    - `Yes` if your change requires a release note.
    - `Yes` if your change is a breaking change.
    - `No` if you change is trivial, such as fixing a typo.
</details>
 
- [ ] Is this change worthy of a release note? <details><summary>Examples of Release Note-worthy examples...</summary>
    - Breaking Changes
    - New, Deprecated, or Removed Languages
    - Security Fixes
    - Significant Refactoring
    - Changing how the product is built
</details>

- [ ] Breaking changes have additional requirements: <details><summary>Expand for instructions...</summary>
    - Add or reference an existing Apache Jira THRIFT ticket.
    - Add a `Breaking-Change` label to the Jira ticket.
    - Add a note to the `lib/<language>/README.md` file.
    - Add a line to the `CHANGES.md` file.
</details>

- [ ] Does this change require a build? <details><summary>Expand for guidance...</summary>
    - `Yes` for any code change
    - `Yes` for any build script change
    - `Yes` for any docker build environment change
    - `Yes` for any change affecting the cross test suite
    - `No` for documentation-only changes
    - `No` for trivial changes, for example fixing a typo.
    <br/>
    If your change does not require a build, you can add [ci skip] to the end of your commit message.<br/>
    This will avoid costly and unnecessary builds in both the pull request and once it is merged.
</details>

<!--
  The Contributing Guide at:
  https://github.com/apache/thrift/blob/master/CONTRIBUTING.md
  has more details and tips for committing properly.
-->
