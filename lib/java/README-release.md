# How to release/deploy to cloudsmith
Notes:
* created a pom.xml so we could use maven to release our changes. no changes to gradle build and code.
* cloudsmith is our internal repo and it would take precedence of any public repo
* so (for now) we could deploy our customized version of Thrift to cloudsmith and shadow the official Thrift

## High level steps
1. assume your local env already has a valid settings.xml for cloudsmith.
1. make any changes on feature branch then get it merged to master branch
1. merge changes from master branch to release branch
1. to deploy a snapshot version run `mvn clean deploy`, it should deploy it to cloudsmith
1. to release and deploy run `mvn -B release:prepare release:perform`
