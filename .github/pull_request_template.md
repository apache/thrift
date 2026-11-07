<!--
Thank you for the pull request. Each heading below is a question a reviewer
asks on every Thrift PR. Answer it in plain text under the heading. "No" and
"None" are answers; a deleted heading is not. These comments are hidden when
the description is rendered, so leaving them in is fine.

The rules live in CONTRIBUTING.md and AGENTS.md at the repository root.
A prose-only edit of existing documentation ends the PR title and the commit
subject with the CI-skip marker described in AGENTS.md section 7; nothing
else does, and the marker never appears in this description.
-->

Jira: THRIFT-

<!--
Every change except a typo or a trivial compiler warning has a ticket in
https://issues.apache.org/jira/browse/THRIFT (account request:
https://selfserve.apache.org/jira-account.html). Complete the key on the line
above; the repository turns a bare key such as THRIFT-5929 into a link. The
same key starts the PR title and the commit subject, "THRIFT-5929: Short
description of the change", and Jira links the ticket to this PR from the
title. For a change too small for a ticket, write the word none after the
colon, followed by the reason.
-->

## Why is this change needed?

<!--
The problem as a user or maintainer meets it, and what happens today. The
ticket may hold the long version; the reviewer still needs one paragraph here.
-->

## What does this PR change?

<!--
What the diff does not show: the approach and why you chose it, what you left
out on purpose, and the languages touched (compiler, lib/<lang>, test/<lang>,
tutorial/<lang>). Repeat the commit message's "Client:" line here, for example
"Client: cpp,py,java".
-->

## How was it verified?

<!--
The test that fails without the change and passes with it, or the command you
ran and its result, for example:
  gradle test --tests '*TestRequiredUnionValidation' -> master 2 of 5 fail,
  this branch 5 of 5 pass
Name what you could not run (another platform, the cross tests). "Existing
tests cover it" names the test. A change without a test gives the reason.
-->

## Does anything change for users?

<!--
Public API, generated code, wire format, defaults, build or packaging, or
behaviour an application can observe; or "No". A breaking change says what
breaks and how to migrate, and the Jira ticket gets the "Breaking-Change"
label. Say which languages, when not all of them.
-->

## Was AI used?

<!--
"No", or the tool and version and what it did (wrote the code, wrote the
tests, review only). An AI-assisted commit also carries a "Co-Authored-By:" or
"Generated-by:" trailer, see AGENTS.md section 4. No links to AI sessions or
conversations anywhere in the commit or here. CI rejects them, except on a
prose-only change, where the CI-skip marker stops that check as well; check
them yourself there.
-->
