# Apache Thrift — AI Contribution Guidelines

This file governs AI-assisted work on the Apache Thrift codebase.
It supplements but does **not** replace [`CONTRIBUTING.md`](CONTRIBUTING.md).

---

## 1. ASF Legal Compliance (Third-Party Code)

Apache Thrift is an [Apache Software Foundation (ASF)](https://www.apache.org/) project released under the **Apache License 2.0**.
The AI **must** actively enforce and monitor ASF licensing policy:

- **Proactively flag conflicts**: Before introducing any dependency, snippet, or code derived from an external source, verify its license is compatible with Apache 2.0.
  Incompatible licenses include (non-exhaustive): GPL, AGPL, SSPL, BUSL, CC-BY-NC.
  Compatible examples: MIT, BSD-2/3, Apache 2.0, ISC, MPL 2.0 (with caveats).
- **Category X / Category A**: Follow the [ASF Third-Party Licensing Policy](https://www.apache.org/legal/resolved.html).
  Category A licenses may be included; Category X licenses must **never** be introduced.
- **Update `LICENSE` and `NOTICE`**: When adding third-party code or binaries that require attribution, add the appropriate notices to `LICENSE` and/or `NOTICE` following the [ASF guide on licenses and notices](https://www.apache.org/dev/licensing-howto.html).
  If in doubt whether an entry is required, **add it and flag it in the PR description** for committer review.
- **Generative AI output**: The [ASF Generative Tooling Guidance](https://www.apache.org/legal/generative-tooling.html) applies. Be aware that AI-generated code may unintentionally reproduce copyrighted material. Flag any non-trivial generated blocks in commit messages or PR descriptions.

---

## 2. Issue Tracking

| Type | Tracker | Notes |
|---|---|---|
| Significant changes | [Apache JIRA — THRIFT project](https://issues.apache.org/jira/browse/THRIFT) | Required for all non-trivial PRs |
| Minor / quick fixes | GitHub Issues | Typos, trivial compiler warnings, etc. |

**JIRA integration with GitHub**: Including a JIRA ticket identifier at the start of a PR title automatically creates a link from JIRA to the PR.

- PR title format: `THRIFT-9999: Short description of the change`
- Commit message format (required for code changes):
  ```
  THRIFT-9999: Short description of the change
  Client: cpp,py,java   (comma-separated list of affected languages)
  ```

Example: [THRIFT-5929](https://issues.apache.org/jira/projects/THRIFT/issues/THRIFT-5929) → [PR #3350](https://github.com/apache/thrift/pull/3350).

---

## 3. Pull Request Requirements

Follow [`CONTRIBUTING.md`](CONTRIBUTING.md) in full. Key points:

- One commit per issue (squash before submitting).
- All significant changes need a JIRA ticket.
- Provide tests for every submitted change.
- Verify coding standards: `make style`.
- Branch name convention: use the JIRA ticket ID, e.g. `THRIFT-9999`.
- PRs go from your fork branch → `apache:master`.

---

## 4. AI-Generated Contributions

Per [`CONTRIBUTING.md § AI generated content`](CONTRIBUTING.md#ai-generated-content) and the [ASF Generative Tooling Guidance](https://www.apache.org/legal/generative-tooling.html):

- **Always** label AI-assisted commits and PRs. Use one or both of:
  ```
  Co-Authored-By: <AI tool name and version>
  Generated-by: <AI tool name and version>
  ```
  Example:
  ```
  THRIFT-9999: Fix connection timeout handling in Go client
  Client: go

  Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>
  ```
- Apply this label even when AI only generated a portion of the change.
- **Never** put tool-internal links into a commit message, PR description, JIRA comment or e-mail.
  This includes AI session and conversation URLs — `Claude-Session:`,
  `https://claude.ai/code/session_…`, `https://chatgpt.com/c/…` and any equivalent from another
  tool. Some assistants append one by default; that default does not apply to this project and
  must be overridden. The attribution line above is the whole of what belongs in a trailer block.
  Such a link is permanent once it reaches git history, means nothing to a reviewer, and may point
  at material that is not public — including work still under embargo per §6. Note that a
  force-push does not undo this: the superseded commit stays reachable by SHA.
- The human author remains responsible for reviewing, testing, and standing behind all submitted code.

---

## 5. Language-Specific Rules (`/lib`, `/test`, `/tutorial`)

- This file remains valid in all cases and must be used in addition to any additional language-specific rules.
- If a target-language directory under `/lib/<lang>/` contains its own `CLAUDE.md` or `AGENTS.md`, those rules apply to all work in that language directory.
- Those language-specific rules extend **by implication** to the corresponding language code under `/test/` and `/tutorial/`.
- If `/test/` or `/tutorial/` themselves contain a `CLAUDE.md`/`AGENTS.md` for a given language, **combine** the rules: the file **closer to the code** (i.e., in the same directory) takes precedence on any conflict.

---

## 6. Security Work

When assisting with security-sensitive changes (transport size limits, TLS configuration,
authentication, serialization bounds, or anything flagged by the project's security team):

- **Never** describe the change as a security fix in public-facing text — commit messages,
  PR titles, PR descriptions, or inline comments.  Use neutral functional language:
  *"add a configurable frame-size limit"* rather than *"fix DoS vulnerability"*.
  Vulnerability details travel through the private ASF channel (`security@apache.org`);
  AI tooling must not short-circuit that process.
- Before proposing mitigations or defaults, consult
  [`doc/thrift-threat-model.md`](doc/thrift-threat-model.md) for the project's documented
  attack surface, trust boundaries, and per-binding security properties.
- External reporters and AI reviewers alike should follow the instructions in
  [`SECURITY.md`](SECURITY.md) for responsible disclosure.
- When asserting a security claim or identifying which component a bug affects, 
  verify reachability/exploitability empirically against the specific released version before stating it as fact.

---

## 7. Code Changes, Git & PR Workflow

- When fixing a bug or addressing an issue, scope the change to ONLY that issue. 
  Do not touch CHANGES.md, version files, or unrelated entries unless explicitly asked.
- Ship bug fixes as standalone pull requests by default. Do not commit directly to master 
  or mix fixes into existing/unrelated work unless told otherwise.
- Follow a strict test-first workflow: write or update tests demonstrating the bug BEFORE applying the fix, 
  and inspect any generated code before changing it.
- **Prose-only changes skip CI.** A commit that only edits the text of existing documentation
  files ends its subject line with `[skip ci]`, and the PR title carries it too:
  ```
  THRIFT-9999: Clarify the TLS options in lib/cpp/README.md [skip ci]
  ```
  GitHub then runs none of the `push` and `pull_request` workflows for it, and AppVeyor skips
  the build.
  - Only the subject line works for both: AppVeyor ignores the rest of the message. A squash
    merge of several commits takes its subject from the PR title.
  - Not prose-only, so no marker: source code, comments and doc comments included; build,
    packaging and CI files; tests and test data (`compiler/cpp/test/compiler/DocTest.md` is a
    golden file, not documentation); adding, removing or renaming a file. `EXTRA_DIST` in the
    `Makefile.am` files lists documentation files by name: a removed or renamed one breaks the
    `make dist` job, and a new one needs an entry of its own, which no CI job asks for. Several
    package manifests name a README, too. When in doubt, leave the marker out.
  - The marker can skip CI for everything pushed along with it, so push a marked commit on its
    own. When a review round adds anything but prose, drop the marker as you amend.
  - GitHub also matches the marker when it is quoted. A commit that is not prose-only must not
    contain it, or GitHub's variants `[ci skip]`, `[no ci]`, `[skip actions]` and
    `[actions skip]`, anywhere in its message. A squash message built from a PR description
    picks it up from the checklist in the PR template.
  - The marker also stops the tool-internal links check (§4) from running, so check the commit
    message (`git log --format=%B`) and the PR text yourself before pushing.

---

## 8. Quick Reference Checklist (before opening a PR)

- [ ] License of any new dependency checked against [ASF Category A/X list](https://www.apache.org/legal/resolved.html)
- [ ] `LICENSE` and/or `NOTICE` updated if third-party attribution is required
- [ ] JIRA ticket exists (unless truly trivial)
- [ ] PR title starts with `THRIFT-NNNN:` (if ticket exists)
- [ ] Commit message includes affected `Client:` languages
- [ ] Single squashed commit
- [ ] Prose-only change: `[skip ci]` ends the commit subject and the PR title; any other change: no such marker anywhere in the commit message
- [ ] Tests added or updated
- [ ] `make style` passes
- [ ] AI authorship labelled with `Co-Authored-By:` / `Generated-by:` where applicable
- [ ] No tool-internal or AI session URLs anywhere in the commit message or PR text
- [ ] Security-sensitive changes use neutral commit/PR language (no public vulnerability details)
- [ ] Changes touching transport limits / TLS / auth cross-checked against `doc/thrift-threat-model.md`
