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
- The human author remains responsible for reviewing, testing, and standing behind all submitted code.

---

## 5. Language-Specific Rules (`/lib`, `/test`, `/tutorial`)

- This file remains valid in all cases and must be used in addition to any additional language-specific rules.
- If a target-language directory under `/lib/<lang>/` contains its own `CLAUDE.md` or `AGENTS.md`, those rules apply to all work in that language directory.
- Those language-specific rules extend **by implication** to the corresponding language code under `/test/` and `/tutorial/`.
- If `/test/` or `/tutorial/` themselves contain a `CLAUDE.md`/`AGENTS.md` for a given language, **combine** the rules: the file **closer to the code** (i.e., in the same directory) takes precedence on any conflict.

---

## 6. Quick Reference Checklist (before opening a PR)

- [ ] License of any new dependency checked against [ASF Category A/X list](https://www.apache.org/legal/resolved.html)
- [ ] `LICENSE` and/or `NOTICE` updated if third-party attribution is required
- [ ] JIRA ticket exists (unless truly trivial)
- [ ] PR title starts with `THRIFT-NNNN:` (if ticket exists)
- [ ] Commit message includes affected `Client:` languages
- [ ] Single squashed commit
- [ ] Tests added or updated
- [ ] `make style` passes
- [ ] AI authorship labelled with `Co-Authored-By:` / `Generated-by:` where applicable
