#!/usr/bin/env python3
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

"""Unit tests for build/generate-changes.py section-assignment logic.

These cover the three cases that previously produced "(No Section)" entries:

  1. dependabot / CI PRs whose only GitHub labels are github_actions /
     dependencies (label -> section mapping);
  2. rebase-merged PRs that land several commits under one PR number
     (PR-label fan-out); and
  3. JIRA tickets with no usable component, filed under the section named
     by their commit's Client: trailer instead (JIRA -> trailer fallback).

No network access is required: only the pure mapping/assignment helpers are
exercised.
"""

import importlib.util
import os
import unittest

# generate-changes.py has a hyphen in its name, so it cannot be imported with a
# plain ``import``; load it as a module from its path instead.
_HERE = os.path.dirname(os.path.abspath(__file__))
_SPEC = importlib.util.spec_from_file_location(
    "generate_changes", os.path.join(_HERE, "generate-changes.py")
)
gc = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(gc)


def make_commit(sha="0" * 40, pr_num=None, sections=None, tickets=()):
    """Build a commit_meta entry like generate_changes() constructs."""
    return {
        "sha": sha,
        "short": sha[:9],
        "subject": sha,
        "tickets": set(tickets),
        "sections": list(sections or []),
        "pr_num": pr_num,
    }


class LabelMappingTests(unittest.TestCase):
    """Fix 1: dependabot / CI labels route to the Build Process section."""

    def test_github_actions_maps_to_build_process(self):
        self.assertEqual(gc.GITHUB_LABEL_MAP["github_actions"], "Build Process")

    def test_dependencies_maps_to_build_process(self):
        self.assertEqual(gc.GITHUB_LABEL_MAP["dependencies"], "Build Process")

    def test_testsuite_maps_to_build_process(self):
        self.assertEqual(gc.GITHUB_LABEL_MAP["testsuite"], "Build Process")

    def test_doc_maps_to_documentation(self):
        self.assertEqual(gc.GITHUB_LABEL_MAP["doc"], "Documentation")

    def test_labels_to_sections_maps_and_dedupes_preserving_order(self):
        self.assertEqual(
            gc.labels_to_sections(["golang", "github_actions", "dependencies"]),
            ["Go", "Build Process"],
        )

    def test_labels_to_sections_is_case_insensitive(self):
        self.assertEqual(gc.labels_to_sections(["GitHub_Actions"]), ["Build Process"])

    def test_labels_to_sections_ignores_unknown_labels(self):
        self.assertEqual(gc.labels_to_sections(["totally-unknown-label"]), [])

    def test_labels_to_sections_accepts_a_generator(self):
        # fetch_pr_labels() passes a generator expression, not a list.
        self.assertEqual(
            gc.labels_to_sections(name for name in ["php"]), ["PHP"]
        )


class PrLabelFanoutTests(unittest.TestCase):
    """Fix 2: a PR's labels reach every section-less commit of that PR."""

    def test_multiple_commits_one_pr_all_get_labelled(self):
        # Regression for the dict-collision bug: a rebase-merged PR lands two
        # section-less commits; both must receive the PR's label section.
        commits = [
            make_commit("a" * 40, pr_num=100),
            make_commit("b" * 40, pr_num=100),
        ]
        candidates = gc.pr_label_fetch_candidates(commits, {})
        self.assertEqual(len(candidates), 2)
        gc.apply_pr_label_sections(candidates, {100: ["Build Process"]})
        self.assertEqual(commits[0]["sections"], ["Build Process"])
        self.assertEqual(commits[1]["sections"], ["Build Process"])

    def test_commit_with_trailer_section_is_not_clobbered(self):
        # Mirrors PR #3385: one commit carries a Client: js trailer, its sibling
        # carries none.  Only the section-less sibling is (re)assigned.
        with_trailer = make_commit("c" * 40, pr_num=3385, sections=["JavaScript"])
        without = make_commit("d" * 40, pr_num=3385)
        candidates = gc.pr_label_fetch_candidates([with_trailer, without], {})
        self.assertEqual(candidates, [without])
        gc.apply_pr_label_sections(candidates, {3385: ["nodejs"]})
        self.assertEqual(with_trailer["sections"], ["JavaScript"])
        self.assertEqual(without["sections"], ["nodejs"])

    def test_fetch_list_is_deduplicated(self):
        commits = [
            make_commit("a" * 40, pr_num=100),
            make_commit("b" * 40, pr_num=100),
        ]
        candidates = gc.pr_label_fetch_candidates(commits, {})
        self.assertEqual(sorted({c["pr_num"] for c in candidates}), [100])

    def test_commit_covered_by_jira_is_not_a_candidate(self):
        commits = [make_commit("e" * 40, pr_num=200, tickets=["THRIFT-1"])]
        self.assertEqual(
            gc.pr_label_fetch_candidates(commits, {"THRIFT-1": {}}), []
        )

    def test_commit_without_pr_number_is_not_a_candidate(self):
        self.assertEqual(
            gc.pr_label_fetch_candidates([make_commit("f" * 40, pr_num=None)], {}), []
        )

    def test_apply_copies_sections_and_does_not_alias(self):
        commit = make_commit("a" * 40, pr_num=100)
        shared = ["Build Process"]
        gc.apply_pr_label_sections([commit], {100: shared})
        self.assertEqual(commit["sections"], ["Build Process"])
        self.assertIsNot(commit["sections"], shared)


class JiraTrailerFallbackTests(unittest.TestCase):
    """Fix 3: a componentless JIRA ticket falls back to its Client: trailer."""

    def test_build_ticket_trailer_sections(self):
        commits = [
            make_commit(tickets=["THRIFT-6068"], sections=["Rust"]),
            make_commit(tickets=["THRIFT-6069"], sections=["Python"]),
        ]
        mapping = gc.build_ticket_trailer_sections(commits)
        self.assertEqual(mapping["THRIFT-6068"], ["Rust"])
        self.assertEqual(mapping["THRIFT-6069"], ["Python"])

    def test_build_ticket_trailer_sections_dedupes(self):
        commits = [
            make_commit("a" * 40, tickets=["THRIFT-1"], sections=["Go"]),
            make_commit("b" * 40, tickets=["THRIFT-1"], sections=["Go"]),
        ]
        self.assertEqual(
            gc.build_ticket_trailer_sections(commits)["THRIFT-1"], ["Go"]
        )

    def test_fallback_used_when_jira_has_no_component(self):
        trailer = {"THRIFT-6068": ["Rust"]}
        self.assertEqual(
            gc.resolve_ticket_sections("THRIFT-6068", ["(No Section)"], trailer),
            ["Rust"],
        )

    def test_jira_component_wins_over_trailer(self):
        trailer = {"THRIFT-6069": ["Rust"]}
        self.assertEqual(
            gc.resolve_ticket_sections("THRIFT-6069", ["Python"], trailer),
            ["Python"],
        )

    def test_no_section_kept_when_no_trailer_available(self):
        self.assertEqual(
            gc.resolve_ticket_sections("THRIFT-9999", ["(No Section)"], {}),
            ["(No Section)"],
        )


if __name__ == "__main__":
    unittest.main()
