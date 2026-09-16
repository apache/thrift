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

"""Unit tests for build/generate-changes.py.

These cover the three cases that previously produced "(No Section)" entries:

  1. dependabot / CI PRs whose only GitHub labels are github_actions /
     dependencies (label -> section mapping);
  2. rebase-merged PRs that land several commits under one PR number
     (PR-label fan-out); and
  3. JIRA tickets with no usable component, filed under the section named
     by their commit's Client: trailer instead (JIRA -> trailer fallback).

They also cover which referenced tickets get a JIRA line: only those JIRA
files under the release version.  A commit that merely mentions an old
ticket (THRIFT-6183 citing THRIFT-1337) must not list it.  Those tests run
generate_changes() against a temporary git repository and a fake JIRA.

No network access is required.
"""

import argparse
import contextlib
import importlib.util
import io
import json
import os
import re
import shutil
import subprocess
import tempfile
import unittest
import urllib.error
import urllib.parse
from unittest import mock

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


def jira_fields(summary, components=(), fix_versions=(), status="Resolved",
                resolution="Fixed"):
    """Build an issue's "fields" object as the JIRA REST API returns it."""
    return {
        "summary": summary,
        "components": [{"name": c} for c in components],
        "fixVersions": [{"name": v} for v in fix_versions],
        "status": {"name": status},
        "resolution": {"name": resolution} if resolution else None,
    }


class FakeJira:
    """Stands in for urllib.request.urlopen and answers the two JIRA searches
    generate-changes.py sends ("key in (...)" and the fixVersion query) from
    a {key: fields} table.  Like the real REST API it returns only the
    fields the request asked for, and it rejects a key list that names an
    unknown key when it validates the query.  Any other request fails the
    test."""

    # issues.apache.org checks that each key exists only in key lists up to
    # this length, and only while validateQuery is on (the default).
    VALIDATED_KEY_LIST = 25

    def __init__(self, issues):
        self.issues = issues

    @staticmethod
    def matches_fix_version_query(jql, fields, version):
        # resolution != Unresolved [AND resolution not in (...)]
        # AND fixVersion = version AND status != Open
        excluded = re.search(r"resolution not in \(([^)]*)\)", jql)
        names = (
            [n.strip().strip('"') for n in excluded.group(1).split(",")]
            if excluded else []
        )
        resolution = fields["resolution"]
        return (
            resolution is not None and
            resolution["name"] not in names and
            version in [v["name"] for v in fields["fixVersions"]] and
            fields["status"]["name"] != "Open"
        )

    def urlopen(self, req, timeout=None):
        prefix = f"{gc.JIRA_BASE}/rest/api/2/search?"
        url = req.full_url
        if not url.startswith(prefix):
            raise AssertionError(f"unexpected request: {url}")
        query = urllib.parse.parse_qs(url[len(prefix):])
        jql = query["jql"][0]
        wanted = query["fields"][0].split(",")
        start = int(query.get("startAt", ["0"])[0])
        keys = re.fullmatch(r"key in \((.*)\)", jql)
        if keys:
            requested = keys.group(1).split(",")
            unknown = [k for k in requested if k not in self.issues]
            validate = query.get("validateQuery", ["true"])[0].lower() == "true"
            if validate and unknown and len(requested) <= self.VALIDATED_KEY_LIST:
                error = {"errorMessages": [
                    f"An issue with key '{k}' does not exist for field 'key'."
                    for k in unknown
                ], "errors": {}}
                raise urllib.error.HTTPError(
                    url, 400, "Bad Request", None,
                    io.BytesIO(json.dumps(error).encode("utf-8")),
                )
            hits = [k for k in requested if k in self.issues]
        else:
            version = re.search(r'fixVersion = "([^"]+)"', jql).group(1)
            hits = [
                k for k, f in self.issues.items()
                if self.matches_fix_version_query(jql, f, version)
            ]
        body = {
            "total": len(hits),
            "issues": [
                {
                    "key": k,
                    "fields": {
                        name: self.issues[k][name]
                        for name in wanted if name in self.issues[k]
                    },
                }
                for k in hits[start:]
            ],
        }
        return io.BytesIO(json.dumps(body).encode("utf-8"))


def sections_of(draft):
    """Map each ### heading of a rendered draft to its bullet lines."""
    result = {}
    bullets = None
    for line in draft.splitlines():
        if line.startswith("### "):
            bullets = result.setdefault(line[len("### "):], [])
        elif line.startswith("- ") and bullets is not None:
            bullets.append(line)
    return result


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


class ZigSectionTests(unittest.TestCase):
    """All three routes must land Zig work in the same "Zig" section."""

    def test_github_label_maps_to_zig(self):
        self.assertEqual(gc.labels_to_sections(["zig"]), ["Zig"])

    def test_client_trailer_maps_to_zig(self):
        self.assertEqual(
            gc.extract_client_sections(
                "THRIFT-6152: Add Zig binding", "Client: zig"
            ),
            ["Zig"],
        )

    def test_jira_components_map_to_zig(self):
        # Both "Zig - Library" and "Zig - Compiler" reduce to the same heading.
        # jira_component_to_section() falls back to the stripped base name, so
        # this holds with or without the JIRA_COMPONENT_MAP entry; it pins the
        # section string the other two routes have to agree with.
        self.assertEqual(gc.jira_component_to_section("Zig - Library"), "Zig")
        self.assertEqual(gc.jira_component_to_section("Zig - Compiler"), "Zig")


class TicketExtractionTests(unittest.TestCase):
    """Which THRIFT-NNNN mentions in a commit message count as tickets."""

    def test_version_string_is_not_a_ticket(self):
        # Mirrors da6ed655d, whose body quotes a path in the 0.24.0 tarball.
        self.assertEqual(gc.extract_tickets(
            "Add cstddef include to fix build error with 6.3.0",
            "thrift-0.24.0/lib/cpp/src/thrift/transport/TBufferTransports.h:110:32:",
        ), set())

    def test_ticket_at_the_end_of_a_sentence_is_kept(self):
        self.assertEqual(
            gc.extract_tickets("Fix the frame size", "Follows up on THRIFT-1337."),
            {"THRIFT-1337"},
        )

    def test_ticket_prefix_in_any_case_is_kept(self):
        self.assertEqual(
            gc.extract_tickets("Thrift-2600: 0.9.2 release", ""),
            {"THRIFT-2600"},
        )


class CleanSubjectTests(unittest.TestCase):
    """A commit line shows the subject without its ticket and trailers."""

    def test_patch_trailer_before_client_trailer_is_stripped(self):
        # Mirrors 2ae9c11db, listed by commit while THRIFT-6108 is open.
        self.assertEqual(
            gc.clean_subject(
                "THRIFT-6108: Consolidate replace_all() into t_oop_generator"
                " Patch: A. Contributor Client: dart,delphi"
            ),
            "Consolidate replace_all() into t_oop_generator",
        )

    def test_autor_trailer_is_stripped(self):
        self.assertEqual(
            gc.clean_subject("THRIFT-1: Fix the build Autor: A. Contributor"),  # codespell:ignore
            "Fix the build",
        )


TICKET_6183 = jira_fields(
    "Use the library-wide default frame size in TNonblockingServer",
    components=["C++ - Library"], fix_versions=["0.25.0"],
)

# Fixed in 2011 and never given a Fix Version/s.
TICKET_1337 = jira_fields(
    "thrift: support maximum frame size in TNonblockingServer",
    components=["C++ - Library"], status="Closed",
)


class ReleaseTicketFilterTests(unittest.TestCase):
    """A ticket is in the release only if JIRA files it under that version."""

    def in_release(self, version="0.25.0", **fields):
        entry = gc.jira_issue_entry(jira_fields("summary", **fields))
        return gc.jira_ticket_in_release(entry, version)

    def test_resolved_ticket_with_the_release_fix_version_is_in(self):
        self.assertTrue(self.in_release(fix_versions=["0.25.0"]))

    def test_ticket_without_a_fix_version_is_out(self):
        self.assertFalse(self.in_release(status="Closed"))

    def test_ticket_fixed_in_another_version_is_out(self):
        self.assertFalse(self.in_release(fix_versions=["0.24.0"]))

    def test_release_among_several_fix_versions_is_in(self):
        self.assertTrue(self.in_release(fix_versions=["0.24.1", "0.25.0"]))

    def test_open_ticket_is_out_even_with_the_release_fix_version(self):
        self.assertFalse(self.in_release(
            fix_versions=["0.25.0"], status="Open", resolution=None
        ))

    def test_unresolved_ticket_is_out_even_when_not_open(self):
        self.assertFalse(self.in_release(
            fix_versions=["0.25.0"], status="In Progress", resolution=None
        ))

    def test_resolutions_that_record_a_change_are_in(self):
        # Duplicate included: the release manager sometimes gives a duplicate
        # the fix version when the release addressed it.
        for resolution in ["Fixed", "Done", "Implemented", "Duplicate"]:
            with self.subTest(resolution=resolution):
                self.assertTrue(self.in_release(
                    fix_versions=["0.25.0"], status="Closed",
                    resolution=resolution,
                ))

    def test_resolutions_without_a_fix_are_out(self):
        # THRIFT-5917 (Won't Do): not a change to report, whatever its
        # Fix Version/s says.
        for resolution in [
            "Won't Do", "Won't Fix", "Not A Problem", "Not A Bug",
            "Cannot Reproduce", "Works for Me", "Invalid", "Incomplete",
            "Information Provided", "Later", "Abandoned", "Auto Closed",
        ]:
            with self.subTest(resolution=resolution):
                self.assertFalse(self.in_release(
                    fix_versions=["0.25.0"], status="Closed",
                    resolution=resolution,
                ))

    def test_entry_keeps_the_fields_the_filter_needs(self):
        entry = gc.jira_issue_entry(jira_fields(
            "summary",
            components=["C++ - Library", "C++ - Compiler"],
            fix_versions=["0.25.0"],
        ))
        self.assertEqual(entry, {
            "summary": "summary",
            "sections": ["C++"],
            "fix_versions": ["0.25.0"],
            "status": "Resolved",
            "resolution": "Fixed",
        })

    def test_filter_reports_skipped_tickets_in_ticket_order(self):
        data = {
            "THRIFT-6183": gc.jira_issue_entry(TICKET_6183),
            "THRIFT-1337": gc.jira_issue_entry(TICKET_1337),
            "THRIFT-892": gc.jira_issue_entry(
                jira_fields("summary", fix_versions=["0.7"])
            ),
        }
        kept, skipped = gc.filter_release_tickets(data, "0.25.0")
        self.assertEqual(list(kept), ["THRIFT-6183"])
        self.assertEqual(skipped, ["THRIFT-892", "THRIFT-1337"])


@unittest.skipUnless(shutil.which("git"), "git is not installed")
class ReleaseTicketDraftTests(unittest.TestCase):
    """End to end: which referenced tickets the rendered draft lists."""

    CPP_6183 = (
        "- [THRIFT-6183](https://issues.apache.org/jira/browse/THRIFT-6183)"
        " - Use the library-wide default frame size in TNonblockingServer"
    )

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.repo = os.path.join(tmp.name, "repo")
        self.output = os.path.join(tmp.name, "CHANGES-draft.md")
        os.mkdir(self.repo)
        # Keep the user's git configuration away from both the setup below and
        # the script's own git calls.
        env = mock.patch.dict(os.environ, {
            "GIT_CONFIG_GLOBAL": os.devnull,
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_AUTHOR_NAME": "Test",
            "GIT_AUTHOR_EMAIL": "test@example.org",
            "GIT_COMMITTER_NAME": "Test",
            "GIT_COMMITTER_EMAIL": "test@example.org",
        })
        env.start()
        self.addCleanup(env.stop)
        self.git("init", "-q")
        configure_ac = os.path.join(self.repo, "configure.ac")
        with open(configure_ac, "w", encoding="utf-8") as f:
            f.write("AC_INIT([thrift], [0.25.0], [dev@thrift.apache.org])\n")
        self.git("add", "configure.ac")
        self.git("commit", "-q", "-m", "Set the version to 0.25.0")
        self.git("tag", "v0.24.0")

    def git(self, *args):
        subprocess.run(
            ["git"] + list(args), cwd=self.repo, check=True, capture_output=True
        )

    def commit(self, message):
        self.git("commit", "-q", "--allow-empty", "-m", message)

    def generate(self, issues, jira_version=None):
        """Run generate_changes() on the repository; return (draft, stderr)."""
        args = argparse.Namespace(
            branch=None, from_tag=None, version=None, jira_version=jira_version,
            no_commits=False, github_token=None, repo="apache/thrift",
            output=self.output,
        )
        log = io.StringIO()
        fake = FakeJira(issues)
        with mock.patch.object(gc, "find_repo_root", return_value=self.repo), \
                mock.patch.object(gc.urllib.request, "urlopen", fake.urlopen), \
                contextlib.redirect_stderr(log):
            gc.generate_changes(args)
        with open(self.output, encoding="utf-8") as f:
            return f.read(), log.getvalue()

    def commit_6183(self):
        # Mirrors 8b79397f7, whose body names THRIFT-1337 only as history.
        self.commit(
            "THRIFT-6183: Use the library-wide default frame size in"
            " TNonblockingServer\n"
            "\n"
            "TNonblockingServer caps the frame it will accept at its own\n"
            "MAX_FRAME_SIZE, 256 * 1024 * 1024 since THRIFT-1337 landed it in\n"
            "2011.\n"
            "\n"
            "Client: cpp\n"
        )

    def test_ticket_mentioned_in_a_commit_body_is_not_listed(self):
        self.commit_6183()
        draft, log = self.generate(
            {"THRIFT-6183": TICKET_6183, "THRIFT-1337": TICKET_1337}
        )
        self.assertEqual(sections_of(draft), {"C++": [self.CPP_6183]})
        self.assertIn("THRIFT-1337", log)

    def test_jira_version_mode_does_not_add_it_back(self):
        # The fixVersion query finds THRIFT-6183; THRIFT-1337 only comes in
        # through the extra lookup of tickets the commits reference.
        self.commit_6183()
        draft, log = self.generate(
            {"THRIFT-6183": TICKET_6183, "THRIFT-1337": TICKET_1337},
            jira_version="0.25.0",
        )
        self.assertEqual(sections_of(draft), {"C++": [self.CPP_6183]})
        self.assertIn("THRIFT-1337", log)

    def test_subject_ticket_outside_the_release_is_listed_by_commit(self):
        # Mirrors f62e1b4bf: THRIFT-1941 was closed without a Fix Version/s,
        # so the commit is listed by its PR and subject instead.
        self.commit(
            "THRIFT-1941: Add PHP serializer regression coverage (#3794)\n"
            "\n"
            "Client: php\n"
        )
        draft, _ = self.generate({
            "THRIFT-1941": jira_fields(
                "PHP Serializer deserialize doesn't work",
                components=["PHP - Library"], status="Closed",
            ),
        })
        self.assertEqual(sections_of(draft), {"PHP": [
            "- [#3794](https://github.com/apache/thrift/pull/3794)"
            " - Add PHP serializer regression coverage"
        ]})

    def test_ticket_resolved_without_a_fix_is_listed_by_commit(self):
        # Like THRIFT-5917, but carrying the fix version, so that the
        # fixVersion query itself has to leave it out.
        self.commit(
            "THRIFT-5917: Remove Rust deprecation warning (#3637)\n"
            "\n"
            "Client: rs\n"
        )
        issues = {
            "THRIFT-5917": jira_fields(
                "Drop Rust support?", components=["Rust - Library"],
                fix_versions=["0.25.0"], status="Closed", resolution="Won't Do",
            ),
        }
        expected = {"Rust": [
            "- [#3637](https://github.com/apache/thrift/pull/3637)"
            " - Remove Rust deprecation warning"
        ]}
        for jira_version in [None, "0.25.0"]:
            with self.subTest(jira_version=jira_version):
                draft, _ = self.generate(issues, jira_version=jira_version)
                self.assertEqual(sections_of(draft), expected)

    def test_version_string_in_a_commit_body_costs_no_ticket(self):
        # Mirrors da6ed655d. "thrift-0.24.0/..." was read as THRIFT-0, and
        # JIRA then rejected the whole lookup, THRIFT-6183 included.
        self.commit_6183()
        self.commit(
            "Add cstddef include to fix build error with 6.3.0 (#3801)\n"
            "\n"
            "thrift-0.24.0/lib/cpp/src/thrift/transport/TBufferTransports.h:110:32:\n"
            " error: 'ptrdiff_t' does not name a type\n"
            "\n"
            "Client: cpp\n"
        )
        draft, log = self.generate({"THRIFT-6183": TICKET_6183})
        self.assertEqual(sections_of(draft), {"C++": [
            self.CPP_6183,
            "- [#3801](https://github.com/apache/thrift/pull/3801)"
            " - Add cstddef include to fix build error with 6.3.0",
        ]})
        self.assertNotRegex(log, r"\bTHRIFT-0\b")

    def test_unknown_ticket_key_costs_no_other_ticket(self):
        # A key that does not exist, such as a typo, must not hide the
        # tickets that other commits reference.
        self.commit_6183()
        self.commit(
            "Clarify the frame size documentation (#3900)\n"
            "\n"
            "Follows up on THRIFT-99999.\n"
            "\n"
            "Client: cpp\n"
        )
        draft, log = self.generate({"THRIFT-6183": TICKET_6183})
        self.assertEqual(sections_of(draft), {"C++": [
            self.CPP_6183,
            "- [#3900](https://github.com/apache/thrift/pull/3900)"
            " - Clarify the frame size documentation",
        ]})
        self.assertIn("THRIFT-99999", log)


if __name__ == "__main__":
    unittest.main()
