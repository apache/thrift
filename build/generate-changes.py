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

"""
generate-changes.py - Generate CHANGES.md content for an Apache Thrift release.

Three complementary data sources are combined:

  1. JIRA (primary, when --jira-version is given)
     Queries all tickets where fixVersion = VERSION and status is resolved.
     This is the authoritative list used in actual releases.

  2. Git commits (always)
     Walks commits between the last v* tag (or --from) and the branch tip.
     Extracts THRIFT-NNNN references from commit messages and fetches their
     JIRA summaries.  Commits with no ticket reference are included as
     GitHub commit links, grouped by their "Client:" trailer.

  3. GitHub PR labels (fallback for commits without a "Client:" trailer)
     When a commit was merged via a PR (subject ends with "(#NNN)") and has
     no "Client:" trailer, the script fetches the PR's GitHub labels and maps
     language-specific labels to CHANGES.md sections.  Requires network access
     to the GitHub API; use --github-token to raise the rate limit.
     With --github-token the script also resolves PR numbers for commits whose
     subject lacks the "(#NNN)" suffix, so all commit links point to their PR.

When --jira-version is NOT given the script is git-only (useful while a
release is still in progress and fixVersions haven't been assigned in JIRA).

Usage:
  generate-changes.py [options]

Options:
  --branch BRANCH           Branch to analyze (default: current branch or master)
  --from TAG                Starting tag or commit ref (default: auto-detect latest v* tag)
  --version VERSION         Release version for the ## header (default: from configure.ac)
  --jira-version VERSION    Also query JIRA for all tickets with this fixVersion;
                            overrides git-extracted tickets as the primary source
  --no-commits              Exclude ticket-less commits from output (default: include them)
  --github-token TOKEN      GitHub personal access token (default: unauthenticated,
                            60 req/hr; with token: 5000 req/hr)
  --repo OWNER/REPO         GitHub repository for PR label lookups
                            (default: apache/thrift)
  --output FILE             Write output to FILE instead of stdout
  -h / --help               Show this message and exit

Examples:
  generate-changes.py
  generate-changes.py --branch release/1.0.0
  generate-changes.py --jira-version 0.24.0 --version 0.24.0
  generate-changes.py --from v0.22.0 --jira-version 0.23.0 --version 0.23.0
  generate-changes.py --no-commits --output /tmp/draft-changes.md
  generate-changes.py --github-token ghp_... --output /tmp/draft-changes.md
"""

import argparse
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request
from collections import defaultdict
from urllib.parse import urlencode

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

JIRA_BASE = "https://issues.apache.org/jira"
GITHUB_BASE = "https://github.com/apache/thrift"
GITHUB_API_BASE = "https://api.github.com"

# Maps JIRA component base-name (after stripping " - Library" / " - Compiler"
# suffix) to the canonical CHANGES.md section heading.
JIRA_COMPONENT_MAP = {
    "AS3": "AS3",
    "Build Process": "Build Process",
    "C glib": "C glib",
    "C#": "netstd",          # legacy JIRA name for .NET Standard
    "C++": "C++",
    "Cocoa": "Cocoa",
    "Common LISP": "Common LISP",
    "Compiler (General)": "Compiler (General)",
    "Contributed": "Contributed",
    "D": "D",
    "Dart": "Dart",
    "Delphi": "Delphi",
    "Deployment": "Deployment",
    "Documentation": "Documentation",
    "Erlang": "Erlang",
    "Go": "Go",
    "Graphviz": "Graphviz",
    "HTML": "HTML",
    "Haskell": "Haskell",
    "Haxe": "Haxe",
    "JSON": "JSON",
    "Java": "Java",
    "JavaME": "JavaME",
    "JavaScript": "JavaScript",
    "Kotlin": "Kotlin",
    "Lua": "Lua",
    "Markdown": "Markdown",
    "Mermaid": "Mermaid",
    "Node.js": "nodejs",
    "OCaml": "OCaml",
    "PHP": "PHP",
    "Perl": "Perl",
    "Python": "Python",
    "Ruby": "Ruby",
    "Rust": "Rust",
    "TypeScript": "nodets",
}

# Maps the value of the "Client:" commit trailer to the canonical section name.
CLIENT_SECTION_MAP = {
    "all": "(All Languages)",
    "build": "Build Process",
    "c#": "netstd",
    "c++": "C++",
    "c_glib": "C glib",
    "compiler": "Compiler (General)",
    "compiler (general)": "Compiler (General)",
    "cpp": "C++",
    "csharp": "netstd",
    "d": "D",
    "dart": "Dart",
    "delphi": "Delphi",
    "docker": "Build Process",
    "erl": "Erlang",
    "erlang": "Erlang",
    "go": "Go",
    "gv": "Graphviz",
    "haskell": "Haskell",
    "haxe": "Haxe",
    "hs": "Haskell",
    "hx": "Haxe",
    "md": "Markdown",
    "mmd": "Mermaid",
    "java": "Java",
    "js": "JavaScript",
    "lua": "Lua",
    "netstd": "netstd",
    "nodejs": "nodejs",
    "perl": "Perl",
    "php": "PHP",
    "py": "Python",
    "python": "Python",
    "rb": "Ruby",
    "rs": "Rust",
    "ruby": "Ruby",
    "rust": "Rust",
    "ts": "nodets",
}

# Maps GitHub label names (lowercase) to the canonical CHANGES.md section heading.
# Labels without a mapping are silently ignored.
GITHUB_LABEL_MAP = {
    "build and general ci": "Build Process",
    "c_glib": "C glib",
    "c#": "netstd",
    "c++": "C++",
    "compiler": "Compiler (General)",
    "d": "D",
    "dart": "Dart",
    "delphi": "Delphi",
    "erlang": "Erlang",
    "golang": "Go",
    "haxe": "Haxe",
    "java": "Java",
    "javascript": "JavaScript",
    "json": "JSON",
    "kotlin": "Kotlin",
    "lua": "Lua",
    "mermaid": "Mermaid",
    "nodejs": "nodejs",
    "perl": "Perl",
    "php": "PHP",
    "python": "Python",
    "ruby": "Ruby",
    "rust": "Rust",
    "typescript": "nodets",
    "releng": "Build Process",
}

# Sections that should sort last regardless of alphabetical order.
# Add any section names you want pinned to the bottom here.
LATE_SECTIONS = {"(All Languages)", "(No Section)"}

TICKET_RE = re.compile(r'\bTHRIFT-(\d+)\b', re.IGNORECASE)
CLIENT_TRAILER_RE = re.compile(r'\bClient:\s*(.+)', re.IGNORECASE)
PR_RE = re.compile(r'\(#(\d+)\)\s*$')


# ---------------------------------------------------------------------------
# Git helpers
# ---------------------------------------------------------------------------

def run_git(*args, cwd=None):
    result = subprocess.run(
        ["git"] + list(args),
        capture_output=True, text=True, cwd=cwd
    )
    if result.returncode != 0:
        raise RuntimeError(f"git {' '.join(args)}: {result.stderr.strip()}")
    return result.stdout.strip()


def find_repo_root():
    try:
        return run_git("rev-parse", "--show-toplevel")
    except RuntimeError:
        return None


def current_branch(repo_root):
    try:
        name = run_git("rev-parse", "--abbrev-ref", "HEAD", cwd=repo_root)
        return "master" if name == "HEAD" else name
    except RuntimeError:
        return "master"


def latest_release_tag(branch, repo_root):
    """Return the most recent v* tag reachable from branch, or None."""
    try:
        return run_git(
            "describe", "--tags", "--abbrev=0", "--match", "v*", branch,
            cwd=repo_root
        )
    except RuntimeError:
        return None


def get_commits(since, until, repo_root):
    """Return list of (sha, subject, body) tuples, no-merges, newest first."""
    # NUL-delimited fields, RS=\x01 between records
    raw = run_git(
        "log", "--no-merges",
        "--format=%H%x00%s%x00%b%x01",
        f"{since}..{until}",
        cwd=repo_root
    )
    commits = []
    for record in raw.split("\x01"):
        record = record.strip()
        if not record:
            continue
        parts = record.split("\x00", 2)
        while len(parts) < 3:
            parts.append("")
        commits.append((parts[0].strip(), parts[1].strip(), parts[2].strip()))
    return commits


# ---------------------------------------------------------------------------
# Parsing helpers
# ---------------------------------------------------------------------------

def extract_tickets(subject, body):
    """Return set of 'THRIFT-NNNN' (uppercase) strings from subject + body."""
    text = f"{subject}\n{body}"
    return {f"THRIFT-{m.group(1)}" for m in TICKET_RE.finditer(text)}


def extract_client_sections(subject, body):
    """Return list of canonical section names from the Client: trailer."""
    text = f"{subject}\n{body}"
    m = CLIENT_TRAILER_RE.search(text)
    if not m:
        return []
    raw = m.group(1)
    # Strip optional "Patch: Name" / "Autor: Name" suffixes
    raw = re.split(r'\s+(?:Patch|Autor):', raw, maxsplit=1)[0]
    langs = [t.strip().lower() for t in raw.split(",")]
    return [CLIENT_SECTION_MAP[l] for l in langs if l in CLIENT_SECTION_MAP]


def extract_pr_number(subject):
    """Return the GitHub PR number from '(#NNN)' at the end of subject, or None."""
    m = PR_RE.search(subject)
    return int(m.group(1)) if m else None


def clean_subject(subject):
    """Remove standard prefixes/suffixes from a commit subject line."""
    subject = re.sub(r'^THRIFT-\d+:\s*', '', subject, flags=re.IGNORECASE)
    subject = re.sub(r'^No\s+ticket:\s*', '', subject, flags=re.IGNORECASE)
    # Remove trailing "Client: ..." trailer that appears on the subject line
    subject = re.sub(r'\s+Client:\s*\S.*$', '', subject, flags=re.IGNORECASE)
    # Strip trailing PR reference " (#NNN)"
    subject = re.sub(r'\s+\(#\d+\)\s*$', '', subject)
    return subject.strip()


# ---------------------------------------------------------------------------
# JIRA helpers
# ---------------------------------------------------------------------------

def jira_base_component(comp_name):
    """Strip ' - Library' / ' - Compiler' suffix from a JIRA component name."""
    return re.sub(r'\s+-\s+(?:Library|Compiler)$', '', comp_name).strip()


def jira_component_to_section(comp_name):
    """Map a raw JIRA component name to a CHANGES.md section heading."""
    base = jira_base_component(comp_name)
    return JIRA_COMPONENT_MAP.get(base, base)


def fetch_jira_issues(ticket_ids):
    """Query JIRA for summary + components.

    Returns dict mapping ticket_id (uppercase) to
      {"summary": str, "sections": [str]}
    Unknown / unreachable tickets are absent from the result.
    """
    if not ticket_ids:
        return {}

    result = {}
    ticket_list = sorted(ticket_ids)

    for i in range(0, len(ticket_list), 50):
        batch = ticket_list[i : i + 50]
        keys = ",".join(batch)
        params = urlencode({
            "jql": f"key in ({keys})",
            "fields": "summary,components",
            "maxResults": 50,
        })
        url = f"{JIRA_BASE}/rest/api/2/search?{params}"
        try:
            req = urllib.request.Request(url, headers={"Accept": "application/json"})
            with urllib.request.urlopen(req, timeout=30) as resp:
                data = json.loads(resp.read())
            for issue in data.get("issues", []):
                key = issue["key"].upper()
                summary = issue["fields"]["summary"]
                raw_sections = [
                    jira_component_to_section(c["name"])
                    for c in issue["fields"].get("components", [])
                ]
                # Deduplicate, preserving order
                seen: set = set()
                sections = []
                for s in raw_sections:
                    if s not in seen:
                        seen.add(s)
                        sections.append(s)
                result[key] = {
                    "summary": summary,
                    "sections": sections if sections else ["(No Section)"],
                }
        except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError) as exc:
            print(f"Warning: JIRA query failed: {exc}", file=sys.stderr)

        if i + 50 < len(ticket_list):
            time.sleep(0.3)

    return result


def fetch_jira_by_fixversion(fix_version):
    """Return the same dict format as fetch_jira_issues, for all tickets that
    have fixVersion = fix_version and are resolved/closed.

    This is the authoritative JIRA query described in ReleaseManagement.md:
      project = THRIFT AND resolution = Fixed
        AND fixVersion = X.Y.Z AND status != Open
    """
    result = {}
    start_at = 0
    page_size = 100

    # Include any resolved/closed ticket regardless of resolution sub-type.
    # The release manager occasionally assigns fixVersion to Duplicate or
    # similar tickets when they were addressed as part of the release.
    jql = (
        f'project = THRIFT AND resolution != Unresolved '
        f'AND fixVersion = "{fix_version}" AND status != Open'
    )

    while True:
        params = urlencode({
            "jql": jql,
            "fields": "summary,components",
            "maxResults": page_size,
            "startAt": start_at,
        })
        url = f"{JIRA_BASE}/rest/api/2/search?{params}"
        try:
            req = urllib.request.Request(url, headers={"Accept": "application/json"})
            with urllib.request.urlopen(req, timeout=30) as resp:
                data = json.loads(resp.read())
        except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError) as exc:
            print(f"Warning: JIRA fixVersion query failed: {exc}", file=sys.stderr)
            break

        for issue in data.get("issues", []):
            key = issue["key"].upper()
            summary = issue["fields"]["summary"]
            raw_sections = [
                jira_component_to_section(c["name"])
                for c in issue["fields"].get("components", [])
            ]
            seen: set = set()
            sections = []
            for s in raw_sections:
                if s not in seen:
                    seen.add(s)
                    sections.append(s)
            result[key] = {
                "summary": summary,
                "sections": sections if sections else ["(No Section)"],
            }

        total = data.get("total", 0)
        start_at += page_size
        if start_at >= total:
            break
        time.sleep(0.3)

    return result


# ---------------------------------------------------------------------------
# GitHub helpers
# ---------------------------------------------------------------------------

def fetch_pr_labels(pr_numbers, repo, github_token=None):
    """Return a dict mapping PR number (int) → list of canonical section names.

    Only PRs whose labels appear in GITHUB_LABEL_MAP are included; labels
    with no known mapping are ignored.  API failures produce a warning and
    are treated as "no labels found" for that PR.
    """
    if not pr_numbers:
        return {}

    headers = {
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
        "User-Agent": "apache-thrift-gen-changes",
    }
    if github_token:
        headers["Authorization"] = f"Bearer {github_token}"

    # Authenticated: 5000 req/hr → 0.72 s/req; unauthenticated: 60 req/hr → 60 s/req.
    delay = 0.72 if github_token else 60.0

    result = {}
    for i, pr_num in enumerate(pr_numbers):
        url = f"{GITHUB_API_BASE}/repos/{repo}/issues/{pr_num}"
        try:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=30) as resp:
                data = json.loads(resp.read())
            sections: list = []
            seen: set = set()
            for label in data.get("labels", []):
                section = GITHUB_LABEL_MAP.get(label["name"].lower())
                if section and section not in seen:
                    seen.add(section)
                    sections.append(section)
            result[pr_num] = sections
        except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError) as exc:
            print(f"Warning: GitHub PR #{pr_num} label fetch failed: {exc}", file=sys.stderr)

        if i < len(pr_numbers) - 1:
            time.sleep(delay)

    return result


def fetch_commit_prs(shas, repo, github_token):
    """Return a dict mapping sha (full) → PR number (int).

    Calls GET /repos/{owner}/{repo}/commits/{sha}/pulls for each SHA and
    returns the number of the first (most-recently-merged) PR found.
    Commits with no associated PR or API failures are absent from the result.

    Requires a GitHub token: the unauthenticated rate limit (60 req/hr) is
    too low for bulk lookups.
    """
    if not shas:
        return {}

    headers = {
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
        "User-Agent": "apache-thrift-gen-changes",
        "Authorization": f"Bearer {github_token}",
    }

    result = {}
    for i, sha in enumerate(shas):
        url = f"{GITHUB_API_BASE}/repos/{repo}/commits/{sha}/pulls"
        try:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=30) as resp:
                data = json.loads(resp.read())
            if data:
                result[sha] = data[0]["number"]
        except (urllib.error.URLError, urllib.error.HTTPError, json.JSONDecodeError):
            pass

        if i < len(shas) - 1:
            time.sleep(0.1)

    return result


# ---------------------------------------------------------------------------
# Version detection
# ---------------------------------------------------------------------------

def version_from_configure(repo_root):
    path = os.path.join(repo_root, "configure.ac")
    if not os.path.exists(path):
        return None
    with open(path, encoding="utf-8") as f:
        for line in f:
            m = re.search(r'AC_INIT\(\s*\[thrift\]\s*,\s*\[([^\]]+)\]', line)
            if m:
                return m.group(1)
    return None


# ---------------------------------------------------------------------------
# Section sort key
# ---------------------------------------------------------------------------

def section_sort_key(name):
    """Alphabetical, but LATE_SECTIONS sort last."""
    return (1 if name in LATE_SECTIONS else 0, name.lower())


# ---------------------------------------------------------------------------
# Main logic
# ---------------------------------------------------------------------------

def generate_changes(args):
    repo_root = find_repo_root()
    if repo_root is None:
        print(
            "Error: not inside a git repository. "
            "Run this script from within the Thrift source tree.",
            file=sys.stderr,
        )
        sys.exit(1)

    # --- Branch ---
    branch = args.branch or current_branch(repo_root)
    print(f"Branch  : {branch}", file=sys.stderr)

    # --- Start of range ---
    if args.from_tag:
        since = args.from_tag
    else:
        since = latest_release_tag(branch, repo_root)
        if since is None:
            print(
                "Error: no v* tag found on branch. "
                "Use --from to specify the starting point.",
                file=sys.stderr,
            )
            sys.exit(1)
    print(f"Since   : {since}", file=sys.stderr)
    print(f"Until   : {branch}", file=sys.stderr)

    # --- Version ---
    version = args.version or version_from_configure(repo_root) or "X.Y.Z"
    print(f"Version : {version}", file=sys.stderr)

    # --- Commits ---
    commits = get_commits(since, branch, repo_root)
    print(f"Commits : {len(commits)}", file=sys.stderr)

    # --- Extract tickets and per-commit metadata ---
    all_tickets: set = set()
    commit_meta = []
    for sha, subject, body in commits:
        tickets = extract_tickets(subject, body)
        all_tickets |= tickets
        sections = extract_client_sections(subject, body)
        commit_meta.append({
            "sha": sha,
            "short": sha[:9],
            "subject": subject,
            "tickets": tickets,
            "sections": sections,
            "pr_num": extract_pr_number(subject),
        })

    # --- Query JIRA ---
    jira_data: dict = {}

    if args.jira_version:
        print(
            f"Querying JIRA fixVersion={args.jira_version} ...",
            file=sys.stderr,
        )
        jira_data = fetch_jira_by_fixversion(args.jira_version)
        print(f"JIRA fixVersion hits: {len(jira_data)}", file=sys.stderr)
        # Also fetch any tickets found in git commits that aren't covered yet
        extra = {t for t in all_tickets if t.upper() not in jira_data}
        if extra:
            print(
                f"Fetching {len(extra)} additional git-referenced tickets from JIRA ...",
                file=sys.stderr,
            )
            jira_data.update(fetch_jira_issues(extra))
    else:
        if all_tickets:
            print(f"Querying JIRA for {len(all_tickets)} git-referenced tickets ...", file=sys.stderr)
        jira_data = fetch_jira_issues(all_tickets)

    print(f"Total JIRA entries: {len(jira_data)}", file=sys.stderr)

    # --- Resolve PR numbers for commits without a "(#NNN)" subject reference ---
    # Only done with a GitHub token; the unauthenticated rate limit (60 req/hr)
    # is too low for bulk commit→PR lookups.
    if not args.no_commits and args.github_token:
        shas_without_pr = [
            c["sha"]
            for c in commit_meta
            if c["pr_num"] is None
            and not any(t.upper() in jira_data for t in c["tickets"])
        ]
        if shas_without_pr:
            print(
                f"Resolving GitHub PRs for {len(shas_without_pr)} commits (authenticated) ...",
                file=sys.stderr,
            )
            sha_to_pr = fetch_commit_prs(shas_without_pr, args.repo, args.github_token)
            commit_by_sha = {c["sha"]: c for c in commit_meta}
            for sha, pr_num in sha_to_pr.items():
                commit_by_sha[sha]["pr_num"] = pr_num

    # --- Fetch GitHub PR labels for commits that lack a Client: trailer ---
    # Only done when commits are included in output; skips commits already
    # covered by JIRA or that already have sections from the trailer.
    if not args.no_commits:
        pr_nums_to_fetch = [
            c["pr_num"]
            for c in commit_meta
            if c["pr_num"] is not None
            and not c["sections"]
            and not any(t.upper() in jira_data for t in c["tickets"])
        ]
        if pr_nums_to_fetch:
            print(
                f"Fetching GitHub labels for {len(pr_nums_to_fetch)} PRs "
                f"({'authenticated' if args.github_token else 'unauthenticated'}) ...",
                file=sys.stderr,
            )
            pr_label_data = fetch_pr_labels(
                pr_nums_to_fetch, args.repo, args.github_token
            )
            # Propagate fetched sections back into commit_meta in-place so the
            # section-building loop below picks them up transparently.
            pr_meta_by_num = {
                c["pr_num"]: c
                for c in commit_meta
                if c["pr_num"] is not None
            }
            for pr_num, sections in pr_label_data.items():
                if sections and pr_num in pr_meta_by_num:
                    pr_meta_by_num[pr_num]["sections"] = sections

    # --- Build per-section entry lists ---
    # sections_jira[section]   = list of (ticket_num, line)
    # sections_commit[section] = list of line  (in git-log = newest-first order)
    sections_jira: dict = defaultdict(list)
    sections_commit: dict = defaultdict(list)

    # Track what we've already emitted to avoid duplicates
    emitted_ticket: dict = defaultdict(set)   # section -> {ticket_id}
    emitted_commit: dict = defaultdict(set)   # section -> {sha}

    # JIRA-backed entries
    for ticket_id, info in jira_data.items():
        ticket_num = int(re.search(r"\d+", ticket_id).group())
        ticket_url = f"{JIRA_BASE}/browse/{ticket_id}"
        line = f"- [{ticket_id}]({ticket_url}) - {info['summary']}"
        for section in info["sections"]:
            if ticket_id not in emitted_ticket[section]:
                emitted_ticket[section].add(ticket_id)
                sections_jira[section].append((ticket_num, line))

    # Ticket-less commit entries (or commits whose ticket wasn't in JIRA)
    if not args.no_commits:
        for c in commit_meta:
            # Skip this commit if at least one of its tickets was found in JIRA
            if any(t.upper() in jira_data for t in c["tickets"]):
                continue
            # Skip commits that have neither a ticket reference nor a PR number;
            # a bare SHA link adds no useful context to the changelog.
            if not c["tickets"] and c["pr_num"] is None:
                continue
            sha = c["sha"]
            subject_clean = clean_subject(c["subject"])
            if c["pr_num"] is not None:
                url = f"{GITHUB_BASE}/pull/{c['pr_num']}"
                ref = f"#{c['pr_num']}"
            else:
                url = f"{GITHUB_BASE}/commit/{sha}"
                ref = c["short"]
            line = f"- [{ref}]({url}) - {subject_clean}"
            target = c["sections"] if c["sections"] else ["(No Section)"]
            for section in target:
                if sha not in emitted_commit[section]:
                    emitted_commit[section].add(sha)
                    sections_commit[section].append(line)

    # --- Render ---
    all_section_names = sorted(
        set(sections_jira) | set(sections_commit),
        key=section_sort_key,
    )

    out_lines = [f"## {version}", ""]
    for section in all_section_names:
        out_lines.append(f"### {section}")
        out_lines.append("")
        # JIRA entries, ascending by ticket number
        for _, line in sorted(sections_jira.get(section, []), key=lambda x: x[0]):
            out_lines.append(line)
        # Commit entries in git-log order (newest first)
        for line in sections_commit.get(section, []):
            out_lines.append(line)
        out_lines.append("")

    output = "\n".join(out_lines) + "\n"

    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            f.write(output)
        print(f"Written to {args.output}", file=sys.stderr)
    else:
        sys.stdout.write(output)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        prog="generate-changes.py",
        description=(
            "Generate a CHANGES.md draft for an Apache Thrift release.\n\n"
            "The script walks git commits between the last v* tag and the tip\n"
            "of the selected branch, queries JIRA for summaries and components,\n"
            "and emits markdown grouped by component section."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "Examples:\n"
            "  %(prog)s\n"
            "  %(prog)s --branch release/1.0.0\n"
            "  %(prog)s --jira-version 0.24.0 --version 0.24.0\n"
            "  %(prog)s --from v0.22.0 --jira-version 0.23.0 --version 0.23.0\n"
            "  %(prog)s --no-commits --output /tmp/draft-changes.md\n"
        ),
    )
    parser.add_argument(
        "--branch", metavar="BRANCH",
        help="git branch to analyze (default: current branch or master)",
    )
    parser.add_argument(
        "--from", dest="from_tag", metavar="TAG",
        help="starting tag or commit ref (default: auto-detect latest v* tag)",
    )
    parser.add_argument(
        "--version", metavar="VERSION",
        help="release version for the ## header (default: read from configure.ac)",
    )
    parser.add_argument(
        "--jira-version", dest="jira_version", metavar="VERSION",
        help=(
            "query JIRA for all tickets with this fixVersion as the primary "
            "source (recommended for release prep once fixVersions are assigned); "
            "git-extracted tickets are merged in as a supplement"
        ),
    )
    parser.add_argument(
        "--no-commits", action="store_true",
        help=(
            "exclude all commit-derived entries from output, including commits "
            "that reference a THRIFT ticket not returned by the JIRA query "
            "(e.g. wrong fixVersion or unresolved status); only JIRA-sourced "
            "entries are emitted (default: include commit entries)"
        ),
    )
    parser.add_argument(
        "--github-token", dest="github_token", metavar="TOKEN",
        help=(
            "GitHub personal access token; enables bulk commit→PR resolution "
            "and raises the API rate limit from 60 to 5000 req/hr"
        ),
    )
    parser.add_argument(
        "--repo", metavar="OWNER/REPO", default="apache/thrift",
        help="GitHub repository for PR label lookups (default: apache/thrift)",
    )
    parser.add_argument(
        "--output", metavar="FILE",
        help="write output to FILE instead of stdout",
    )
    args = parser.parse_args()
    generate_changes(args)


if __name__ == "__main__":
    main()
