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

"""Validate WinGet manifests against the published JSON schemas.

A manifest that does not match its schema is rejected by microsoft/winget-pkgs
after the pull request has been opened, which is an awkward place to find out.
This checks the rendered manifests before they are submitted.

Each manifest names its own type and schema version, and the matching schema is
fetched from Microsoft, so this follows the manifests rather than pinning a
schema version here.

Usage:
    python3 validate_manifests.py <directory>

Requires PyYAML and jsonschema.
"""

import argparse
import datetime
import json
import pathlib
import sys
import urllib.request

import yaml
from jsonschema import Draft7Validator

SCHEMA_URL = "https://aka.ms/winget-manifest.{type}.{version}.schema.json"

# Every manifest a multi file submission needs, and nothing else: a directory
# missing one of them is not submittable.
REQUIRED_TYPES = {"version", "installer", "defaultLocale"}


def load_schema(manifest_type, manifest_version, cache):
    """Fetch and cache the schema for one manifest type and version."""
    key = (manifest_type, manifest_version)
    if key not in cache:
        url = SCHEMA_URL.format(type=manifest_type, version=manifest_version)
        with urllib.request.urlopen(url, timeout=60) as response:
            cache[key] = json.loads(response.read().decode("utf-8"))
    return cache[key]


def jsonify(value):
    """Turn YAML scalars into what JSON Schema expects.

    YAML resolves an unquoted 2026-09-20 to a date, and the schema wants the
    string it was written as.
    """
    if isinstance(value, dict):
        return {k: jsonify(v) for k, v in value.items()}
    if isinstance(value, list):
        return [jsonify(v) for v in value]
    if isinstance(value, (datetime.datetime, datetime.date)):
        return value.isoformat()
    return value


def validate(path, cache):
    """Validate one manifest. Returns a list of human readable problems."""
    document = jsonify(yaml.safe_load(path.read_text(encoding="utf-8")))
    if not isinstance(document, dict):
        return [f"{path.name}: not a YAML mapping"]

    manifest_type = document.get("ManifestType")
    manifest_version = document.get("ManifestVersion")
    if not manifest_type or not manifest_version:
        return [f"{path.name}: ManifestType or ManifestVersion is missing"]

    schema = load_schema(manifest_type, manifest_version, cache)
    problems = []
    for error in sorted(Draft7Validator(schema).iter_errors(document), key=str):
        location = "/".join(str(part) for part in error.absolute_path) or "(document)"
        problems.append(f"{path.name}: {location}: {error.message}")
    return problems


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", help="directory holding the rendered manifests")
    args = parser.parse_args(argv)

    directory = pathlib.Path(args.directory)
    manifests = sorted(directory.glob("*.yaml"))
    if not manifests:
        print(f"error: no .yaml manifests in {directory}", file=sys.stderr)
        return 1

    cache = {}
    problems = []
    seen_types = set()

    for manifest in manifests:
        found = validate(manifest, cache)
        if found:
            problems.extend(found)
        else:
            print(f"ok   {manifest.name}")
        document = yaml.safe_load(manifest.read_text(encoding="utf-8"))
        if isinstance(document, dict):
            seen_types.add(document.get("ManifestType"))

    missing = REQUIRED_TYPES - seen_types
    if missing:
        problems.append("missing manifest types: " + ", ".join(sorted(missing)))

    if problems:
        print(f"\n{len(problems)} problem(s):", file=sys.stderr)
        for problem in problems:
            print(f"  {problem}", file=sys.stderr)
        return 1

    print(f"\n{len(manifests)} manifests validated.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
