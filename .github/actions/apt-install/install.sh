#!/usr/bin/env bash
#
# apt-get update and install with a time budget per attempt and a mirror
# fallback. Driven by action.yml; reads its parameters from the environment:
#
#   PACKAGES         packages to install, separated by spaces
#   ATTEMPTS         how many times to try before giving up
#   UPDATE_TIMEOUT   seconds one apt-get update may take
#   INSTALL_TIMEOUT  seconds one apt-get install may take
#
# Why a budget: .github/apt/99-ci-network already bounds a mirror that stops
# answering, but a mirror that keeps trickling bytes never trips apt's
# inactivity timeout, and apt-get then runs until the job's timeout-minutes
# cancels it, with nothing in the log to say what it was waiting for. The
# `timeout` here ends such an attempt itself, and the retry gets a fair chance
# at a different mirror.
#
# Why a fallback mirror: the runner images resolve the Ubuntu archive through
# a mirror list (mirror+file:/etc/apt/apt-mirrors.txt, Azure first). From the
# second attempt on, the sources point straight at archive.ubuntu.com, so a
# bad day on the Azure mirror costs one attempt, not the job.

set -u

read -r -a packages <<< "$PACKAGES"
archive=http://archive.ubuntu.com/ubuntu/

# One attempt: update, then install. Both run under `timeout`, which sends
# SIGTERM at the deadline and SIGKILL ten seconds later.
run_apt() {
  sudo env DEBIAN_FRONTEND=noninteractive \
    timeout --kill-after=10 "$UPDATE_TIMEOUT" \
    apt-get update -yq || return
  sudo env DEBIAN_FRONTEND=noninteractive \
    timeout --kill-after=10 "$INSTALL_TIMEOUT" \
    apt-get install -y --no-install-recommends "${packages[@]}"
}

# Rewrite every apt source that goes through the mirror list so that it goes
# to the canonical archive instead. Covers the one-line sources.list of the
# ubuntu-22.04 image and the deb822 ubuntu.sources of ubuntu-24.04.
use_canonical_archive() {
  local file changed=0
  for file in /etc/apt/sources.list /etc/apt/sources.list.d/*.sources /etc/apt/sources.list.d/*.list; do
    [ -f "$file" ] || continue
    if grep -q 'mirror+file:' "$file"; then
      sudo sed -i "s|mirror+file:[^ ]*|$archive|g" "$file"
      changed=1
    fi
  done
  if [ "$changed" = 1 ]; then
    echo "::notice::apt sources switched to $archive"
  fi
}

for ((attempt = 1; attempt <= ATTEMPTS; attempt++)); do
  if [ "$attempt" -gt 1 ]; then
    sleep 15
    use_canonical_archive
    # An install that `timeout` ended may have left dpkg half-way through
    # configuring a package; apt refuses to run until that is finished.
    sudo env DEBIAN_FRONTEND=noninteractive \
      timeout --kill-after=10 "$INSTALL_TIMEOUT" \
      dpkg --configure -a || true
  fi

  echo "::group::apt-get attempt $attempt of $ATTEMPTS"
  run_apt
  status=$?
  echo "::endgroup::"

  if [ "$status" = 0 ]; then
    exit 0
  fi
  if [ "$status" = 124 ] || [ "$status" = 137 ]; then
    echo "::warning::apt-get attempt $attempt of $ATTEMPTS ran out of time (update ${UPDATE_TIMEOUT}s, install ${INSTALL_TIMEOUT}s)"
  else
    echo "::warning::apt-get attempt $attempt of $ATTEMPTS failed with status $status"
  fi
done

echo "::error::apt-get failed after $ATTEMPTS attempts"
exit 1
