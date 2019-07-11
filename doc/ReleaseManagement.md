# Apache Thrift Release Management

Instructions for preparing and distributing a release of Apache Thrift are fairly complex.  These procedures are documented here, and we're working to automate as much of this as possible.  There are few projects like ours that integrate with 28 programming languages.  Given the extreme number of package management systems that Apache Thrift integrates with (compared to perhaps any), part of the burden of releasing Apache Thrift is to manually package and upload some of these [language-specific packages](http://apache.thrift.org/libraries).

It is important to note here that Apache Thrift is designed for version interoperability, so one can use a version 0.7.0 client with a 0.12.0 server.  A particular version number does not make any guarantees as to the features available in any given language.  See the [Language Feature Matrix](https://github.com/apache/thrift/blob/master/LANGUAGES.md) to learn more.

## Concepts

### Versioning

Apache Thrift and the vast majority of package management systems out there conform to the [SemVer 2.0](https://semver.org/spec/v2.0.0.html) version numbering specification.  Apache Thrift uses the following versioning rules:

- *major* is currently always zero;
- *minor* is increased for each release cycle;
- *patch* is increased for patch builds between release cycles to address critical defect, security, or packaging issues

Further, if there are only packaging changes for a single third-party distribution point to correct an issue, the major.minor.patch may remain the same while adding a suffix compatible with that distribution point, for example "0.12.0.1" for nuget, or "0.12.0-1" for maven.

#### External Package Patches

It is common to have language-specific critical defects or packaging errors that need to be resolved between releases of Apache Thrift.  The project handles these on a case-by-case basis for languages that have their own [package management systems](http://apache.thrift.org/libraries).  When a language-specific patch is made, the patch level of the distribution pushed to the external package manager is bumped.

 As such, there may be cases between Apache Thrift releases where there are (for example) a `0.12.1` and `0.12.2` version of a Haskell Hackage package, and perhaps also a `0.12.3` version of a dlang dub package.  You will not find a tag or an official project release in these cases, however the code changes will be reflected in the release branch and in master.  In these cases we would not release a version of Apache Thrift nor would we refresh all the external language packages.

#### Version in the master branch

The master branch will always contain the next anticipated release version.  When a release cycle begins, a branch is cut from master.  The release branch will already have all of the correct versions, and therefore release branches can be easily merged back into master.  (This was not true of releases before 0.12.0).

### Code Repository

The authoritative repository for Apache Thrift is stored in [GitHub](https://github.com/apache/thrift).  It is mirrored by [GitBox](https://gitbox.apache.org/repos/asf?p=thrift.git).

### Branches

All code (submitted via pull request or direct push) is committed to the `master` branch.  Until version 1.0 of Apache Thrift each release branch was named `<version>`, for example in version `0.12.0` there is a branch named the same.  For version 1.0 releases any beyond, releases will have a branch named `release/<version>`.

### Tags

Up to version `0.12.0` each release of Apache Thrift was tagged with a `<version>` tag.  Starting with the `0.12.0` release, each release of Apache Thrift will be tagged with a `v<version>` tag to satisfy external package management tools (such as ones for dlang and golang).  For example the tag of version `0.12.0` is `v0.12.0`.

## Release Procedures

### Release Schedule

Apache Thrift has no official release schedule, however the project aims to release at least twice per year.

A complete release cycle will take about 1 week to complete, if things go well, with half of that time waiting for a vote.

### Release Manager

Before a release cycle begins, someone must nominate themselves on the development mailing list as the release manager for that release.  In order to be a release manager you must meet the following criteria:

1. You are a [member](http://people.apache.org/phonebook.html?pmc=thrift) of the Apache PMC group.
1. Your profile at https://id.apache.org/ is valid and contains a PGP key.  If it does not, see the [Apache OpenPGP Instructions](https://www.apache.org/dev/openpgp.html).  If your PGP private key creation seems to hang indefinitely while creating entropy, try these fixes:
    - Generate disk I/O with: `dd if=/dev/sda of=/dev/zero`
    - Install the `rng-tools` package.
1. Your PGP key is visible in the [Apache Committer Keys](http://people.apache.org/keys/committer/) for code signing.  This list is updated periodically from your Apache ID (see previous step).
1. You have read and agree with the contents of the [ASF Release Distribution Policy](https://www.apache.org/dev/release-distribution.html).
1. You have access and the ability to use subversion.  All distribution artifacts are released through a subversion commit.
1. You can build in the Linux Docker Container, and you have Visual Studio 2017.
1. You have sufficient time to complete a release distribution.

### Release Candidate

All Apache Thrift releases go through a 72-hour final release candidate voting procedure.  Votes from members of the Apache Thrift PMC are binding, and all others are non-binding.  For these examples, the `master` branch is at version 1.0.0 and that is the next release.

1. Scrub the Apache Jira backlog.  There are a couple things to do:

    1. [Open Issues without a Component](https://issues.apache.org/jira/issues/?filter=-1&jql=project%20%3D%20THRIFT%20and%20status%20!%3D%20Closed%20and%20component%20is%20empty) - make sure everything has an assigned component, as the release notes are grouped together by language.

    1. [Open Issues with a Fix Version](https://issues.apache.org/jira/issues/?filter=-1&jql=project%20%3D%20THRIFT%20and%20status%20in%20(OPEN%2C%20%27IN%20PROGRESS%27%2C%20REOPENED)%20and%20fixVersion%20is%20not%20empty) - these will be issues that someone placed a fixVersion on in Jira, but have not been resolved or closed yet.  They are likely stale somehow.  Resolutions for these issues include resolving or closing the issue in Jira, or simply removing the fixVersion if the issue hasn't been fixed.

    1. [Open Blocking Issues](https://issues.apache.org/jira/issues/?filter=-1&jql=project%20%3D%20THRIFT%20and%20priority%20in%20(blocker)%20and%20status%20not%20in%20(closed)%20order%20by%20component%20ASC) - blocking issues should block a release.  Scrub the list to see if they are really blocking the release, and if not change their priority.

    1. [Open Critical Issues](https://issues.apache.org/jira/issues/?filter=-1&jql=project%20%3D%20THRIFT%20and%20priority%20in%20(critical)%20and%20status%20not%20in%20(closed)%20and%20type%20not%20in%20(%22wish%22)%20order%20by%20component%20ASC) - this list will end up in the known critical issues list in the changes file.  Scrub it to make sure everything is actually critical.

    It is healthy to scrub these periodically, whether or not you are making a new release.

1. Check that the version number in the `master` branch matches the version number of the upcomning release.  To check the `master` branch version, run:

    ```bash
    thrift$ grep AC_INIT configure.ac | cut -d'[' -f3 | cut -d']' -f1
    1.0.0
    ```

    If it does not match (this should be extremely rare), you need to submit a pull request setting the `master` branch to the desired version of the upcoming release.  In the following example, we prepare to commit a branch where the version number is changed from `1.0.0` to `1.1.0`:

    ```bash
    thrift$ git checkout -b fix-version-for-release
    thrift$ build/veralign.sh 1.0.0 1.1.0
    # check to see if any of the manually modified files needs changes
    thrift$ git push ... # make a pull request
    ```

1. Create a release branch for the release, in this example `1.0.0`:

    ```bash
    thrift$ git checkout master
    thrift$ git pull
    thrift$ git checkout -b "release/1.0.0"
    thrift$ git push
    ```

    Now there is a `release/1.0.0` branch in GitHub for Apache Thrift.

    By creating a release branch we allow work to continue on the `master` branch for the next release while we finalize this one.  Note that `release/1.0.0` and `master` in this example are now identical, and therefore it is possible to merge the release branch back into `master` at the end of the release!

1. Modify these files manually, inserting the release into them at the appropriate location.  Follow existing patterns in each file:
    - `doap.rdf`
    - `debian/changelog`

1. Generate the content for `CHANGES.md` - this is one of the most time-consuming parts of the release cycle.  It is a lot of work, but the result is well worth it to the consumers of Apache Thrift:

    1. Find all [Issues Fixed but not Closed in 1.0.0](https://issues.apache.org/jira/issues/?filter=-1&jql=project%20%3D%20thrift%20and%20fixVersion%20%3D%201.0.0%20and%20status%20!%3D%20closed) (adjust the version in the link to suit your needs).

    1. Export the list of issues to a CSV (Current Fields) and open in Excel (or a similar spreadsheet).

    1. Hide all columns except for the issue id (i.e. THRIFT-nnnn), the component (first one), and the summary.

    1. Sort by component ascending and then by id ascending.

    1. Create a fourth column that will contain the contents of each line that goes into the release notes.  Once you have the formula working in one cell paste it into the other rows to populate them.  Use a formula to get the column to look like this:

        ```vcol
        Issue       Component      Summary     RelNote
        THRIFT-123  C++ - Library  Drop C++03  [THRIFT-123](https://issues.apache.org/jira/browse/THRIFT-3978) - Drop C++03
        ```

        For example, if the row above was row "B" in EXCEL it would look something like:

        ```text
        =CONCAT("[", B1, "]",
                "https://issues.apache.org/jira/browse/", 
                B1, " - ", B3)
        ```

    1. Create a level 3 section in `CHANGES.md` under the release for each component and copy the items from the RelNote column into the changes file.

    1. Find all [Open Critical Issues](https://issues.apache.org/jira/issues/?filter=-1&jql=project%20%3D%20THRIFT%20and%20priority%20in%20(critical)%20and%20status%20not%20in%20(closed)%20and%20type%20not%20in%20(%22wish%22)%20order%20by%20component%20ASC) and add them to `CHANGES.md` in the list of known critical issues for the release.

1. Commit all changes to the release branch.

1. Generate the source tarball.

    1. On a linux system get a clean copy of the release branch, for example:

        ```bash
        ~$ git clone -b "release/1.0.0" git@github.com:apache/thrift.git thrift-1.0.0-src
        ```

    1. In the clean copy of the release branch, start a docker build container and run `make dist`:

        ```code
        ~$ cd thrift-1.0.0-src
        ~/thrift-1.0.0-src$ docker run -v $(pwd):/thrift/src:rw \
            -it thrift/thrift-build:ubuntu-bionic /bin/bash
        root@8b4101188aa2:/thrift/src# ./bootstrap.sh && ./configure && make dist
        ```

        The result will be a file named `thrift-1.0.0.tar.gz`.  Check the size and make sure it is roughly 4MB.  It could get larger over time, but it shouldn't jump by orders of magnitude.  Once satisfied you can exit the docker container with `exit`.

    1. Generate signatures and checksums for the tarball:

        ```bash
        gpg --armor --output thrift-1.0.0.tar.gz.asc --detach-sig thrift-1.0.0.tar.gz
        md5sum thrift-1.0.0.tar.gz > thrift-1.0.0.tar.gz.md5
        sha1sum thrift-1.0.0.tar.gz > thrift-1.0.0.tar.gz.sha1
        sha256sum thrift-1.0.0.tar.gz > thrift-1.0.0.tar.gz.sha256

1. Generate the Windows Thrift Compiler.  This is a statically linked compiler that is portable and folks find it useful to be able to download one, especially if they are using third-party distributed runtime libraries for interpreted languages on Windows.  There are two ways to generate this:

    - Using a Development VM

        1. On a Windows machine with Visual Studio, pull down the source code and checkout the release branch.
        1. Open an x64 Native Tools Command Prompt for VS 2017 and create an out-of-tree build directory.
        1. Install the latest version of cmake.
        1. Install chocolatey and install winflexbison with chocolatey.
        1. Run cmake to generate an out-of-tree build environment:
            ```cmd
            C:\build> cmake ..\thrift -DBISON_EXECUTABLE=c:\ProgramData\chocolatey\lib\winflexbison\tools\win_bison.exe -DFLEX_EXECUTABLE=c:\ProgramData\chocolatey\lib\winflexbison\tools\win_flex.exe -DWITH_MT=ON -DWITH_SHARED_LIB=OFF -DWITH_CPP=OFF -DWITH_JAVA=OFF -DWITH_HASKELL=OFF -DWITH_PYTHON=OFF -DWITH_C_GLIB=OFF -DBUILD_TESTING=OFF -DBUILD_TUTORIALS=OFF -DBUILD_COMPILER=ON
            C:\build> cmake --build . --config Release
            ```

    - Using [Docker for Windows](../build/docker/msvc2017/README.md), follow the instructions for building the compiler.
    - In both cases:
        1. Verify the executable only depends on kernel32.dll using [depends.exe](http://www.dependencywalker.com/).
        1. Copy the executable `thrift.exe` to your linux system where the signed tarball lives and rename it to `thrift-1.0.0.exe` (substitute the correct version, of course).
        1. Sign the executable the same way you signed the tarball.

1. Upload the release artifacts to the Apache Dist/Dev site.  This requires subversion:

    ```bash
    ~$ mkdir -p dist/dev
    ~$ cd dist/dev
    ~/dist/dev$ svn co "https://dist.apache.org/repos/dist/dev/thrift" thrift
    ~/dist/dev$ cd thrift
    ```

    Copy the tarball, windows compiler executable, and 8 additional signing files into a new directory for the release:

    ``` bash
    ~/dist/dev/thrift$ mkdir 1.0.0-rc0
    # copy the files into the directory
    ~/dist/dev/thrift$ svn add 1.0.0-rc0
    ```

    The layout of the files should match the [current release](https://www.apache.org/dist/thrift/).  Once done, add the release candidate and check it in:

    ```bash
    ~/dist/dev/thrift$ svn status
    # verify everything is correct
    ~/dist/dev/thrift$ svn commit -m "Apache Thrift 1.0.0-rc0 in dist dev" \
        --username <apache-username> --password <apache-password>
    ```

1. Verify the release candidate artifacts are available at:

    [https://dist.apache.org/repos/dist/dev/thrift/](https://dist.apache.org/repos/dist/dev/thrift/)

1. Send a voting announcement message to `dev@thrift.apache.org` following this template as a guide:

    ```code
    To: dev@thrift.apache.org
    Subject: [VOTE] Apache Thrift 1.0.0-rc0 release candidate
    ---
    All,

    I propose that we accept the following release candidate as the official Apache Thrift 1.0.0 release:

    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0-rc0.tar.gz

    The release candidate was created from the release/1.0.0 branch and can be cloned using:

    git clone -b release/1.0.0 https://github.com/apache/thrift.git

    The release candidates GPG signature can be found at:
    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0-rc0.tar.gz.asc

    The release candidates checksums are:
    md5: 
    sha1: 
    sha256: 


    A prebuilt statically-linked Windows compiler is available at:
    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0-rc0.exe

    Prebuilt statically-linked Windows compiler GPG signature:
    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0-rc0.exe.asc

    Prebuilt statically-linked Windows compiler checksums are:
    md5: 
    sha1: 
    sha256: 

    
    The source tree as ZIP file to be published via Github releases:
    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0-rc0.zip

    ZIP source tree GPG signature:
    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0-rc0.zip.asc

    ZIP source tree checksums are:
    md5: 
    sha1: 
    sha256: 
    
    The CHANGES list for this release is available at:
    https://github.com/apache/thrift/blob/release/1.0.0/CHANGES.md


    Please download, verify sig/sum, install and test the libraries and languages of your choice.

    This vote will close in 72 hours on 2019-07-06 21:00 UTC

    [ ] +1 Release this as Apache Thrift 1.0.0
    [ ] +0
    [ ] -1 Do not release this as Apache Thrift 1.0.0 because...
    ```

1. If any issues are brought up with the release candidate, you will need to package another and reset the voting clock.

Voting on the development mailing list provides additional benefits (wisdom from [Christopher Tubbs](https://issues.apache.org/jira/browse/THRIFT-4506?focusedCommentId=16791902&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-16791902)):
- It creates a public record for the vote,
- It allows for participation/evaluation from our wider user audience (more diversity in evaluators improves quality), and
- It provides more entry points for potential future committers/PMC members to earn merit through participation.

### Official Release

1. Send a message to `dev@thrift.apache.org` with the voting results.  Use this template as a guide:

    ```code
    To: dev~thrift.apache.org
    Subject: [VOTE][RESULT] Release Apache Thrift 1.0.0
    ---
    All,

    Including my own vote of +1 we have N binding +1 and no -1.
    The vote for the Apache Thrift 1.0.0 release is ***successful***.
    Thank you to all who helped test and verify.
    ```

1. Use svn to checkout the release part of thrift (similar to dev) and copy the files over from dev, matching the previous release structure:

    ```bash
    ~$ mkdir -p dist/release
    ~$ cd dist/release
    ~/dist/release$ svn co "https://dist.apache.org/repos/dist/release/thrift" thrift
    ~/dist/release$ cd thrift
    ~/dist/release/thrift$ mkdir 1.0.0
    ~/dist/release/thrift$ cp -p ../../dev/thrift/1.0.0-rc0/* 1.0.0/
    ~/dist/release/thrift$ svn status
    # verify everything is correct
    ~/dist/release/thrift$ svn commit -m "Apache Thrift 1.0.0 official release" \
        --username <apache-username> --password <apache-password>
    ```

    **NOTE** One you check in, you need to wait about a day for all the mirrors to update.  You cannot send the announcement email or update the web site until the mirrors are updated.

1. Create and push a tag for the release, for example "v1.0.0".

    **NOTE:** All new releases must have the "v" prefix to satisfy third  party package managers (dlang dub, golang, etc..)

    **NOTE:** You **should** [sign the release tag](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work).  Since you already have a GPG signing key for publishing the Apache Release, you want to [upload that key to your GitHub account](https://help.github.com/en/articles/adding-a-new-gpg-key-to-your-github-account).  Once the key is known by GitHub you can sign the tag.

    ```bash
    ~/thrift$ # make sure you are on the release branch
    ~/thrift$ git checkout release/1.0.0
    ~/thrift$ git pull
    ~/thrift$ git tag -s v1.0.0 -m "Version 1.0.0"
    ~/thrift$ git push --tags
    ```

    **NOTE:** If you get the error "gpg failed to sign the data" when tagging, try this fix: "export GPG_TTY=$(tty)"

1. Create a new release from the [GitHub Tags Page](https://github.com/apache/thrift/tags).  Attach the statically built Windows thrift compiler as a binary here.

1. Merge the release branch into master.  This ensures all changes made to fix up the release are in master.

    ```bash
    ~/thrift$ git checkout master
    ~/thrift$ git pull
    ~/thrift$ git merge release/1.0.0
    ```

    The merge of 1.0.0 into master should proceed as a fast-forward since the 1.0.0 release branch.  If there are discrepancies the best thing to do is resolve them and then submit a pull request.  This pull request must be *MERGED* and not *REBASED* after the CI build is successful.  You may want to do this yourself and mark the pull request as `[DO NOT MERGE]`.

1. Update the ASF CMS content for thrift to include the new release.  Note over time we will retire this in favor of including all documentation in the GitHub repository.  The page with the variables that are important like the current release or distribution links is in trunk/lib/path.pm in the ASF CMS for thrift.

    1. Go to the [ASF CMS for Thrift](https://cms.apache.org/thrift/).
    1. Get a working copy.
    1. On the top right, click on `trunk`.
    1. Navigate into `lib`.
    1. Open `path.pm`.
    1. Edit
    1. Change `current_release` and `current_release_date` to reflect the correct information.
    1. Submit
    1. Commit
    1. Submit
    1. Follow Staging Build until it completes.
    1. Open the Staged site.
    1. Ensure the download links work.
    1. Publish Site.

1. Make an announcement on the dev@ and user@ mailing lists of the release.  There's no template to follow, but you can point folks to the official web site at https://thrift.apache.org, and to the GitHub site at https://github.org/apache.thrift.

### Post-Release

1. Visit https://reporter.apache.org/addrelease.html?thrift and register it.  You will get an automated reminder as the one who committed into dist.  This informs the Apache Board of Directors of releases through project reports.

1. Create a local branch to bump the release number to the next anticipated release:

    ```bash
    ~/thrift$ git checkout -b bump-master
    ~/thrift$ build/veralign.sh 1.0.0 1.1.0
    ```

    The veralign script will set the version number in all of the language packaging files and headers.  You do not need to worry about the manually modified files at this time.  You should however ensure everything is correct by looking at the diff.

1. Create a pull request to advance master to the next anticipated release.

1. In Apache Jira, select all tickets where the fix version is the release and the status is not closed ([example](https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20fixVersion%20%3D%201.0%20%20and%20status%20!%3D%20Closed)) and use the bulk editing tool to close them.
1. **FIXME** Ask someone with admin access to Apache Jira to change the fixVersion in question from unreleased to released, for example:
    https://issues.apache.org/jira/browse/THRIFT-4686

1. Ensure that the [Jira release page](https://issues.apache.org/jira/projects/THRIFT?selectedItem=com.atlassian.jira.jira-projects-plugin%3Arelease-page&status=unreleased) for the version has the same number of issues in the version as issues done, and that there are no issues in progress and no issues to do, and no warnings.  Finally, mark it as released and set the date of the release.
  
* [Report any CVEs](https://apache.org/security/committers.html) that were fixed.  You can email `security@apache.org` if you are not sure if there are any CVEs to report.

#### Third Party Package Managers

See https://thrift.apache.org/lib/ for the current status of each external package manager's distribution.  Information below is from the 0.12.0 release:

  > This section needs to be updated with detailed instructions for each language, or pointers to the README.md files in each language directory with detailed release instructions for the given package management system.

* [dart] Releasing this requires a google account.
  * You will need to install the same version of dart that is used in the docker image.
  * Go into lib/dart and run "pub publish --dry-run" and resolve any warnings.
  * Run "pub publish" and go through the google account authorization to allow it.
* [dlang] Within a day, the dlang dub site https://code.dlang.org/packages/apache-thrift?tab=info
  should pick up the release based on the tag.  No action needed.
* [haskell] https://hackage.haskell.org/package/thrift
    https://jira.apache.org/jira/browse/THRIFT-4698
* [npmjs] @jfarrell is the only one who can do this right now.
    https://issues.apache.org/jira/browse/THRIFT-4688
* [perl] A submission to CPAN is necessary (normally jeking3 does this):
  * Checkout the release branch or tag on a linux system.
  * Fire up the docker build container.
  * Run "make clean" and remove any gen-perl directories.
  * Inside `lib/perl` run the script `build-cpan-dist.sh`.
  * Upload the resulting package.  If there's a mistake that needs to be corrected,
    increase the suffix. (_1, _2, ...) and upload another.  You cannot replace a release on CPAN.
* [php] @jfarrell, @bufferoverflow, @jeking3 are the only ones who can do this right now.
  * Once the release is tagged, one just has to hit the "Update" button to pick it up.
* [pypi] @jfarrell is the only one who can do this right now.
    https://issues.apache.org/jira/browse/THRIFT-4687
* [rust] Any thrift project committer is allowed to upload a new crate.

If you have any questions email `dev@thrift.apache.org`.
