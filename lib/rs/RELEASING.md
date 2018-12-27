# Publishing the thrift crate

Publishing the Rust thrift crate is straightforward, and involves two major steps:

1. Setting up your [crates.io](https://www.crates.io) account _(one-time)_

2. Packaging/publishing the Rust thrift crate itself

## Set up your crates.io account (one-time)

1. Go to [crates.io](https://www.crates.io) and click the `Log In` button at the top right.

   Log in **as the Github user with write permissions to the thrift repo!**

2. Click your user icon button at the top right and select `Account Settings`.

3. Click `New Token` next to `API Access`.

   This generates a new API key that cargo uses to publish packages to crates.io.
   Store this API key somewhere safe. If you will only use this Github account to
   publish crates to crates.io you can follow the instructions to save the
   generated key to `~/.cargo/credentials`.

## Package and Publish

You can use the automated script or run the release steps manually.

**Important**: `cargo` expects that version numbers follow the semantic versioning format.
This means that `THRIFT_RELEASE_VERSION` must have a major, minor and patch number, i.e., must
be in the form `#.##.##`.

#### Automated

Run `./release.sh [THRIFT_RELEASE_VERSION]`.

_Requires you to have stored your credentials in `~/.cargo/credentials`._

#### Manual

1. Edit `Cargo.toml` and update the `version = 1.0` key to `version = [THRIFT_RELEASE_VERSION]`

2. `git add Cargo.toml`

3. `git commit -m "Update thrift crate version to [THRIFT_RELEASE_VERSION]" -m "Client: rs"`

4. `cargo login`

    _(not required if you have stored your credentials in `~/.cargo/credentials`)_

5. `cargo clean`

6. `cargo package`

   This step fails if there are any uncommitted or ignored files. Do **not** use the `--allow-dirty`
   flag! Instead, add the highlighted files as entries in the `Cargo.toml` `exclude` key.

7. `cargo publish`
