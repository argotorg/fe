This directory collects "newsfragments": short files that each contains
a snippet of markdown formatted text that will be added to the next
release notes. This should be a description of aspects of the change
(if any) that are relevant to users. (This contrasts with the
commit message and PR description, which are a description of the change as
relevant to people working on the code itself.)

Each file should be named like `<PR>.<TYPE>.md`, where
`<PR>` is the number of the pull request that creates the fragment, and `<TYPE>` is one of:

* `feature`
* `bugfix`
* `performance`
* `doc`
* `internal`
* `removal`
* `misc`

So for example: `123.feature.md`, `456.bugfix.md`

Open the PR first, then use its number for the newsfragment. If a PR needs
multiple fragments of the same type, add a numeric suffix, for example
`123.feature.1.md` and `123.feature.2.md`.

Describe changes from the previous release. Do not include fixes for bugs
introduced since that release or changes superseded by later work.

The release notes are assembled with
[eisenbote](https://github.com/fe-lang/eisenbote), configured in
`eisenbote.toml`. `make check-notes` checks the fragment names, and
`target/eisenbote/bin/eisenbote draft --version <next version>` (after any
eisenbote make target, such as `make check-notes`) previews the release notes.
