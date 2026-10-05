This directory collects "newsfragments": short files that each contains
a snippet of markdown formatted text that will be added to the next
release notes. This should be a description of aspects of the change
(if any) that are relevant to users. (This contrasts with the
commit message and PR description, which are a description of the change as
relevant to people working on the code itself.)

Each file should be named like `<ISSUE>.<TYPE>.md`, where
`<ISSUE>` is an issue numbers, and `<TYPE>` is one of:

* `feature`
* `bugfix`
* `performance`
* `doc`
* `internal`
* `removal`
* `misc`

So for example: `123.feature.md`, `456.bugfix.md`

If the PR fixes an issue, use that number here. If there is no issue,
then open up the PR first and use the PR number for the newsfragment.

The release notes are assembled with
[eisenbote](https://github.com/fe-lang/eisenbote), checked out next to this
repository. Run `../eisenbote/bin/eisenbote draft --version <next version>`
to preview the release notes, and `../eisenbote/bin/eisenbote check` to check
the fragment names (`make -C ../eisenbote download` fetches its executable).
