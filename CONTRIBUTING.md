Contributing
-------------

We welcome [bug reports][issues] and [pull requests][PRs] from anyone! Some general contribution guidelines:

* Have commit messages contain information about *why* a particular change was made, not just what the change was
* Keep your individual commits small, performing a single unit of work, and whenever possible successfully building at each point
  * In particular, separate out refactoring from new code that takes advantage of the new organization
* Document all externally-visible changes, in at least the following places where applicable:
  * Haddock on top-level definitions
  * Updates to the project [README][README]
  * Updates to the project [changelog][changelog]
* Pull requests against `master` should be completely ready to go into a release before merging.

[issues]: https://guides.github.com/features/issues/
[PRs]: https://help.github.com/articles/about-pull-requests/
[README]: ./README.md
[changelog]: ./ChangeLog.md
