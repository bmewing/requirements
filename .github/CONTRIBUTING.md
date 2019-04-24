# Contributing to this Project
**Here's how you can help.**

## Process
To contribute to this project you should fork the code to your own personal account and the submit a Pull Request to this project.  We use Pull Requests to develop conversations around ideas, and turn ideas into actions.

**Some PR Basics**

- Anyone can submit a Pull Request with changes they'd like to see made.
- Pull Requests should attempt to solve a single [1], clearly defined problem [2].
- Everyone should submit Pull Requests early (within the first few commits), so everyone on the team is aware of the direction you're taking.
- Authors are responsible for explicitly tagging anyone who might be impacted by the pull request and get the recipient's sign-off [3].
- The Pull Request should serve as the authority on the status of a change, so everyone on the team is aware of the plan of action.
- Any primary contributor _except_ the author can merge a pull request once all sign-offs are complete.

[1]: if there are multiple problems you're solving, it is recommended that you create a branch for each.  For example, if you are implementing a small change and realize you want to refactor an entire function, you might want to implement the refactor as your first branch (and pull request), then create a new branch (and pull request) from the refactor to implement your new _feature_.  This helps resolve merge conflicts and separates out the logical components of the decision-making process.  
[2]: include a description of the problem that is being resolved in the description field, or a reference to the issue number where the problem is reported.  Examples include; "Adding support for local CRAN mirrors" or "Refactoring functions to reduce cyclomatic complexity".  
[3]: notably, document the outcome of any out-of-band conversations in the pull request.

## Coding Conventions

- follow standard `lintr::lint_package()` guidelines with two exceptions:
  - use only `=` for assignment
  - code line length should be 120
- vertically align logically similar items.
- run `cyclocomp::cyclocomp_package_dir()` and strive for a cyclomatic complexity of no more than five
- roxygenate every function with `@param`, `@return` - ideally include `@examples` and `@details` to make it clear what new functions do
- all functions should have `testthat` tests included