---
toc-title: Table of contents
---

# ![book](book_small.png) Book of Workflow

**book.of.workflow** provides action-oriented functions that support
data processing and connection workflows. The following functional
families are covered in `book.of.workflow`.

## Installation

Use
`remotes::install_github("delriaan/book.of.workflow", subdir = "pkg")`
to install the latest version from GitHub.

## Functions

The following functional families are covered in `book.of.workflow`:

### Chapter 1: Environment Integrity

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `check_env_arg`                     Resolves an environment reference
                                      from symbols, strings, quosures, or
                                      expressions.

  `check.env`                         Checks environments for required
                                      objects registered with
                                      `%must.have%`.

  `%must.have%`                       Declares required object names for
                                      an environment integrity check.

  `%+=%`                              Adds named objects from a list into
                                      an environment.

  `%-=%`                              Removes named objects from an
                                      environment.
  -----------------------------------------------------------------------

### Chapter 2: Environment Processing

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `load_unloaded`                     Loads packages that are not already
                                      attached, with optional install
                                      support.

  `load.unloaded`                     Alias of `load_unloaded`.

  `save_image`                        Saves selected objects from an
                                      environment to a timestamped
                                      `.rdata` file.

  `save.obj`                          Alias of `save_image`.

  `copy_obj`                          Copies, renames, or moves objects
                                      across environments.

  `copy.obj`                          Alias of `copy_obj`.

  `refer_to`                          Resolves an environment from an
                                      attached name or expression.

  `refer.to`                          Alias of `refer_to`.
  -----------------------------------------------------------------------

### Chapter 3: Workflow Management

  ---------------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- ---------------------------------------
  `is_studio_audience`                Detects whether execution is
                                      interactive in an RStudio session.

  `check_action`                      Validates snippet action mode values.

  `read_snippet`                      Finds and reads tagged code snippets
                                      from source files.

  `read.snippet`                      Alias of `read_snippet`.

  `make_snippet`                      Inserts or generates snippet tags and
                                      boilerplate snippet wrappers.

  `make.snippet`                      Alias of `make_snippet`.

  `snippets_toc`                      Lists snippet tags and optionally runs
                                      selected snippets.

  `snippets.toc`                      Alias of `snippets_toc`.

  `%read%`                            Infix shorthand for
                                      `read_snippet(..., action = "exec")`.
  ---------------------------------------------------------------------------
