# book.of.workflow 0.1.3.1

## Bug Fixes

- `read_snippet()`: Updated code to skip lines with matching snippet tags or specifying `doc = NULL` to allow nested snippet signatures to be read and processed without the need for aliasing `read_snippet()`.

# book.of.workflow 0.1.3.0

- Closed [Add Snippet Selector](https://github.com/delriaan/book.of.workflow/issues/3) and [Interactive GUI Confirmation](https://github.com/delriaan/book.of.workflow/issues/5)
- Added library `htmltools` to Imports
- `make_snippet()`: 
   - Argument `doc` in the generated snippet is defaulted to `NULL`
- `read_snippet()`:
   - When no matching snippets are found, a message is produced.
   - Replaced messaging calls to use `cli::cli_alert_*` functions.
   - Argument `doc` is defaulted to `NULL`
   - Argument `action`:
      - Removed functionality for `goto`: if supplied, nothing happens.
      - Set the default value to `parse`
- `snippets_toc()`:
   - Argument `doc` is defaulted to `NULL`
   - Added argument `action` to be passed to `read_snippet()` when `choose` is `TRUE`

# book.of.workflow 0.1.2.1120

- `snippets_toc()`: Added a check for interactivity when `choose = TRUE`

# book.of.workflow 0.1.2.1111

- Documentation updates
- Export of function aliases `read.snippet()` and `make.snippet()`

# book.of.workflow 0.1.2.1110

- Documentation updates only
# book.of.workflow 0.1.2.1110

- Documentation updates only

# book.of.workflow 0.1.2.1000

## Enhancements

- `save_image()`: 
   - Added `svDialogs::okCancelBox` to trigger when argument `safe` is `TRUE`
- `read_snippet()`: 
   - Argument `...` changed to be processed with `rlang::enexprs`
- `snippets_toc()`:
   - Added the `choose` argument
