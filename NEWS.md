# semptools 0.2.8.1

- Added `change_node_label` for changing the labels of nodes. Several other functions
  were modified to adapt for this function.

- Added `to_list_of_lists` for converting a named vector to a list of lists. Specifying
  a list of lists is necessary in some cases because the a label may not be string (e.g.,
  it may be an expression). However, in most cases, all elements are strings or numbers
  and so a named vector will do. This function is to be used internally by other functions,
  not to be used by users.

# semptools 0.2.8

- Fix a bug in set_cfa_layout. It now will not raise an error for one-factor models.

- Fix some typo errors in documentation pages.

# semptools 0.2.7

- Import the pipe operator from magrittr so users no need to load the package themselves.

# semptools 0.2.6

- Update the documentation of mark_sig and mark_se to emphasize that  currently they require a lavaan output.

# semptools 0.2.5

- Used pkgdown to build a site. The first draft, with minimal customization.

# semptools 0.2.4 

- Alpha release. Ready for testing.
