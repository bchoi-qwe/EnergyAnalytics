# Project Notes

This folder is for project notes that explain why certain datasets, metadata,
and architecture decisions exist.

Current notes:
- `contract-expiry-metadata.md`: why `RTL::expiry_table` is included, what it
  is for, and when to use it in the analytics build.
- `fred-shell-fix.md`: why the repo startup profile patches `quantmod` FRED
  fetches in this shell environment.
