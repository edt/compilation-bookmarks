# compilation-bookmarks
A bookmark system for compilation commands.

### How to install

Add compilation-bookmarks to your load-path and add the following lines to your init.

```(require 'compilation-bookmarks.mode)
(compilation-bookmarks-mode)
```

### Available commands

##### compilation-bookmarks-add-bookmark
Add compilation bookmark to your collection.

##### compilation-bookmarks-remove-bookmark
Remove bookmark from your collection.

##### compilation-bookmarks-change-bookmark

Made a mistake when entering a bookmark?
With this you can change the settings of bookmark

##### compilation-bookmarks-compilation-once
Use the compilation bookmark just once and restore your previous compilation after that.

###### compilation-bookmarks-compile
Set compilation settings to the current compilation bookmark

##### compilation-bookmarks-recompile
Looks for compilation buffer to recompile or starts a new compilation.

##### compilation-bookmarks-save-bookmarks

Saves compilation bookmarks to `compilation-bookmarks-save-file'.
Done automatically when compilation-bookmarks-mode is active and you quit emacs.

##### compilation-bookmarks-load-bookmarks

Loads compilation bookmarks from `compilation-bookmarks-save-file'.
Done when compilation-bookmarks-mode is enabled.

### License

Compilation-bookmarks is published under the GPL.
