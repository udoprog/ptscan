# GUI

* ~~Want a functional address list.~~
* Ability to edit type in address list.
* Ability to edit signedness in address list.
* Be able to right click a scan result to copy address.
* Be able to right click a scan result to open scan result edit menu.
  * Change the type (from drop down).
* Be able to right click scan result to delete it (shortcut: del, when selected).
* ~~Want to be able to double-click to add address to address list.~~
* Shared thread pool.
* While the process is running, show how many locations have been found so far!

# ARCH

* ~~Make updating current results independent of scan results (they are usually _much_ fewer).~~
* Make `Address` an immediate type.
* Allow adding addresses to `Addresses`.

# BUGS

* ~~Something wrong with scanning `$value == u128`.~~
  * Missed adding it to a numerical testing macro.