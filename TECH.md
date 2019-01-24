# Pointer Scanning

Once we've found a value in memory we need to locate a stable pointer to it.

We currently have access to values being stored in "static" memory.
Static memory is memory that is part of loaded images (.exe files, .dll files) and these can always be enumerated for stable memory addresses.

From these, we need to find pointers and corresponding offsets that lead to our data.

* Scanning all pointers is prohibitively expensive. We end up with a tree of permutations that rapidly explodes.
  Other pointers might also be at offsets, so some fuzzing will be needed.
  Note: Maybe it's possible to build a non-cyclic map out of all pointers?
* Scanning over all memory and identifying pointers is probably what Cheat Engine calls "building a pointermap".

#### Algorithm

Say we have the following static memory locations, where 'FROM -> TO` denotes a pointer.

```
"app.exe" -> "app.exe" + 0x1000
"app.exe" + 0x100 -> A
A + 0x100 -> B
```

## TODO
Build a predicate that derefs pointer and checks that they are pointing to something in-range of process memory.