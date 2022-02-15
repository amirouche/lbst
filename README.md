# Immutable Log-Balanced Search Tree

**prototype**

![pink sakura tree at daytime](https://images.unsplash.com/photo-1515863149848-223cbed59017?w=1024&q=80)

## Kesako a Log-Balanced Search Tree?

- A search tree, is a dictionary that preserves ordering according to
  an user specified function, also known under the name sorted
  dictionary.
  
- In the original description of the algorithm, LBST used a logarithm
  function to decide how to balance the tree.

## Kesako an immutable datastructure?

Immutable datastructures, also known under the name *persistent
datastructures* are datastructures that will produce new values
instead of changing, mutating, the value in-place.

Immutable datastructures are useful in situations where you need to
keep around previous versions of the data to keep around an history to
ease debugging or an *undo* feature such as in editors; another way
immutable datastructures can be put to good use is to keep the data
consistent in a concurrent or parallel programming setting, while a
flow of executition (the writer) produce a new version of the
datastructure, the readers still have access the previous version of
the truth without requiring readers to wait for the writer to finish
(single-writer / multiple readers), hence it speeds up readers.

## When is an immutable datastructure useful?

Anytime you copy a *big* data structure, you may instead use an
immutable datastructure.

## What is the difference with `dict` insertion order?

Python builtin `dict` are sorted according to the time of insertion,
if `"z"` is added to a dictionary before `"a"`, then the dictionary
will have the keys in the following order `["z", "a"]`. That is not
always what is useful.

The following kind of code is a hint that you may use LBST:

```python
frob = dict()
frob[key1] = value1
frob[key2] = value2
frob[key3] = value3
...

# then re-create the dictionary with an order given by `mykeyfunc`:
frob = {k: v for k in sorted(frob.keys(), key=mykeyfunc)
```

That is somekind of copy, see the previous hint, that re-orders the
dictionary keys according to `mykeyfunc` in order for instance to
speed up linear lookup. Using LBST, you can build a dictionary that is
sorted at construction time, hence possibly save a few cycles by
avoding a reconstruction, duplicated effort, copies, and keep lookup
times under control.

## `lbst.make(comparator=operator.lt)`

Return an immutable search tree, ordered according to
`comparator`. `comparator` must take two arguments, and return whether
the first arguments is less than the second. The default value for
`comparator` is the builtin
[`operator.lt`](https://docs.python.org/3/library/operator.html#operator.lt). You
may use a specialized `comparator` for performance reason.

## `lbst.set(tree, key, value)`

Return a tree based on `tree` where `key` is associated with
`value`.

## `lbst.delete(tree, key)`

Return a tree based on `tree` where `key` is not present.

## `lbst.size(tree)`

Return the size of `tree`.

## `lbst.min(tree)`

Return the smallest key present in `tree`.

## `lbst.max(tree)`

Return the biggest key present in `tree`.

## `lbst.cursor(tree)`

Return a cursor for `tree`. The initial position is not specified. A
cursor is stateful: its position is changed / mutated in-place.

## `lbst.cursor_clone(cursor)`

Return a cursor at the same position as `cursor` that does share state
with `cursor`.

## `lbst.cursor_seek(cursor, key)`

Position `cursor` near `key`. If `key` is present in the tree
associated with `cursor`, then the cursor will be exactly positioned
on `key` and `lbst.cursor_seek` will return `0`. Otherwise, when `key`
is not present in the tree associated with `cursor`, there is two
cases: 1) if the cursor is positioned after, then `lbst.cursor_seek`
returns `1`, and 2) if the cursor is positioned before, then
`lbst.cursor_seek` return `-1`.

That is `lbst.cursor_seek` return value are:

- Return `-1`, then `cursor` is before `key`;
- Return `0`, then `cursor` is **on** `key`;
- Return `1`, then `cursor` is after `key`.

## `lbst.cursor_key(cursor)`

Return the key associated with `cursor`.

## `lbst.cursor_value(cursor)`

Return the value associated with `cursor`.

## `lbst.cursor_next(cursor)`

Move `cursor` to the next position, that is a bigger key that is just
after the current key.

## `lbst.cursor_previous(cursor)`

Move `cursor` to the previous position, that is a smaller key that is
just before the current key.

## `lbst.to_dict(tree)`

Return a `dict` representation of `tree`. The returned `dict` is
sorted according to the comparator associated with `tree`.
