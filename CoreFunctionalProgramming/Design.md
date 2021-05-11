# Design

## ARC (Automatic Reference Counting)

will be able to collect garbages with ARC only

## No infinite non-recurring data structures and full lazy evaluation

They are sometimes complex and problematic.

But I don't want `if` to be a macro. How about add [Idris-style Lazy](https://web.archive.org/web/20210511112931/https://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#laziness) everywhere?

Still have problems implementing in FullRel. Add some feature to tell FullRel to ignore certain relations if all referenced variables are not linked to `q`?
