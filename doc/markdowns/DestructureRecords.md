# Destructuring Records in Haskell

Jason and I worked on figuring this out.  Here's our sample:

```haskell
Prelude> x $ (\(Source c) -> c) source
Point {start = 4, finish = 8}
Prelude> destructureSource = (\(Source c) -> c)
Prelude> x $ destructureSource source
Point {start = 4, finish = 8}
Prelude> (x . destructureSource) source
Point {start = 4, finish = 8}
Prelude> doit source x = (x . destructureSource) source
Prelude> doit source x
Point {start = 4, finish = 8}
```

A reasonable replacedment name for `doit` would be `fieldForSource` or `srcFld`?

Hence:

```haskell
Prelude> srcFld source x = (x . destructureSource) source
Prelude> srcFld source x
Point {start = 4, finish = 8}
```

Now, having to see `start` and `finish` everywhere in our code makes me relieved that
we opted to express segments using tuples.  Here's the difference:

```haskell
Prelude> srcFld source x
Point (4,8)
```

Oooh!  This can be generalized.

```haskell
Prelude> field source x
Point (4,8)
```

And, reverse the arguments so that the field specifier comes first:

```haskell
Prelude> field x source
Point (4,8)
```






