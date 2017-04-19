## What is machinator?

[Machinator](https://github.com/ambiata/machinator) is a simple, data description language.

It defines types for use in [projector](projector.html) templates.


## Anatomy of a machinator file

Let's take a look a simple example.

```
-- machinator @ v1

data Size =
    Small
  | Medium
  | Large

record Input = {
    name : String
  , size : Size
  }
```

### version

The first line is the version, which is mandatory.
If new features or syntax are added to machinator, this version may
change, but old file will continue to work.

```
-- machinator @ v1
```

### `data`

Next is the `Size` `data` definition.

- `Size` is one form of "type" (there are others).
- `Small`, `Medium` and `Large` are known as "constructors"
- The `|` symbol is a way to separate constructors
  It can also be defined on a single line:
  `data Size = Small | Medium | Large`

### `record`

Finally is the `Input` `record`

- `Input` is also the `type` (similiar to `Size`)
- `name` and `size` are field names
- `name` has the type `String`
- `size` has the type `Size` (which is what we defined above)


## What are the predefined types?

When running loom there are a few predefined "primitive" types
that are available in every machinator file.

- `String`

  This is specifically just a stream of characters

- `Bool`

  A Boolean, constructed with and matched by `True` and `False`

- `Html`

  This could be any raw html block

- `List`

  This is a container of another type eg. `List String`.
  You can even have a list of lists eg. `List (List String)`!
