## What is projector?

[Projector](https://github.com/ambiata/projector) is a type-safe
[template language](https://en.wikipedia.org/wiki/Web_template_system).


## Anatomy of a projector file

Let's take a look at a simple example:

```html
<p>Hello world</p>
```

Projector accepts any [well-formed](https://en.wikipedia.org/wiki/Well-formed_element)
HTML. That means it's not permissable to not close tags, either with
by self-closing or with a closing tag (of the exact same name).

Perhaps something a little more interesting:

```prj
\input : Input ->

<div>
  {
    case input of
      Input name size ->
        <a id="hello" class="{
          case size of
            Small ->
              "small"
            Medium ->
              "medium"
            Large ->
              "large"
        }">
          My name is: { text name }
        </a>
  }
</div>
```

Starting from the beginning:

- `\`

  indicates the start of a "lambda" (another word for anonymous function).
  This can be done optionally at the start of a template if you accept arguments,
  or when using `each` (see below).

- `input : Input`

  A lambda value called "input" which has the type "Input", which can be declared
  in a [machinator](machinator.html) file.

- `->`

  The end of the lambda.

- `<div>...</div>`

  As above we can write (well-formed) html directly in the template.

_ `{ ... }`

  Using `{` indicates the start of an "expression block", where certain kinds of
  logic can be preformed on value that might be passed to the template.

- `case input of`

  "case" is the work-horse of projector and allows values to be matched on,
  and different logic applied depending on the value.

- `Input name size ->`

  "Input" is the name of the only construtor for the "Input" record (it's
  important to keep in mind that they can be different for `data` types).
  "name" is a _new_ name for the first argument of the "Input" constructor.
  "size" is a _new_ name for the second argument of the "Input" constructor.
  "->" signifies the end of _this_ match.

- `<a ...>...</a>`

  Ahh, we're back to html again.

- `id="hello"`

  Declaring normal html attributes is fine.

- `class="{...}"`

  Attribute strings can also can expressions, which can only return "String"
  values and not "Html" (which makes sense if you think about how hml works).

- `case size of`

  There case is again. We're matching on the size value we matched from
  the case above. It has the type "Size".

- `Small ->`

  This is just one of the constructors of the data type "Size".

- `"small"`

  We're constructing a "String" type which will be the value of "href"

- `Medium -> "medium"; Large -> "large"`

  The final two matches of the other two "Size" constructors.

  Loom will throw errors if any constructors _haven't_ been matched.
  You have to match _everything_. This is useful especially when we add
  new constructors to a data type and want to know where we might care
  about it.

  Projector is whitespace-sensitive, and uses the indent level to
  determine where a case begins and ends. In general, indent further
  to the right when writing a case, and go back to the left to start a
  new case. These rules also apply to lambdas.

- `My name is:`

  Some normal text to be rendered in html.

- `{ text name }`

  Opening another expression block, calling "text" with the "name" value
  that we matched way back from the "Input" constructor.


## How do I call another template?

It's important to keep in mind that templates are _just_ functions.
Each template is available under a function name that is derived from
its filepath and project name.


So, in Bikeshed, `modules/my-component/template.prj` will be
accessible under the name `bikeshed/modules/my-component/template`.

## What is a function?

A function is something that accepts an argument and returns a value.
That value in turn may itself be a function, which returns a value,
and so on. This is often expressed with the following syntax:

```prj
functionName : String -> String -> Html
```

In this case `functionName` requires a value of type `String`
to be passed/applied, which return another function that requires
another value of type `String`, that finally returns an `Html` value.


## How do I call a function?

Taking the example of `functionName` above, we need to "apply" two
`String` values before we can return the `Html`. This function application
is expressed like this:

```prj
{ functionName "a" "b" }
```

The space between the function and each subsequent argument is the
critical step.

If you happen to want to pass a value from a function that takes
it's own arguments, you need to use parenthesis to express that intent.

```prj
{ functionName "a" (anotherFunction "b") }
```

This is _not_ the same as passing three arguments to `functionName`:

```prj
{ functionName "a" anotherFunction "b" }
```


## What are the predefined functions?

When running loom there are a few predefined "primitive" functions
that are available in every projector file.


### `text : String -> Html`

This function takes a normal `String` type and returns `Html`.

```prj
{ text "hello" }
```

Although it's more likely that you'll have a `String` variable that
needs to be converted to `Html`.

```prj
{ text myStringVariable }
```


### `blank : Html`

This function return a blank `Html` value.
This is mainly useful/required when matching and some cases don't return any
html.

```prj
{
  case input of
    Small ->
      <i>small</i>
    Medium ->
      <b>medium</b>
    Large ->
      { blank }
}
```


### <a id="each">`each : List a -> (a -> Html) -> Html`</a>

A specialised, but very useful function, for iterating over each element of a `List`,
and specifically returning `Html` values, to eventually be joined into a single
`Html` result.

```prj
\names : List String ->
{
  each names \name ->
    <span>{ text name }</span>
}
```
