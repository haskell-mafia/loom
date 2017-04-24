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
\input : Input -> Html =

<div>
  {
    case input of
      Input name size ->
        <a id="hello" class="{{
          case size of
            Small ->
              "small"
            Medium ->
              "medium"
            Large ->
              "large"
        }}">
          {- TODO localisation -}
          My name is: {{ name }}
        </a>
  }
</div>
```

Starting from the beginning:

- `\`

  Indicates the start of the *type signature*. This indicates the
  template's *arguments*, which have both a name and a type, as
  well as its *result type*.

- `input : Input`

  An *argument* named "input", which has the type "Input".

  We can define types in a [machinator](machinator.html) file.

- `->`

  This arrow separates arguments in the type signature.

- `Html`

  The last type in a type signature is the *result type*. Usually, a
  template produces HTML, like this example.

  The result type does not
  have a name, since it is constructed by the template, rather than
  being passed in as a parameter.

- `=`

  The end of the type signature. The template begins immediately after
  the equal sign.

- `<div>...</div>`

  We can write HTML directly in the template.

_ `{ ... }`

  Using `{` indicates the start of an "expression block", where certain kinds of
  logic can be preformed on value that might be passed to the template.
  (These expressions need to be things that produce `Html` values.)

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

- `class="{{...}}"`

  Attribute strings can also be expressions, which can only return "String"
  values and not "Html" (which makes sense if you think about how html works).
  This double-curly (`{{ ... }}`) here in the _attribute_ means something
  that takes a string and gives you a string as a result; this double-curly
  will crop up again below as meaning something different.

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

- `{- TODO localisation -}`

  A comment or note you want to add that's ignored by Projector and *doesn't*
  show up in the HTML output (unlike the usual `<!-- ... -->`).

- `My name is:`

  Some normal text to be rendered in html.

- `{{ name }}`

  A special kind of expression block that, when we're producing HTML, is
  for rendering string values as html. In this case we're taking the
  "name" value that we matched way back from the "Input" constructor,
  and producing an chunk of HTML text.


## How do I call another template?

It's important to keep in mind that templates are _just_ functions.
Each template is available under a function name that is derived from
its filepath and project name.

The default template for a given component is named `template.prj`,
though there can be more than one template in each directory.

Suppose we're working in Bikeshed on a module located in
`modules/my-component/template.prj`. The project name for Bikeshed is
`bikeshed`, set in its `loom.toml` file, so all names will be prefixed thus.

- `modules/my-component/template.prj` is the default template, so it
  gets a nice short name. It will be accessible under the name
  `bikeshed/modules/my-component`.
- `modules/my-component/foobar.prj` will be accessible under the name
  `bikeshed/modules/my-component/foobar`.

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

This is more commonly written as:

```prj
{{ myStringVariable }}
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

Note the second argument to `each`, a function which takes an item of
the list and turns it into `Html`. This is most convenient when used
with an *anonymous function* or *lambda*, as per the example below:

```prj
\names : List String -> Html =
{
  each names \name ->
    <span>{ text name }</span>
}
```

## How do I refer to CSS, JS, or image assets?

Loom provides these as Projector variables.

### `loom/css : List String`

The `loom/css` list contains every stylesheet.

We can iterate over the CSS list with `each`:

```prj
{ each loom/css \css -> <link rel="stylesheet" href="{{ css }}/> }
```

### `loom/js : List String`

The `loom/js` list contains every JS file.

We can iterate over the JS list with `each`:

```prj
{ each loom/js \js -> <script src="{{ js }}"/> }
```

### `loom/js/main : String`

Each JS file is also provided as a variable.

For example, we could call the `main` JS bundle like so:

```prj
<script src="{{ loom/js/main }}"/>
```

... and we could call the `first` JS bundle like so:

```prj
<script src="{{ loom/js/first }}"/>
```


### `component/name/here/image/svg : String`

Images inside components are available under their component name,
followed by their file name, followed by their extension.

If `component/name/here` has a file named `ambiata.jpg`, we could
refer to it like so:

```prj
<img src="{{ component/name/here/ambiata/jpg }}"/>
```

(Note that `.jpg` has been replaced by `/jpg`, because `.` is used for
record syntax.)

## How do I make new values?

Back to our earlier function example:

```prj
{ functionName "a" "b" }
```

We have a couple of `String` values, `"a"` and `"b"`. We've made them up on the spot (instead of receiving them as inputs to the template). We can do this for both in-built types (like String), and our own custom [Machinator data](./machinator.md) types.

### Predefined Types

- `String`

  Constructed with quotation marks only, eg. `"foo"`. If you want to use a quote mark within a string, use a `\`, eg. `"It's \"nice\" to meet you."`

- `List`

  Constructed with square brackets, eg. `["Hello", "World"]`.

- `Bool`

  Construct with `True` or `False`, like you do with any other Machinator `data` type (see below).


### Custom Types

Here are a couple of custom types, defined in a `data.mcn` somewhere:

```mcn
data Shape =
    Square
  | Circle

record FigureDescription = {
    title : String
  , summary : List String
  }
```

And a template called `modules/drawing/shape`

```prj
\ shape : Shape ->
  desc : FigureDescription ->
  Html =
<div> {- whatever -} </div>
```

You can use this template like so (in an mock, for example):

```prj
{
  modules/drawing/shape
    Square
    (FigureDescription "Square" ["A square.", "It's interesting!"])
}
```

Breaking this down:

- `modules/drawing/shape`

  As in the earlier sections, this is a function. It takes two arguments.

- `Square`

  Make a `Shape` data value by calling the `Square` constructor.

- `FigureDescription "Square" ["A square.", "It's interesting!"]`

  The `FigureDescription` is a constructor that expects two arguments (as if it were
  a `String -> List String -> FigureDescription` function), in the order the
  record fields were declared (`string`, then `summary`).
