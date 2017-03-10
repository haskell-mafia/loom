## What is a component?

A component is a collection of files intended to form a self-contained element
that can be reused by other compoonents.
This include:

- a template for the html
- related images and styles
- concrete examples
- documentation


## What are the possible component files?

The following are all potential files that can exist in a component.

- `README.md`
- [`*.scss`](#sass)
- [`*.svg` + `*.png` + `*.jpg`](#assets)
- [`data.mcn`](machinator.html)
- [`template.prj`](projector.html)
- [`examples/*.prj`](#examples)
- [`mocks/*.prj`](#mocks)


### <a id="sass">SASS</a>

- [Official guide](http://sass-lang.com/guide)
- [Ambiata guidelines](https://github.com/ambiata/engineering/blob/master/how/guideline/language/css.md)

NOTE: Please don't use `@import` in your SASS files as they will not be
"watched" for changes.


### <a id="assets">Assets</a>


### How do I reference an asset?

When referring to component assets, such as images, it is important to
refer to them correctly so that the final path will be handled correctly.


### From SASS?

Just reference it, relative from the current project (_not_ relative to the component).

```sass
background-image: url('components/my-component/test.png');
```

### From projector?

You can't. This is a bug and needs to be fixed.
### <a id="examples">Examples</a>

Examples are rendered inline on a component page as an inline snippet.
This is commonly used for smaller components.

The example templates cannot accept any parameters.

```prj
{ myComponent "hello" }
```

As a comparison, the following example would be invalid and would result in an
error at build time:

```prj
\title : String ->
{ myComponent title }
```


### <a id="mocks">Mocks</a>

Mocks are added when the component is intended to be viewed as a full, separate
page rather than inline. In this case a link to that page will be added for each
mock.

The mock templates cannot accept any parameters (see example above for more
details).

```prj
{ globalTemplate (myComponent "hello") }
```
