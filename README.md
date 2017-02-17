Loom
====

![](https://cloud.githubusercontent.com/assets/355756/23049526/c99ade24-f510-11e6-851c-3e7902ed310c.jpg)

> Why it's an extraordinary adventure with an interface on
> magic... stunning, high-resolution, 3D landscapes... sophisticated
> score and musical effects. Not to mention the detailed animation and
> special effects, elegant point 'n' click control of characters,
> objects, and magic spells.
>
> Beat the rush! Go out and buy Loom™ today!
>
> - Cobb, The Secret of Monkey Island

Loom is a project for developing, building and packaging web-based projects.

## Background

Currently bikeshed is the central point for the entire front-end, and has 3 separate responsibilities.

- Pattern library modules
- Application-level components
- Gulp build for generating web resource artifacts

## Requirements

From each of the top-level applications, like manor or aperture, we _must_ be able to achieve the following goals:

- Bespoke components
- Efficient completed resources
- Pattern library
   - Live and easy to change

## Layout

This would be the expected layout (either convention or possibly configuration) for the new build tool.

```
src/... .hs
lib/bikeshed/
  modules/...
  components/...
  package.json
  bower.json
components/input/
  x.scss
  x.purs
  x.js
  x-ffi.js
  README.md
  examples
  x.jpeg
  interface
package.json
bower.json
```

## Lifecycle

This is a high level flow diagram of the bikeshed gulp build.

```
              js    purs
             deps   deps
               |     |
  templates    |  .--'     assets
     |         |  |  .------' |
     |         |  |  |        |
     |         |  | sass      |
     |         |  |  | '----. |
     |         |  |  |      | |
     |         |  v  v      | |
     |         | purescript | |
     |         | |          | |
     |         | | .--------+ |
     |         | | |        | |
     |-------. | | | .------C-+
     |       | | | | |      | |
     |       v v v v v      | |
     |      javascript      | |
     |        |  .----------' |
     |        |  |  .---------'
     |        |  |  |
     |        v  v  v
     |       manifests
     |       |      |
     '---.   |      |
         v   v      v
        mocks &    bikeshed.hs
        library    bmx renderer
```

## Commands

```
# Build and package the required resources
loom build

# Run any available linting
loom lint

# Run any javascript/purescript tests for this project
loom test

# Start a running process that serves up a "live" web version of this library
loom start
```

## Required executables

The current build process requires the following (additional) executable binaries.

- Purescript (currently built by bikeshed)
- Node/NPM
- Sass

## Conceptual Warriors

- Mark Hibberd
- Charles O'Farrell
- Rob Howard
