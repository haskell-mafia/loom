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


## Command line

```
# Build and package the required resources
loom build

# Start a running process that serves up a "live" web version of this library
# and watches the filesystem for changes
loom watch
```


## Configuration

Loom is configured via a single `loom.toml` file in the root of the project.

Other loom libraries can be referenced in the set of `dependencies`.
This will include all their components/sass files as if they were directly referenced
by _this_ project, and will be visible in the generated site.

```toml
[loom]
  # Mandatory
  version = 1

  # Mandatory
  name = "my_project"

  # Optional
  dependencies = ["lib/bikeshed"]

[components]
  # Optional
  paths = ["components/*"]

[sass]
  # Optional
  paths = ["scss/*"]
```


## Environment variables

- `LOOM_SITE_PREFIX`

  The prefix root to use for all links rendered on the generated site.
  This can be used if the site is being hosted under a sub-prefix.

  An example of this might be publishing the site for the current commit
  to `s3` to be reviewed as part of a pull request.

  Defaults to "/".


## Prerequisites

- `sassc`

  Currently [sassc](https://github.com/sass/sassc) is required to be on the `$PATH`.
  It can be installed with one of the following options:

  - For OSX users: `brew install sassc`
  - For Arch users: `pacman -S sassc`
  - Everyone else: https://github.com/sass/sassc#documentation
