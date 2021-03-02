# Edify: Markdown for Technical Authors

Edify is a Markdown processor/compiler with features designed for
authors writing technical documentation, articles, books, etc.

## Markdown Extensions

Edify extends Markdown in the following ways:

  * [Insert source code][insert] from external files with narrowing

  * [Execute shell commands][exec] and insert output into the final document

  * [Import Markdown files][import] (or just a portion of a file) anywhere

  * [Rewrite image links][image] appropriate to the output format

## Getting Started

Install Edify and start your first project!

### Installing Edify

If you already have an existing Markdown tool chain installed
(e.g. Pandoc, Tex Live, etc.) you can download a pre-compiled
executable from the [release][] page.

For the rest of us I recommend installing [Nix][] and installing all
of the required dependencies in a single step:

```
$ nix-env -iA bin -f https://github.com/pjones/edify/archive/v0.5.tar.gz
```

### Building a Markdown Project

Here's a quick way to get started:

  1. Ask Edify to generate a project configuration file:

     ```
     $ edify generate --project
     ```

  2. Review the generated "`.edify.yml`" file and change it as
     necessary.

  3. Build all targets:

     ```
     $ edify build
     ```

[insert]: doc/insert.md
[exec]: doc/exec.md
[import]: doc/import.md
[image]: doc/image.md
[release]: https://github.com/pjones/edify/releases
[nix]: https://nixos.org/download.html
