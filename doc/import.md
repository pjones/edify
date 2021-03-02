# Markdown Importing

As a documentation project grows it's common to break it into several
Markdown files.  However, this comes with the burden of telling our
tools about all of the file and the order to process them.

Edify solves this problem by supporting *import syntax* so that one
Markdown file can import another.

## Using the Import Syntax

To import the `hello.md` file into the current Markdown file use the
following syntax:

```markdown
<<(hello.md)
```

## Narrowing Markdown

Sometimes we want to import only a subset of a Markdown file.  We can
do this in Edify by supplying the ID of a heading to import.  The ID
is separated from the file name using an octothorp ("`#`").

For example, let's say that `hello.md` looks like this:

```markdown
# Hello

Hi there.

## World {#world}

Hello World!

### Me Also

Don't forget me!
```

To import just the "World" heading and all subheadings the following
syntax is used:

```markdown
<<(hello.md#world)
```
