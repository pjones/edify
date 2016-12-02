# Edify -- Custom Extensions to Markdown and Pandoc

Edify is a command-line tool and Haskell library for working with
Markdown documents.  I ([Peter J. Jones][pjones]) use it for all of my
writing and teaching (published and unpublished books, course
curriculum, blog articles, etc.)

## Available Commands

  * `filter`: A Pandoc filter that provides custom extensions to Markdown.
  * `outline`: Produces an outline of a Markdown document.
  * `stitch`: Output a complete Markdown file from a manifest file.

## Extensions to Markdown

Using the `filter` subcommand to `edify` you can process Markdown
files that use Edify's custom extensions to Markdown.

The `filter` subcommand is meant to be used as a Pandoc JSON filter.
For example:

```
$ cat filter.sh
#!/bin/sh -eu
edify filter

$ pandoc -F ./filter.sh README.md
```

Unfortunately, Pandoc is a bit stupid and won't let you pass arguments
to the filter.  Therefore you have to use a script in order to run
`edify`.

### Inserting Source Code from an External File

Stop writing snippets of source code in your Markdown files!  Instead,
pull source code in from an external file that has been tested and
known to work!  No more syntax errors in printed books!

#### Basic Syntax

To insert an external file into your Markdown use a normal fenced code
block and specify the file name using the `insert` key:

`````
```{.haskell insert="/path/to/file.hs"}
```
`````

The body of the code block will be updated so that it contains the
contents of the source file.

#### Limiting Insertion to a Subset of Code

Sometimes you don't want to include an entire file.  In that case you
can limit the insertion command using tokens.  Wrap the part of code
you want to insert in special token delimiters and then tell Edify the
name of the token.  For example:

```javascript
function someFunction () {
  var a = 1;

  // <<: arrow
  var f = e => console.log(e);
  // :>>

  var b = 2;
}
```

You can see that the delimiters are `<<:` and `:>>` and can be placed
behind comment markers.  The opening delimiter should be followed by a
name, in this case `arrow`.  The code between the opening and closing
delimiters will have a level of indentation removed. The amount of
indentation before the opening delimiter will be used as the amount of
indentation to remove from the code snippet.

To limit the `insert` filter, add the `token` key:

`````
```{.javascript insert="/path/to/file.js" token="arrow"}
```
`````

After filtering the Markdown will contain:

`````
```{.javascript insert="/path/to/file.js" token="arrow"}
var f = e => console.log(e);
```
`````

### Running a Command and Capturing Standard Output

Want to demonstrate how to read the output of a command, but want to
capture the output every time you build your Markdown?  Simple, use a
fenced code block and the `exec` key.

`````
```{exec="edify --help"}
```
`````

The body of the code block will be replaced with the standard output
of the given command (which is run with `/bin/sh -c`).

### Promoting and Removing `<div>` Tags

Pandoc allows you to use HTML directly in your Markdown.  This can be
very useful for (e.g. notes that should be in a handout but not in
slides).

Say you have something like this in your Markdown:

```
<div class="notes">
  * These should be in the handout
  * And not in the slides
</div>
```

Edify can "promote" the `<div>` by removing the class name:

```
$ edify filter --promote notes
```

Or remove the div so it doesn't appear in the output Markdown:

```
$ edify filter --remove notes
```

<!-- ====================================================================== -->
<!-- Links:                                                                 -->
<!-- ====================================================================== -->
[pjones]: http://www.devalot.com/about/pjones.html
