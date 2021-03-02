# File Insertion and Narrowing

Edify can insert any file into the current document using fenced
blocks.  This includes fenced `div` blocks as well as fenced code
blocks.

Inserting external source code is a great way to include example code
in our documentation.  The code can be tested and verified externally
and only inserted after we know it's valid, working code.

## Inserting Files with the `insert` Attribute

File insertion is triggered by the `insert` attribute using Pandoc
style block attributes.  For example, here's a fenced code block that
inserts `src/hello.c` into the document:

````markdown
``` {.c insert="src/hello.c"}
```
````

And here's a `div` that inserts another Markdown file:

```markdown
::: {insert="docs/hello.md"}
:::
```

**NOTE:** Using [Markdown importing](import.md) is the preferred way to insert
Markdown into the current document.

## Narrowing and the `token` Attribute

Quite often we only want to insert a portion of a file, and not the
entire thing.  This is especially true for source code.

Inserting a portion of a file is called *narrowing* in Edify.  To
narrow a file you mark it up with *narrow markers* and then use the
`token` attribute to tell Edify to narrow the file.

Here's an example source file that we want to insert into the
document:

```javascript
let makeCounter = function(startingValue) {
  let n = startingValue;

  // <<: closure
  return function() {
    return n += 1;
  };
  // :>>
};
```

The narrowing markers ("`<<:`" and "`:>>`") are placed inside source
code comments around the text we want to extract.  In the Markdown you
can use a fenced code block with the `insert` and `token` attributes
to trigger insertion and narrowing:

````markdown
``` {.javascript insert="src/counter.js" token="closure"
```
````

And Edify will produce:

````markdown
``` {.javascript inserted="src/counter.js"}
return function() {
  return n += 1;
};
```
````
