# Image Link Rewriting

Good technical documentation usually includes diagrams.  Edify allows
us to link to the source diagram (for example, Graphviz `dot` files)
and have those links rewritten to the appropriately generated output
diagram (PDF or PNG depending on the target document).

## Compiling Diagrams

Edify can process a range of documents and produce the appropriate
output document depending on the current target.  If the target is
HTML then PNG files will be generated, otherwise PDF will be used.

 | File Extension | Tool     |
 | -------------- | -------- |
 | `dot`          | Graphviz |
 | `svg`          | Inkscape |
 | `tex`          | TeX Live |
 | `msc`          | mscgen   |

## Adding Additional Diagram Tools

Edify does not yet allow us to configure additional diagram tools.
This feature will be included in a future version.
