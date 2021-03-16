# Version History

## 0.6.0 (Unreleased)

### New Features

  * Manual dependencies can now be added to targets.  This is useful
    if your target command uses a script or other file that, when
    changed, should trigger a rebuild.

    Use the `dependencies` key in the target definition to add
    dependencies:

    ```yml
    targets:
        - name: web
          format: html

          dependencies:
            - meta/style.html

          command:
            pandoc --from=markdown
                   --to=html
                   --include-in-header=%p/meta/style.html
                   --output=%o
                   %i
    ```

### Bug Fixes

  * Markdown link definitions can now have titles in single quotes or
    parentheses.  The URL in a link definition can also be wrapped in
    angle brackets.  This brings Edify inline with Pandoc for link
    definitions.

### Library Changes

  * A new module (`Edify.Compiler.Build`) has been added that provides
    an easy-to-use `convert` function to convert Edify-flavored
    Markdown to whatever Markdown variant you are using.

## 0.5.0 (March 2, 2021)

  * First documented version

### Minor Updates

  * 0.5.1 (March 5, 2021):

    In previous versions the target's command was run in the project's
    top level directory.  However, all asset (image) paths were
    relative to the output file meaning that some tools (i.e. pandoc)
    would not correctly resolve those paths.  So, starting in 0.5.1:

    - A target's command is run from the directory containing the file
      being generated (the `%o` command variable).

    - A new command variable (`%p`) was introduced so a target's
      command can refer to the project's top level directory if
      needed.

  * 0.5.2 (March 9, 2021)

    - Links using the Pandoc `shortcut_reference_links` extension are
      now properly written back into Markdown.

      In previous versions shortcut reference links would be converted
      to Markdown with superfluous empty square brackets followed by
      no white space.  Starting in this version shortcut links will be
      converted back to Markdown shortcut links (no brackets, correct
      amount of space).

    - Bracketed spans (Pandoc `bracketed_spans` extension) are no
      longer parsed as links.
