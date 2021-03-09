# Version History

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
