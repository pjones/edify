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
