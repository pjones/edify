# Executing Shell Commands

Edify can execute (approved) shell commands and capture their output.
The output can then be inserted back into the Markdown document.

## Using the `exec` Attribute

Let's say we want to execute a command and display the output in a
fenced code block.  The `exec` attribute tells Edify to run a shell
command and replace the current fence block with its output:

````markdown
``` {exec="cat /etc/issue"}
```
````

Becomes:

````markdown
``` {execed="cat /etc/issue"}
<<< Welcome to NixOS 20.09pre-git (\m) - \l >>>

Run 'nixos-help' for the NixOS manual.
```
````

## Approving Commands

Edify will refuse to execute shell commands that have not been
approved.

To approve shell commands start by using the `edify audit` command to
audit the document.  Performing an audit allows us to see all of the
shell commands that could be executed and that aren't already
approved.

Once you are satisfied that there are no malicious commands embedded
in the Markdown document the `edify allow` command can be used to mark
all commands as approved for running.

## Implicit Trust During Authoring

While authoring a project it's common to regularly edit `exec`
attributes.  Doing so will invalidate their prior approvals and
require us to continually audit and approve them.

If you are certain that a document is safe, you can use the
`--unsafe-allow-commands` command line option to the `edify build`
command to disable the requirement that commands are approved prior to
execution.
