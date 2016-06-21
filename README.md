
### Synopsis

    cd 7.10.x
    stack build
    ../install-haddock --from-stack-work

This will build the modified version of haddock for GHC-7.10.x
and install it in your ~/.stack directory so that stack will
use it.

Note: The specific version of GHC used depends on the resolver
setting in the file `7.10.x/stack.yaml`.

For GHC-8.0.1, use the source in the `8.0.x` directory.

### Introduction

This repo contains experiments with haddock. Specifically it
contains modified versions of haddock which can emit type span
and import origin information for enhancing the hyperlinked-source
pages.

### Using a custom haddock with stack

There doesn't seem to be a direct way to tell stack to use
a custom haddock binary. When producing documention
stack always looks for haddock
in the appropriate `~/.stack/programs/.../bin` directory
for the resolver being used.  The program `install-haddock`
will change which haddock binary stack uses for a particular
version of ghc. It may also be used to revert back to the
stack-supplied version of haddock.

To change the haddock binary used for version of ghc, use:

    $ install-haddock --ghc 8.0.1 --haddock /path/to/custom/haddock 

To revert the change:

    $ install-haddock --ghc 8.0.1 --revert

### Redirecting to a wrapper

Instead of having stack call haddock directly, it turns out to be useful
to intercept the calls with a wrapper. The wrapper can be used to
log calls, alter arguments and even avoid calling haddock for certain
phases of documentation generation.

To install a wrapper to haddock, use:

    $ install-haddock --ghc 8.0.1 --haddock /path/to/custom/haddock --wrapper /path/to/wrapper

The path to the custom haddock binary will be passed as the first command line
argument to the wrapper.

For an example of a wrapper script, have a look at the file `wrap-haddock` in
the repository.

### @-arguments

Both stack and cabal can pass a lot of command line arguments to haddock.
To shorten command lines haddock interprets an argument of the form `@...`
to mean to read additional arguments from the file `...`.

### haddock data files

If you compile haddock with stack, the auxiallary html and css data files
used for the docs will be located in the `.stack-work` directory, and this
path will be hard-coded into the haddock binary.

If you relocate these files, use the `-l...` option to specify the
new location of the files.

### stack <-> haddock interaction

During the course of building documentation for a module
`stack` will typically call haddock several times:

1. Initially with the args `--version` to query the version

2. With `--hyperlinked-source` but no other other arguments to
determine if haddock supports producing hyperlinked source.

3. With `--ghc-version` to determine the GHC version haddock
was built with.

4. With all the arguments to really build a module's documentation.

5. With `--gen-index` and `--gen-contents` to build the contents and index pages for just the module.

6. Once to rebuild the master contents and index pages for the documentation directory.

