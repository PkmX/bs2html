# bs2html

Convert [telnet://bs2.to](telnet://bs2.to) backups to a static website.

[Demo](https://pkmx.github.io/bs2html/) (converted from `testbrd`)

## Installation

You need [Haskell (ghc)](https://www.haskell.org/ghc/) and [Stack](http://www.haskellstack.org), both of which can be found on most distribution's repostiories. After that, simply run:

```
$ stack install
```

This will download and compile all dependencies as well as the program, which may take a while. The resulting binary will be installed to `~/.local/bin/bs2html`.

Alternatively, you may download generic `x86-64` Linux binaries from GitHub's releases page.

## Usage

```
$ bs2html --help
Usage: bs2html INPUT OUTPUT [--show-hidden] [--debug]
  Convert bs2.to backups to a static website

Available options:
  -h,--help                Show this help text
  INPUT                    Directory to bs2.to backup
  OUTPUT                   Output directory (created if non-existent)
  --show-hidden            Include hidden posts
  --debug                  Print debug information
```

### Example

Download a `.tar.gz` backup from [http://www.bs2.to](http://www.bs2.to) and extract it to the current directory (for testing purporses, you may use `testbrd` in the repository):

```
$ tar xf P_foo.tar.gz
$ tree -a
.
└── brd
    └── P_foo
        ├── @
        ├── 0
        ...
        ├── .DIR
        ...
```

Convert to directory `out`:

```
$ bs2html brd/P_foo out
Parsing .DIR ... done
Converting articles to HTML [=======================>] 2273/2273
Generating index.html
```

This generates the website under the directory `out`, which you may view with:

```
$ ${BROWSER} out/index.html
```

Alternatively, you may host the generated static files with any HTTP servers of your choice.

## Notes

The utility by default does not convert restricted articles (隱文) in order to protect your privacy. Specify `--show-hidden` to force generation of restricted posts.

Performance-wise, currently it can convert a moderately-sized board with 2273 articles (~11MB text) in about 3 seconds.

## TODO

* Implement search/filter functions on the index page.
* Thread-level navigation within article (`[` and `]`).
* Generate a fully local website without depending on CDN (Vaadin's CDN is slow).
* Factor out common article `<style>`'s to a separate CSS stylesheet.
* Optimize generated CSS/JS.
