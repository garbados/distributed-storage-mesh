# Distributed Storage Mesh (DSM)

The DSM is a pattern for encrypted storage in a distributed system.
It is built with [Goblins](https://spritely.institute/goblins/),
a powerful distributed programming basis.

This library is written in [Guile](https://www.gnu.org/software/guile/),
a [Scheme](https://spritely.institute/static/papers/scheme-primer.html) dialect
-- a [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)).
However, it is intended as a reference implementation for a future standard
regarding [portable encrypted storage](https://spritely.institute/static/papers/spritely-core.html#portable-encrypted-storage).

## How It Works

Consider a distributed system where peers exchange and request information.
They interact by exchanging [capabilities](https://garbados.github.io/my-blog/conceptual-intro-to-spritely-goblins.html).
The DSM requires a few capabilities, which have expected parameters and outputs.
Specifically, four:

- The encoder: encodes content into blocks.
- The writer: persists blocks with a content-addressed identifier.
- The reader: fetches blocks with that identifier.
- The decoder: decodes blocks into content.

Content, in this case, is anything that can be [syrup-encoded](https://github.com/ocapn/syrup);
that is, most of the [primitives](https://github.com/ocapn/syrup#pseudo-specification)
that you already use.

When a user saves some immutable content, the *encoder* chunks it into blocks
and encrypts them with a *convergence key*. This key is used to perform
[convergent encryption](https://en.wikipedia.org/wiki/Convergent_encryption),
so the same content with the same key will always encode to the same blocks.
This is important for deduplication:
if multiple parties try saving the same information,
it should only be stored once.
Furthermore,
the parties storing data should not need to know what they are storing.

This encoding process follows the [ERIS](https://eris.codeberg.page/) specification.

The blocks produced by the encoder
are then sent to the writer,
who persists them somehow.
Because the content is chunked,
and each chunk is encrypted,
the writer cannot know what they are storing,
so they cannot snoop on user content
even as they hold it.

The encoding process produces a *read capability*
(also known as a "readcap")
which looks like this:

> urn:eris:BIAD77QDJMFAKZYH2DXBUZYAP3MXZ3DJZVFYQ5DFWC6T65WSFCU5S2IT4YZGJ7AC4SYQMP2DM2ANS2ZTCP3DJJIRV733CRAAHOSWIYZM3M

This URN can be interpreted to obtain the content:

``` scheme
(define-values (readcap release)
  ($ content-provider 'save-content "hello world"))
(pk ($ content-provider 'read-content readcap))
;;> "hello world"
```

This URN contains the information needed to find
the root block in the tree of blocks composing the content.
Thus, it should be treated as a sort of password or address:
if you have the readcap, you know how to find the content.

When you save content, you are granted a lease on it.
You can *release* this lease to tell the
providers of the content's blocks that you no longer
wish for the content to be held.
*Releasing content does not guarantee its blocks will be deleted!*
Because many parties may have leases on content,
all leases must be released before the content is deleted.
However, it's important to note that
storage providers are NOT guaranteed to hold blocks
even after they're saved.
They might be wiped after an expiry,
or for any reason at all.
The uptime of blocks is up to
the collective availability of an arbitrary *mesh* of block providers.

Consider a *proxy actor* which syndicates commands to other actors.
A proxy reader can distribute read requests across providers
and then return the first that responds (or the second that agrees, etc).
It can distribute writes too,
considering a write a "success" even if some providers failed to save.
This provides significant redundancy and parallelism,
akin to the service maps of production clustered databases.

The division of these roles reflect the dynamism of the trust they require:
you may have the right to read blocks, but not save them;
you may be able to interpret readcaps, but not save content.
The division of these capabilities reflects who is *doing the work*:
the machine serving blocks is expected to persist them somehow,
the machine reading blocks is expected to seek them out;
in turn those machines' owners have the right to only share
the right to call upon this capability
with those they trust sufficiently.
(Of course, this trust can always be [revoked](https://spritely.institute/static/papers/spritely-core.html#revocation-accountability).)

## Install

You can build the project from source with [guix](https://guix.gnu.org/):

``` shell
$ git clone https://github.com/garbados/distributed-storage-mesh.git
$ cd distributed-storage-mesh
# run the test suite
$ ./run-tests.sh
```

You can also load the library to require in other files:

``` shell
# TODO this deserves a better story
$ guile -l utils.scm -l dsm.scm your-file.scm
```

Then you can use the library and its functions:

``` scheme
;; TODO this is outdated
(use-modules ((goblins)
              #:select (spawn-vat define-vat-run $))
             ((dsm)
              #:select (spawn-sqlite-block-provider
                        spawn-sync-content-provider)))

(define a-vat (spawn-vat))
(define-vat-run a-run a-vat)

(define-values
  (block-provider revoked?)
  (a-run (spawn-sqlite-block-provider "dsm.db")))
(define content-provider
  (a-run (spawn-sync-content-provider block-provider)))

(pk (a-run ($ content-provider 'save-content "hello world")))
;;> (list readcap release)
```

## Usage

TODO this is outdated

### spawn-block-provider

Given methods for interacting with a persistence layer, returns a block provider
and a cell that can be used to revoke access to the provider.

Parameters:
- `save-block` - `(save-block ref block)`: given a reference to a block
  and the block itself, return a `release` function which clears the
  lease on the block. When all leases have been cleared, the block
  is deleted.
- `read-block` - `(read-block ref)`: given a reference to a block, returns that block.
  The reference and block are both bytevectors.
- `delete-block` - `(delete-block ref)`: given a reference to a block,
  remove the block from storage.
- `create-lease` - `(create-lease ref)`: given a reference to a block, return a
  `release` function. As an example, this function might increment a ref counter
  and return a function that decrements it. The `release` function will only
  be run once if ever.
- `count-leases` - `(count-leases ref)`: given a reference to a block, return
  the number of open leases as an integer.

Given these functions, the following values are returned:

- `block-provider`: a Goblins object with two methods:
  - `($/<- block-provider 'read-block ref)`: Returns a block by ref
    using the supplied `read-block` function.
  - `($/<- block-provider 'save-block ref block)`: Persists a block
    with the supplied `save-block` function.
- `revoked?`: a "cell" that can be flipped to a truthy value
  to disable the block provider. While the cell is truthy,
  the block provider's methods will throw `(error 'revoked)`.
  - to flip the cell: `($/<- revoked? #t)`

*(`$/<-` refers to the `$` and `<-` operators, which run an object method)*

### spawn-proxy-block-provider

TODO not yet implemented

### spawn-sqlite-block-provider

Creates a block provider backed by a SQLite database.

Parameters:
- `db-path`: The path to the file that will be used as a SQLite database,
  as a string.
- (optional: `clear?`: a boolean that defaults to false. When truthy,
  the file at `db-path` will be wiped and the necessary tables reinitialized)

Returns the same values as `spawn-block-provider`.

### spawn-memory-block-provider

Creates a block provider backed by in-memory hash tables
made with `make-hash-table`.

No parameters. Returns the same values as `spawn-block-provider`.

### ^content-provider

Given methods for persisting blocks, return a content provider.

Usually these methods are provided through a block provider,
such as with `spawn-async-content-provider`
and `spawn-sync-content-provider`.
Thus this is intended as raw access --
the ability to implement a content provider
without a block provider.

As it is a Goblins object, it must be invoked with `spawn`:

``` scheme
(spawn ^content-provider save-block read-block process-results)
```

Parameters:
- `save-block` - `(save-block ref block)`: given a reference to a block
  and the block itself.
  This is done for each block produced by the encoding;
  the results of all these saves are sent to `process-results`.
- `read-block` - `(read-block ref)`: given a reference to a block, returns that block.
  The reference and block are both bytevectors.
- `process-results` - `(process-results results)`: given a list of the results
  of saving each block produced while encoding some content, return a list of
  `release` functions. These functions are then bundled into the `release`
  function given to the caller once `save-content` returns.

Returns a Goblins object with the following methods:

- `($/<- content-provider 'save-content content #:optional key block-size)`:
  - Given content to save, chunk it into blocks and persist them.
  - Optionally, a convergence key can be provided as a 32-byte vector.
    By default, a vector of random bytes is used.
  - A block size can also be specified,
    as either the symbol `'small` or `'large`.
    `'small` produces 1KiB (1024 bytes) blocks,
    `'large` produces 32KiB (32768 bytes) blocks.
    This defaults to `'small`,
    but you should set it to `'large` if you expect content to be
    larger than 16KiB.

*(`$/<-` refers to the `$` and `<-` operators, which run an object method)*

### spawn-async-content-provider

Given a block provider, returns a content provider:

``` scheme
(spawn-async-content-provider block-provider)
```

The block provider is expected to be running on a different machine (or "vat")
than the content provider.

Returns the same value as `(spawn ^content-provider ...)`.

### spawn-sync-content-provider

Given a block provider, returns a content provider:

``` scheme
(spawn-sync-content-provider block-provider)
```

The block provider is expected to be running on the same machine (or "vat")
as the content provider.

Returns the same value as `(spawn ^content-provider ...)`.

## Tests

You can run the test suite with the included runner:

``` shell
# install dependencies in an isolated
$ ./run-tests.sh
```

## License

I don't even know anymore.
