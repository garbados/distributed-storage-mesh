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
They interact with by exchanging [capabilities](https://garbados.github.io/my-blog/conceptual-intro-to-spritely-goblins.html).
The DSM requires a few capabilities, which have expected parameters and outputs.
Specifically: the ability to encode content, and the ability to store blocks.

When a user saves content, the *content provider* chunks it into blocks
and then encrypts them with a *convergence key*. This key is used to affect
[convergent encryption](https://en.wikipedia.org/wiki/Convergent_encryption),
so that the same content with the same key will always encode to the same blocks.
This encoding process follows the [ERIS](https://eris.codeberg.page/) specification.

The blocks produced by the content provider
are then sent to the *storage provider*,
who persists them somehow.
Because the content is chunked,
and each chunk encrypted,
the storage provider cannot know what they are storing,
so they cannot snoop on user content
even as they hold it.

The encoding process process produces a *read capability*
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

This URN contains the convergence key and the reference
of the root block in the tree of blocks composing the content.
Thus, it should be treated as a sort of password:
if you have the readcap, you know how to find the content.

When you save content, you are granted a lease on it.
You can *release* this lease to indicate to tell the
providers of the content's blocks that you no longer
desire the content be held.
*This does not guarantee the blocks will be deleted!*
Because many parties may have leases on content,
all leases must be released before the content is deleted.
However, it's important to note that
storage providers are NOT guaranteed to hold blocks
even after they're saved.
They might be wiped after an expiry,
or for any reason at all.
The uptime of blocks is up to
the collective availability of an arbitrary *mesh* of block providers.

Consider a *proxy block provider* which syndicates reads and writes
to other block providers.
This proxy block provider can distribute read requests across providers
and then return the first that returns (or the second that agrees, etc);
it can distribute writes across providers too,
considering a write a "success" even if some providers failed to save.
This provides significant redundancy and parallelism,
while protecting users and storage providers alike.

## Install

You can build the project from source with [guix](https://guix.gnu.org/):

``` shell
$ git clone https://github.com/garbados/distributed-storage-mesh.git
$ cd distributed-storage-mesh
# see a demo of what's possible
$ guix shell -m manifest.scm -- guile demo.scm
```

You can also load the library to require in other files:

``` shell
$ guile -l dsm.scm your-file.scm
```

Then you can use the library and its functions:

``` scheme
(use-modules ((dsm)
              #:select (spawn-block-provider
                        spawn-proxy-block-provider
                        spawn-sqlite-block-provider
                        spawn-memory-block-provider
                        ^content-provider)))
```

## Usage

### spawn-block-provider

Parameters:
- `read-block` - `(read-block ref)`: given a reference to a block, returns that block.
  The reference and block are both bytevectors.
- `save-block` - `(save-block ref block)`: TODO
- `delete-block` - `(delete-block ref block)`: TODO
- `create-lease` - `TODO`: TODO
- `count-leases` - `TODO`: TODO

Given these functions, the following values are returned:

- `block-provider`: a [Goblins](https://gitlab.com/spritely/guile-goblins/-/blob/main/doc/goblins.org) object
  with two methods:
  - `(<- block-provider 'read-block ref)`: Returns a block by ref.
  - `(<- block-provider 'save-block ref block)`: Persists a block
    with the supplied `save-block` function.
- `revoked?`: a "cell" that can be flipped to a truthy value
  to disable the block provider. While the cell is truthy,
  the block provider's methods will throw `(error 'revoked)`.

### spawn-proxy-block-provider

Parameters:
- `add-provider` - `(add-provider sturdyref)`: TODO
- `remove-provider` - `(remove-provider sturdyref)`: TODO
- `map-providers` - `(map-providers (lambda (sturdyref) ...)`: TODO
- `providers`: a list of sturdyrefs to begin with. Defaults to an empty list.

Returns the same values as `spawn-block-provider`.

### spawn-sqlite-block-provider

TODO

### spawn-memory-block-provider

TODO

### ^content-provider

TODO

## Tests

You can run the test suite with the included runner:

``` shell
# install dependencies in an isolated
$ ./run-tests.sh
```

## License

I don't even know anymore.
