This is an alternative ocaml capnp implementation. It aims to be fully
compatible with capnp as and overcome the complexity and limitations of the
current implementations.

This is achieved in two ways:

Firstly, there will be uses of a gadt type system to describe the schema and accessors. This will drastically simplify the definitions.

Secondly, RPC will be defined using the two-party system.

Where there is a tradeoff between simplicity and performance, simplicity
will be preferred.

However, there will be at all times an awareness that capnp is a low-level protocol and to be used by those who know what they are doing.

The key to the design is that accessors are abstracted and the returned type representeed through a gadt. Variants return the gadt present.

Accessors contain enough information for regeneration of the source lines (i.e. without comments) of the capnp file. This is an important distinction between previous approaches and opens up new realms for testing.

A field contains all the information necessary to access the elements.

There are two types of memory access in capnproto which work in different ways

What do we need to find the offset:
  - the struct spec (concretely, n_words and n_ptrs)
    technically only needed for 
  - Perhaps chars make more sense after all
