# Plan 9 Filesystem Protocol, as implemented in Clojure.

[![Build Status](https://travis-ci.org/dspearson/phlegyas.svg?branch=master)](https://travis-ci.org/dspearson/phlegyas)

```clj
[phlegyas "master-SNAPSHOT"]
```

The vast majority of the protocol-level documentation was sourced from the wonderful [Plan 9 from User Space](https://9fans.github.io/plan9port/man/man9/) project.

I have copied the test resources from [droyo's styx package](https://github.com/droyo/styx/), credit due for making it available.

Run `lein test` to verify things work as they should. Currently, 100% of the provided framedumps are successfully handled, hopefully indicating that this is fully up to spec.

Note the field names in `types.clj`. The `assemble-packet` function will take a map of these and create a byte-array for you. `disassemble-packet` will do the reverse.

### Notes

An example server is available (see the `phlegyas.server` namespace).

Client routines are also available (see the `phlegyas.client` namespace).

For testing, build [plan9port](https://9fans.github.io/plan9port/) (will require compilers & development headers):

`git clone https://github.com/9fans/plan9port.git && cd plan9port && ./INSTALL`

Or, alternatively, if you have _Docker_ installed, [there is a project](https://github.com/dspearson/plan9port-docker) that will do this for you inside a container, so you don't have to touch your base system.

Then, run the built 9P FUSE client, e.g.:

`9pfuse -D 'tcp!localhost!10001' mountpoint`

The example VFS layer will create a single filesystem for attaching, and some example files within, with both dynamic and static content (see the `phlegyas.vfs` namespace).

### TODO

Authentication is not yet supported. The example VFS is rudimentary and leaves room for improvement.
