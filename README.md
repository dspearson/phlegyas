# Plan 9 Filesystem Protocol, as implemented in Clojure.

```clj
[phlegyas "0.1.4-SNAPSHOT"]
```

*WARNING: DRAGONS LIE AHEAD! THIS IS WOEFULLY INCOMPLETE. USE AT YOUR OWN PERIL!*

The vast majority of the protocol-level documentation was sourced from the wonderful [Plan 9 from User Space](https://9fans.github.io/plan9port/man/man9/) project.

I have copied the test resources from [droyo's styx package](https://github.com/droyo/styx/), credit due for making it available.

Run `lein test` to verify things work as they should. Currently, 100% of the provided framedumps are successfully handled, hopefully indicating that this is fully up to spec.

"LISP programmers know the value of everything and the cost of nothing." Thus, I have not measured performance of the encode/decode in any serious manner, and the example state machine is a dumb single loop, likely unsuitable for any serious use. However, the principles of how to piece things together should be evident, and the design entirely customisable.

Note the field names in `types.clj`. The `assemble-packet` function will take a map of these and create a byte-array for you. `disassemble-packet` will do the reverse.

### Development Notes

There are still many functions that require implementation, not least the VFS layer. Consider it unstable and subject to major changes.

I have included a built-in TCP server in order to aid this development, accessible from the phlegyas.core namespace.

Jack in with Spacemacs/CIDER with `,'` and then, at the REPL, `(r)`

This will start a server at localhost on port 10001.

For testing, build [plan9port](https://9fans.github.io/plan9port/) (will require compilers & development headers):

`git clone https://github.com/9fans/plan9port.git && cd plan9port && ./INSTALL`

Or, alternatively, if you have _Docker_ installed, [there is a project](https://github.com/dspearson/plan9port-docker) that will do this for you inside a container, so you don't have to touch your base system.

Then, run the built 9P FUSE client, e.g.:

`9pfuse -D 'tcp!localhost!10001' mountpoint`

This should aid in the development cycle.

The example VFS layer will create a single filesystem for attaching, and some example files within, with both dynamic and static content.

When hitting inevitable issues, a simple call to `(r)` again will reset the service back to a clean state, ready to continue on your adventures.

### Example Application

I have created a [small bit of standalone code](https://github.com/dspearson/phlegyas-example) to illustrate how to use this library in your applications.
