# Plan 9 Filesystem Protocol, as implemented in Clojure.

```clj
[phlegyas "0.0.1-SNAPSHOT"]
```

*WARNING: DRAGONS LIE AHEAD! THIS IS WOEFULLY INCOMPLETE. USE AT YOUR OWN PERIL!*

The vast majority of the protocol-level documentation was sourced from the wonderful [Plan 9 from User Space project](https://9fans.github.io/plan9port/man/man9/).

Development Notes:

There are still many functions that require implementation, not least the VFS layer, and the basic protocol handling.

I have included a built-in TCP server in order to aid this development, accessible from the phlegyas.core namespace.

Jack in with Spacemacs/CIDER with `,'` and then, at the REPL, `(r)`

This will start a server at localhost on port 10001.

For testing:

`git clone https://github.com/9fans/plan9port.git && cd plan9port && ./INSTALL`

Then run the built 9P FUSE client:

`9pfuse -D 'tcp!localhost!10001' mount-point-goes-here`

This should aid in the development cycle.

When hitting inevitable issues, a simple call to `(r)` again will reset the service back to a clean state, ready to continue on your adventures.
