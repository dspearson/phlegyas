# Plan 9 Filesystem Protocol, as implemented in Clojure.

```clj
[phlegyas "0.1.2"]
```

The vast majority of the protocol-level documentation was sourced from the wonderful [Plan 9 from User Space](https://9fans.github.io/plan9port/man/man9/) project.

This release solely covers byte-array encoding/decoding.

I have included the test resources from [droyo's styx package](https://github.com/droyo/styx/).  Run `lein test` to verify things work as they should. Currently, 100% of the provided framedumps are successfully handled, hopefully indicating that this is up to spec.

## Usage

Keys for the frame encoding can be found in the `phlegyas.types` namespace. Check the `frame-layouts` map. There are only a few special cases, namely:
* `:Twrite` and `:Rread` frames, where the `count[4]` is automatically calculated.
* `:Twalk`, where `nwname[2]` is automatically calculated and `:wnames` should be a vector of strings.
* `:Rwalk`, where `nwqid[2]` is automatically calculated and `:nqwids` should be a vector of `{:qid-type qid.type[1] :qid-vers qid.vers[4] :qid-path qid.path[8]}` maps.
* The `qid.type[1]`, `qid.vers[4]`, `qid.path[8]` fields are named with dashes rather than dots, to make the buffer operator functions easier to resolve.

Encoding and decoding, as done via the REPL:

```
phlegyas.core=> (vec (assemble-packet {:frame :Tversion :tag 0 :msize 8192 :version "9P2000"}))
[19 0 0 0 100 0 0 0 32 0 0 6 0 57 80 50 48 48 48]

phlegyas.core=> (disassemble-packet (byte-array [19 0 0 0 100 0 0 0 32 0 0 6 0 57 80 50 48 48 48]))
{:frame :Tversion, :tag 0, :msize 8192, :version "9P2000"}
```
