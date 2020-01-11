## A MIDI Processing Language

> TODO: give it a name <3

Synchronous dataflow programming language (àlà Lustre) designed for live-coding
and easy manipulation of MIDI data. Intended to be used to program musical
patterns in live performances.

Currently contains:
* A compiler written in ML that compiles a program to a binary format
* A runtime engine written in Rust that listens to incoming transmissions of
  compiled programs via TCP and runs them

### Usage :

1. Compile everything:

```
cd compiler
ocamlbuild Main.native

cd ../runtime
cargo build
```

2. Compile a program:

```
./compiler/Main.native examples/example1 > test.bin
```

3. Start runtime:

```
cd runtime/
cargo run -- -c midi --input-dev 0
```

> `-c midi` will make the runtime tick according to the MIDI clock events
> received on the source device specified with `--input-dev`. Internal clock is
> not yet implemented. `--list-devices` will give an overview of available MIDI
> source devices.

> The runtime will listen by default on `127.0.0.1` on port `12000`. Change with
> `--bind-addr`/`-b` and `--port`/`-p`.

4. Run compiled program:

```
nc localhost 12000 < test.bin
```

### Emitting MIDI

The `open_out` and `output_poly` and `output_mono` primitives can be used in a
program node to emit MIDI to an output device:

```
node main () () = let
    out = open_out("IAC Driver Bus 1");

    _ = output_poly(out, 10, drums());
    x = output_mono(out, 2, massive());
    y = output_mono(out, 3, acid());
tel
```

Read the documentation on the [representation of MIDI signals](doc/midi.md) for
more info.

> On macOS, enable the IAC Driver in Audio MIDI Setup to send MIDI across apps.
