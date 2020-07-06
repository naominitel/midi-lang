extern crate clap;
extern crate log;
extern crate midir;
extern crate num_derive;
extern crate num_traits;

use std::io;
use std::net::{TcpListener};
use std::sync::{Arc, Mutex};
use log::debug;

use midir::{MidiInput, MidiOutput};

mod bytecode;
mod engine;
mod handler;
mod interp;
mod message;
mod node;
mod primitives;
mod runtime;

#[derive(Debug)]
enum RuntimeError {
    MidiInitError(midir::InitError),
    MidiPortInfoError(midir::PortInfoError),
    MidiConnectError(midir::ConnectError<MidiInput>),
    IoError(io::Error),
    UserError(String)
}

type Result<T> = ::std::result::Result<T, RuntimeError>;

fn err(msg: String) -> RuntimeError {
    RuntimeError::UserError(msg)
}

fn list_devs() -> Result<()> {
    use RuntimeError::*;
    let inp = MidiInput::new("runtime-client").map_err(|e| MidiInitError(e))?;
    let out = MidiOutput::new("runtime-client").map_err(|e| MidiInitError(e))?;

    println!("Available input devices:");
    for id in 0 .. inp.port_count() {
        let name = inp.port_name(id).map_err(|e| MidiPortInfoError(e))?;
        println!("    {}: {}", id, name);
    }

    println!("Available output devices:");
    for id in 0 .. out.port_count() {
        let name = out.port_name(id).map_err(|e| MidiPortInfoError(e))?;
        println!("    {}: {}", id, name);
    }

    Ok(())
}

fn main_run() -> Result<()> {
    use clap::Arg;

    let clap = clap::App::new("myapp")
        .version("0.1").author("Naomi Nitel <naominitel@gmail.com>")
        .about("runtime program for unnamed_lang")
        .arg(Arg::with_name("LIST_DEV")
             .short("L").long("list-devices")
             .help("List available MIDI devices and exits"))
        .arg(Arg::with_name("CLK")
             .short("c").long("clock")
             .takes_value(true)
             .help("Sets MIDI clock source: `int` (default) or `midi`"))
        .arg(Arg::with_name("TEMPO")
             .short("t").long("tempo")
             .takes_value(true)
             .help("Sets initial tempo, in BPM (when clock is `int`)"))
        .arg(Arg::with_name("IN_DEV")
             .short("i").long("input-dev")
             .takes_value(true)
             .help("Sets the MIDI input device (when click is `midi`)"))
        .arg(Arg::with_name("ADDR")
             .short("b").long("bind-addr")
             .takes_value(true)
             .help("Listening address (defaults to 127.0.0.1)"))
        .arg(Arg::with_name("PORT").short("p").long("port").takes_value(true)
             .help("Listening address (defaults to 12000)"));

    let opts = clap.get_matches();

    if opts.is_present("LIST_DEV") {
        return list_devs();
    }

    let bind_addr = opts.value_of("ADDR").unwrap_or("localhost");
    let bind_port = opts.value_of("PORT").unwrap_or("12000").parse::<u16>()
        .map_err(|_| err(format!("bad port")))?;

    if let Some("midi") = opts.value_of("CLK") {
        let dev_id = opts.value_of("IN_DEV")
            .ok_or(err("must specify input-dev when using MIDI clock".into()))?
            .parse::<usize>()
            .map_err(|_| err(format!("input-dev must be an integer")))?;

        let mut inp = MidiInput::new("runtime-client")
            .map_err(|e| RuntimeError::MidiInitError(e))?;
        inp.ignore(midir::Ignore::None);

        let name = inp.port_name(dev_id)
            .map_err(|e| RuntimeError::MidiPortInfoError(e))?;
        debug!("listening for MIDI input on {}", name);

        let program = runtime::load_program();
        let runtime = Arc::new(Mutex::new(runtime::Runtime::new(program)));
        let mut engine = engine::Engine::new();
        let handler = handler::MidiHandler::new(runtime.clone());

        // handler moved into the callback closure and into the midi thread
        let conn = inp.connect(dev_id, "runtime-port", move |_, data, _| {
            handler.handle_message(data[0], &data[1..])
        }, ()).map_err(|e| RuntimeError::MidiConnectError(e))?;

        println!("listening on {}:{}", bind_addr, bind_port);
        let listener = TcpListener::bind((bind_addr, bind_port)).unwrap();


        loop {
            let (mut cli, _) = listener.accept().unwrap();
            println!("client connected from {}", cli.peer_addr().unwrap());

            loop {
                use message::Message;
                let msg = match Message::read(&mut cli) {
                    Ok(msg) => msg,
                    Err(io_err) => match io_err.kind() {
                        io::ErrorKind::UnexpectedEof => {
                            debug!("client closed connection");
                            break;
                        }
                        _ => return Err(RuntimeError::IoError(io_err))
                    }
                };

                println!("message received");
                match msg {
                    Message::Update(update_msg) => {
                        let node_def = bytecode::NodeDef::parse(update_msg)
                            .map_err(|e| RuntimeError::IoError(e))?;

                        node_def.dump();
                        let inst = engine.update_and_inst(node_def);
                        let mut runtime = runtime.lock().unwrap();
                        runtime.send_pattern(0, runtime::Pattern {
                            len: 96,
                            code: inst
                        })
                    }
                }
            }
        }
    }

    Ok(())
}

fn main() {
    match main_run() {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{:?}", e);
            ::std::process::exit(1);
        }
    }
}
