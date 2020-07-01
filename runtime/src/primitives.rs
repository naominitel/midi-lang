use std::collections::HashMap;
use bytecode::Value;

#[derive(Clone, Copy)]
pub struct PrimHandler {
    pub instance: fn() -> Vec<Value>,
    pub update: fn(instance: &mut Vec<Value>, args: Vec<Value>) -> Value,
}

pub mod open_out {
    use midir::MidiOutput;
    use bytecode::Value;
    use super::PrimHandler;

    fn find_device(midi_out: &MidiOutput, name: &str) -> usize {
        for i in 0 .. midi_out.port_count() {
            if midi_out.port_name(i).unwrap() == name { return i };
        }
        panic!("midi_out: device not found");
    }

    fn instance() -> Vec<Value> {
        vec![Value::Undef]
    }

    fn update(instance: &mut Vec<Value>, args: Vec<Value>) -> Value {
        match args.as_slice() {
            &[Value::Str(ref name)] => {
                match &instance[0] {
                    Value::Undef => {
                        let out = MidiOutput::new("runtime-client").unwrap();
                        let dev = find_device(&out, &name);
                        let conn = out.connect(dev, "runtime-output").unwrap();
                        let p = Box::into_raw(Box::new(conn)) as usize;
                        let v = Value::Primitive(p);
                        instance[0] = v.clone();
                        v
                    }

                    v => v.clone()
                }
            },
            &[_] => panic!("bad types for open_out"),
            _ => panic!("bad arity for open_out")
        }
    }

    pub static PRIM: PrimHandler = PrimHandler { instance, update };
}

mod output_poly {
    use midir::MidiOutputConnection;
    use bytecode::Value;
    use bytecode::Gate;
    use super::PrimHandler;

    fn instance() -> Vec<Value> {
        let mut state = Vec::with_capacity(128);
        for _ in 0 .. 128 {
            state.push(Value::Bool(false));
        }
        state
    }

    fn update(inst: &mut Vec<Value>, args: Vec<Value>) -> Value {
        match args.as_slice() {
            &[Value::Primitive(p), Value::Int(ch), Value::Poly(ref notes)] => {
                let ch = ch - 1;
                let midi_out = p as *mut MidiOutputConnection;
                let mut buf = [0u8;3];
                for &(pitch, gate, vel) in notes.iter() {
                    let last = match inst[pitch as usize] {
                        Value::Bool(b) => b,
                        _ => unreachable!()
                    };
                    match gate {
                        Gate::On => unsafe {
                            // note off
                            if last {
                                buf[0] = 0x80 | ch as u8;
                                buf[1] = (pitch & 0x7F) as u8;
                                buf[2] = 0x7Fu8;
                                midi_out.as_mut().unwrap().send(&buf).unwrap();
                            }
                            // note on
                            buf[0] = 0x90 | ch as u8;
                            buf[1] = (pitch & 0x7F) as u8;
                            buf[2] = (vel & 0x7F) as u8;
                            midi_out.as_mut().unwrap().send(&buf).unwrap();
                            inst[pitch as usize] = Value::Bool(true);
                        },

                        Gate::Off => unsafe {
                            if last {
                                buf[0] = 0x80 | ch as u8;
                                buf[1] = (pitch & 0x7F) as u8;
                                buf[2] = 0x7Fu8;
                                midi_out.as_mut().unwrap().send(&buf).unwrap();
                            }
                            inst[pitch as usize] = Value::Bool(false);
                        },

                        Gate::Tie => ()
                    };
                }
                Value::Undef
            }
            &[_, _, _] => panic!("bad types for output_mono"),
            _ => panic!("bad arity for output_mono")
        }
    }

    pub static PRIM: PrimHandler = PrimHandler { instance, update };
}
pub mod output_mono {
    use midir::MidiOutputConnection;
    use bytecode::Value;
    use bytecode::Gate;
    use super::PrimHandler;

    fn instance() -> Vec<Value> {
        vec![Value::Bool(false), Value::Int(0)]
    }

    fn update(inst: &mut Vec<Value>, args: Vec<Value>) -> Value {
        match args.as_slice() {
            &[Value::Primitive(p), Value::Int(ch), Value::Mono((pitch, gate, vel))] => {
                let ch = ch - 1;
                let midi_out = p as *mut MidiOutputConnection;
                let mut buf = [0u8;3];
                let last_pitch = match inst[1] { Value::Int(p) => p as u32, _ => unreachable!() };
                let last_on = match inst[0] { Value::Bool(b) => b, _ => unreachable!() };
                 unsafe {
                        match gate {
                            Gate::On => {
                                // note off
                                if last_on {
                                    buf[0] = 0x80 | ch as u8;
                                    buf[1] = (last_pitch & 0x7F) as u8;
                                    buf[2] = 0x7Fu8;
                                    midi_out.as_mut().unwrap().send(&buf).unwrap();
                                }
                                // note on
                                buf[0] = 0x90 | ch as u8;
                                buf[1] = (pitch & 0x7F) as u8;
                                buf[2] = (vel & 0x7F) as u8;
                                midi_out.as_mut().unwrap().send(&buf).unwrap();
                                inst[1] = Value::Int(pitch as i64);
                                inst[0] = Value::Bool(true);
                            }

                            Gate::Off => {
                                if last_on {
                                    buf[0] = 0x80 | ch as u8;
                                    buf[1] = (last_pitch & 0x7F) as u8;
                                    buf[2] = 0x7Fu8;
                                    midi_out.as_mut().unwrap().send(&buf).unwrap();
                                }
                                inst[0] = Value::Bool(false);
                            }

                            Gate::Tie => {
                                if last_on && (pitch != last_pitch) {
                                    // note on
                                    buf[0] = 0x90 | ch as u8;
                                    buf[1] = (pitch & 0x7F) as u8;
                                    buf[2] = (vel & 0x7F) as u8;
                                    midi_out.as_mut().unwrap().send(&buf).unwrap();
                                    // note off
                                    buf[0] = 0x80 | ch as u8;
                                    buf[1] = (last_pitch & 0x7F) as u8;
                                    buf[2] = 0x7Fu8;
                                    midi_out.as_mut().unwrap().send(&buf).unwrap();
                                    inst[1] = Value::Int(pitch as i64);
                                }
                            }
                        }
                        Value::Undef
                    }
            }
            &[_, _, _] => panic!("bad types for output_poly"),
            _ => panic!("bad arity for output_poly")
        }
    }

    pub static PRIM: PrimHandler = PrimHandler { instance, update };
}

static PRIMITIVES: [(&'static str, PrimHandler) ; 3] = [
    ("open_out", open_out::PRIM),
    ("output_mono", output_mono::PRIM),
    ("output_poly", output_poly::PRIM),
];

#[derive(Clone, Copy)] pub struct Primitive(usize);
#[derive(Clone)] pub struct Instance(Vec<Value>);
pub type Env = HashMap<&'static str, Primitive>;

pub fn env() -> Env {
    let mut env = HashMap::with_capacity(PRIMITIVES.len());
    for (i, &(name, _)) in PRIMITIVES.iter().enumerate() {
        env.insert(name, Primitive(i));
    }
    env
}

fn get(Primitive(i): Primitive) -> &'static PrimHandler {
    &PRIMITIVES[i].1
}

pub fn instance(prim: Primitive) -> Instance {
    Instance((get(prim).instance)())
}

pub fn update(prim: Primitive, instance: &mut Instance, args: Vec<Value>)
              -> Value {
    let Instance(instance) = instance;
    (get(prim).update)(instance, args)
}
