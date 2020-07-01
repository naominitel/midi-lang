use node::NodeInstance;
use bytecode;
use std::sync::Arc;

#[derive(Clone)]

pub struct Pattern {
    pub len: usize,
    pub code: NodeInstance
}

struct Table<T>(Vec<T>);
#[derive(Clone, Copy)]
struct Var<T>(usize, ::std::marker::PhantomData<T>);

use std::ops::Index;
impl<T> Index<Var<T>> for Table<T> {
    type Output = T;
    fn index(&self, at: Var<T>) -> &T {
        let Table(tbl) = self;
        let Var(var, _) = at;
        &tbl[var]
    }
}

type Track = Vec<Pattern>;

pub struct Program {
    tracks: Vec<Track>,
    scenes: Vec<Scene>
}

pub struct Runtime {
    patterns: Table<Pattern>,

    program: Program,
    scene: Scene,

    // FIXME dummy
    out_chan: bytecode::Value,
    output_mono: Vec<bytecode::Value>,
}

type Scene = Vec<Canal>;

struct Canal {
    progression: Box<Progression + ::std::marker::Send>,
    // scenes
    counter: usize,
    // ticks
    pc: usize,
    loop_end: usize,
    loop_begin: usize
}

trait Progression {
    fn at(&mut self, index: usize) -> &mut Pattern;
    fn len(&self) -> Option<usize>;
}

impl Progression for Vec<Pattern> {
    fn at(&mut self, index: usize) -> &mut Pattern {
        &mut self[index]
    }

    fn len(&self) -> Option<usize> {
        Some(self.len())
    }
}

// a detached pattern
impl Progression for Pattern {
    fn at(&mut self, _: usize) -> &mut Pattern {
        self
    }

    fn len(&self) -> Option<usize> {
        None
    }
}

fn play(patterns: &Scene) {
    // ??
}

impl Runtime {
    // tick one instant
    pub fn step(&mut self) {
        // FIXME: dummy
        use primitives;
        use bytecode::Value;

        for chan in self.scene.iter_mut() {
            match chan.progression.len() {
                Some(len) if len <= chan.counter => continue,
                _ => ()
            }

            if chan.pc == chan.progression.at(chan.counter).len {
                // next scene
                chan.counter += 1;
                chan.pc = 0;
                // TODO: loops
                if chan.counter == chan.loop_end {
                    chan.counter = chan.loop_begin;
                }
            }

            // FIXME: dummy
            chan.progression.at(chan.counter).code.update(vec![]);
            let mut outputs = chan.progression.at(chan.counter).code.get_outputs();
            let mono = outputs.swap_remove(0);

            (primitives::output_mono::PRIM.update)(
                &mut self.output_mono,
                vec![self.out_chan.clone(), Value::Int(3), mono]
            );

            chan.pc += 1;
        }
    }

    // detach one channel to play given pattern
    pub fn send_pattern(&mut self, channel: usize, pattern: Pattern) {
        self.scene[channel].progression = Box::new(pattern);
    }

    // play the given scene (detach all patterns)
    fn send_scene(&mut self, scene: Vec<Pattern>) {
        for (i, p) in scene.into_iter().enumerate() {
            self.send_pattern(i, p);
        }
    }

    // rattach a given channel to its program progression
    fn rattach(&mut self, channel: usize) {
        self.scene[channel].progression =
            Box::new(self.program.tracks[channel].clone());
        // FIXME !!! should not copy whole tracks 😰
    }

    // detach a given channel to loop on its current pattern
    fn detach(&mut self, channel: usize) {
        let counter = self.scene[channel].counter;
        self.scene[channel].progression =
            Box::new(self.scene[channel].progression.at(counter).clone());
        // FIXME: clarify cloning dynamics (probably needs RC somewhere)
    }

    // detach a channel on its current pattern but makes a copy
    // of it so that modifications don't change it in the program
    fn detach_copy(&mut self, canal: usize) {
        let counter = self.scene[canal].counter;
        self.scene[canal].progression =
            Box::new(self.scene[canal].progression.at(counter).clone());
    }

    // all channels jump to corresponding position
    fn jump_scene(&mut self, scene: &[usize]) {
        for (i, &pos) in scene.iter().enumerate() {
            self.scene[i].counter = i;
        }
    }

    // all channels jump to corresponding position and rattach
    fn jump_scene_and_rattach(&mut self, scene: &[usize]) {
        for (i, &pos) in scene.iter().enumerate() {
            self.scene[i].counter = pos;
            self.rattach(i);
        }
    }

    // all channels counters jump forward by a given offset
    fn jump_forward(&mut self, offset: usize) {
        for chan in self.scene.iter_mut() {
            chan.counter += offset;
        }
    }

    // same as jump_forward() and rattach() all channels
    fn jump_and_rattach(&mut self, counter: usize) {
        self.jump_forward(counter);
        for i in 0 .. self.scene.len() {
            self.rattach(i);
        }
    }

    // load a new progression on the track in the program
    fn load_track(&mut self, i: usize, track: Track) {
        self.program.tracks[i] = track;
    }

    // load a new track and jump the corresponding channel at a given pos
    fn load_and_jump(&mut self, i: usize, track: Track, pos: usize) {
        self.load_track(i, track);
        self.scene[i].counter = pos;
    }

    // same as load_and_jump() and rattach() the channel
    fn load_and_rattach(&mut self, i: usize, track: Track, pos: usize) {
        self.load_and_jump(i, track, pos);
        self.rattach(i);
    }

    // starts a new loop of n bars for all channels
    fn loop_n(&mut self, n: usize) {
        for i in 0 .. self.scene.len() {
            let counter = self.scene[i].counter;
            self.loop_channel(i, counter, counter + n);
        }
    }

    // loops a given channel between given positions
    fn loop_channel(&mut self, channel: usize, from: usize, to: usize) {
        self.scene[channel].loop_begin = from;
        self.scene[channel].loop_end = to;
    }

    // starts runtime from initial program
    pub fn new(program: Program) -> Runtime {
        let patterns = Table(vec![]);
        let scene = program.tracks.iter().map(|t| {
            Canal {
                progression: Box::new(t.clone()), // FIXME! no copy!
                counter: 0,
                pc: 0,
                loop_begin: 0,
                loop_end: 0
            }
        }).collect::<Vec<_>>();

        // dummy
        use primitives;
        use bytecode::Value;
        let mut open_out = (primitives::open_out::PRIM.instance)();
        let out_chan = (primitives::open_out::PRIM.update)(
            &mut open_out,
            vec![Value::Str(Arc::new("IAC Driver Bus 1".to_string()))]
        );
        let output_mono = (primitives::output_mono::PRIM.instance)();

        Runtime { patterns, program, scene, out_chan, output_mono }
    }
}

pub fn load_program() -> Program {
    // // dummy program
    // let mut pattern0 = Vec::with_capacity(24*4);
    // for i in 0 .. 24*4 {
    //     pattern0.push((64, ::bytecode::Gate::Tie, 127));
    // }

    // for &i in [ 6, 12, 18, 30, 36,
    //             48, 54,66,78, 84, 90].iter() {
    //     pattern0[i] = ((64, ::bytecode::Gate::On, 127));
    //     pattern0[i-1] = ((64, ::bytecode::Gate::Off, 127));
    // }

    Program {
        tracks: vec![
            vec! []
        ],
        // do we need them?
        scenes: vec![]
    }
}
