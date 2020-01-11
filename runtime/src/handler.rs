use std::sync::{Arc, Mutex};
use node;

// the type that contains the code to handle incoming
// MIDI messages. contains the entry point of all code
// that will be executed from the CoreMidi thread.
//
// stores only a shared pointer to the main node instance
pub struct MidiHandler {
    main_instance: Arc<Mutex<node::NodeInstance>>,
}

const MIDI_COMMAND_CLOCK: u8 = 0xF8;

impl MidiHandler {
    pub fn new(main_instance: Arc<Mutex<node::NodeInstance>>) -> MidiHandler {
        MidiHandler { main_instance }
    }

    fn handle_clock(&self) {
        let mut main_instance = self.main_instance.lock().unwrap();
        main_instance.update(vec![]);
    }

    // entry point: called automatically from the callback closure
    // in the CoreMidi thread whenever a MIDI message is received
    // decoding and code execution is handled in that thread
    pub fn handle_message(&self, msg: u8, _data: &[u8]) {
        if msg == MIDI_COMMAND_CLOCK {
            self.handle_clock();
        }
    }
}
