use std::collections::HashMap;

use bytecode::NodeDef;
use node::NodeInstance;
use primitives;

// Engine: contains the current running state of the
// runtime, that is: all active node definitions and
// a shared pointer to the graph of node instances
//
// functions on this type are only used to update the
// state but don't do any actual computing (this is
// done in the CoreMidi thread), so code in this module
// should be executed only from the main thread
pub struct Engine {
    pub node_defs: DefMap,
    pub primitives: primitives::Env,

    // main_instance: Arc<Mutex<NodeInstance>>,
}

pub type DefMap = HashMap<String, NodeDef>;

impl Engine {
    pub fn new() -> Engine {
        let env = primitives::env();
        // let main_node = NodeDef::default();
        let node_defs = DefMap::new();
        // let main_instance = NodeInstance::new(&main_node, &node_defs, &env);
        // start with an empty main node which does nothing
        // node_defs.insert("main".into(), main_node);

        Engine {
            node_defs: node_defs,
            primitives: env,

            // main_instance: Arc::new(Mutex::new(main_instance))
        }
    }

    // returns a new shared pointer to the main instance node
    // pub fn main_instance(&mut self) -> Arc<Mutex<NodeInstance>> {
    //     self.main_instance.clone()
    // }

    // called when a new node definition is received over the network
    pub fn update(&mut self, node_def: NodeDef) {
        // let new_main = {
            self.node_defs.insert(node_def.name.to_string(), node_def);

            // TODO: this is not how it should be done...
            // let new_main = self.node_defs.get("main").unwrap();
            // NodeInstance::new(&new_main, &self.node_defs, &self.primitives)
            // release def table
        // };

        // *self.main_instance.lock().unwrap() = new_main;
    }

    pub fn update_and_inst(&mut self, node_def: NodeDef) -> NodeInstance {
        let name = node_def.name.to_string();
        self.update(node_def);
        let def = self.node_defs.get(&name).unwrap();
        let inst = NodeInstance::new(&def, &self);
        inst
    }
}
