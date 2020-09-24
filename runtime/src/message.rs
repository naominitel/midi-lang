use std::io;
use std::mem::transmute;

// message types
const T_UPDATE: u8 = 0x10;

#[repr(packed)]
#[derive(Copy, Clone)]
pub struct Header {
    msg_size: u32,
    msg_type: u8,

    _pad: [u8 ; 3]
}

#[repr(packed)]
#[derive(Copy, Clone)]
pub struct UpdateHeader {
    pub header: Header,
    pub name_ptr: u32,
    pub n_inputs: u8,
    pub n_outputs: u8,
    pub n_locals: u16,
    pub output_vars: [u8 ; 16],

    pub equ_sec_size: u32,
    pub cst_sec_size: u32,

    _pad: [u8 ; 0x18],
}

pub struct UpdateMessage {
    pub header: UpdateHeader,
    pub equ_section: Vec<u8>,
    pub locals: Vec<u8>,
    pub cst_section: Vec<u8>,
}

pub enum Message {
    Update(UpdateMessage)
}

impl Message {
    fn read_update<T: io::Read>(from: &mut T, header_buf: [u8 ; 0x40]) -> io::Result<Message> {
        let header = unsafe {
            *(transmute::<_, *const UpdateHeader>(&header_buf))
        };
        let mut equ_sec_buf = Vec::with_capacity(header.equ_sec_size as usize);
        equ_sec_buf.resize(header.equ_sec_size as usize, 0);
        from.read_exact(&mut equ_sec_buf)?;

        if header.equ_sec_size % 16 != 0 {
            // is always aligned either on 16 or 8
            let mut buf = [0;8];
            from.read_exact(&mut buf)?;
        }

        // FIXME: ew
        let locals_size =
            if header.n_locals == 0 { 0 }
            else { ((((header.n_locals - 1) << 1) >> 4) + 1) << 4 };
        let mut locals_buf = Vec::with_capacity(locals_size as usize);
        locals_buf.resize(locals_size as usize, 0);
        from.read_exact(&mut locals_buf)?;

        let mut cst_sec_buf = Vec::with_capacity(header.cst_sec_size as usize);
        cst_sec_buf.resize(header.cst_sec_size as usize, 0);
        from.read_exact(&mut cst_sec_buf)?;

        Ok(Message::Update(UpdateMessage {
            header: header, equ_section: equ_sec_buf,
            locals: locals_buf, cst_section: cst_sec_buf
        }))
    }

    pub fn read<T: io::Read>(from: &mut T) -> io::Result<Message> {
        let mut header_buf = [0 ; 0x40];
        from.read_exact(&mut header_buf)?;
        let header = unsafe { *(transmute::<_, *const Header>(&header_buf)) };

        match header.msg_type {
            T_UPDATE => Message::read_update(from, header_buf),
            _        => panic!("unknown message type")
        }
    }
}
