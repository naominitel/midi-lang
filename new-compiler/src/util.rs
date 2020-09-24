use std::io;

#[cfg(target_endian = "little")]
unsafe fn write_le<S: io::Write + ?Sized, T>(w: &mut S, x: T) -> io::Result<()> {
    use std::{mem, slice};
    let sz = mem::size_of::<T>();
    let bin = mem::transmute::<_, *const u8>(&x);
    w.write(slice::from_raw_parts(bin, sz))?;
    Ok(())
}

pub trait BinWrite: io::Write {
    fn write_le<T>(&mut self, x: T) -> io::Result<()> {
        unsafe { write_le(self, x) }
    }

    fn pad(&mut self, count: usize) -> io::Result<()> {
        // TODO: optimize
        for _ in 0 .. count {
            self.write_le(0u8)?;
        }
        Ok(())
    }
}

pub trait BinWriteExt: BinWrite {
    fn len(&self) -> usize;

    fn align(&mut self, align: usize) -> io::Result<()> {
        while self.len() % align != 0 {
            self.write_le(0u8)?;
        }
        Ok(())
    }
}

impl<T: io::Write> BinWrite for T {}
