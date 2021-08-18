use std::{
    collections::HashMap,
    convert::TryInto,
    error::Error,
    fmt,
    io::{Error as IError, ErrorKind, Read, Result as IResult, Write},
};

macro_rules! _get_num {
    ($num:ty,$r:expr,$en:expr) => {{
        let mut buf = [0u8; std::mem::size_of::<$num>()];
        $r.read_exact(&mut buf)?;
        match $en {
            Endian::Big => <$num>::from_be_bytes(buf),
            Endian::Little => <$num>::from_le_bytes(buf),
        }
    }};
}

macro_rules! _gen_enum {
    {
        $($name:ident($inner:ty)=$id:literal,)*
    } => {
        #[derive(Clone,Debug)]
        pub enum Value{
            $($name($inner)),*
        }

        impl Value{
            pub const fn id(&self)->u8{
                match self{
                    $(
                        Value::$name(_)=>$id,
                    )*
                }
            }

            pub const fn name(id:u8)->Option<&'static str>{
                match id{
                    $(
                        $id=>Some(stringify!("TAG_"$name)),
                    )*
                    _=>None
                }
            }

            pub fn from_reader_raw<R:Read>(id:u8, mut r:R, e:Endian)->IResult<Value>{
                Ok(match id{
                    $(
                        $id=><$inner>::read_nbt(&mut r,e)?.into(),
                    )*
                    _=>return Err(IError::new(
                        ErrorKind::InvalidData,
                        format!("Invalid type id: `{}`",id)
                    ))
                })
            }

            pub fn to_writer_raw<W:Write>(&self,check_id:Option<u8>,mut w:W, e:Endian)->IResult<()>{
                let err_id;
                match self{
                    $(
                        Value::$name(val)=>{
                            match check_id{
                                Some(id) if id != $id =>{
                                    //检查失败
                                    err_id=$id;
                                }
                                _=>{
                                    val.write_nbt(&mut w,e)?;
                                    return Ok(());
                                }
                            }
                        }
                    )*
                }
                Err(IError::new(
                    ErrorKind::InvalidData,
                    format!("Expected `{}`, found `{}`",Value::name(check_id.unwrap()).unwrap(),Value::name(err_id).unwrap())
                ))
            }
        }

        $(
            impl From<$inner> for Value{
                #[inline]
                fn from(fr:$inner)->Self{
                    Value::$name(fr)
                }
            }

            impl TryInto<$inner> for Value{
                type Error=ConvertError;
                fn try_into(self)->Result<$inner,Self::Error>{
                    match self{
                        Value::$name(v)=>Ok(v),
                        other=>Err(ConvertError(Value::name(other.id()).unwrap()))
                    }
                }
            }
        )*
    };
}

macro_rules! _impl_nbtval_for_num {
    ($($num:ty),*) => {
        $(
            impl NbtValue for $num{
                fn read_nbt<R:Read>(mut r:R,e:Endian)->IResult<Self>{
                    Ok(_get_num!($num,&mut r,e))
                }

                fn write_nbt<W:Write>(&self,mut w:W,e:Endian)->IResult<()>{
                    w.write_all(&match e{
                        Endian::Big=>self.to_be_bytes(),
                        Endian::Little=>self.to_le_bytes()
                    })?;
                    Ok(())
                }
            }
        )*
    };
}

macro_rules! _impl_nbtval_for_numarr {
    ($($num:ty),*) => {
        $(
            impl NbtValue for Vec<$num> {
                fn read_nbt<R: Read>(mut r: R, e: Endian) -> IResult<Self> {
                    let size = i32::read_nbt(&mut r, e)?;
                    let mut v = Self::with_capacity(size as usize);
                    for _ in 0..size{
                        v.push(<$num>::read_nbt(&mut r,e)?);
                    }
                    Ok(v)
                }

                fn write_nbt<W: Write>(&self, mut w: W, e: Endian) -> IResult<()> {
                    (self.len() as i32).write_nbt(&mut w,e)?;
                    for x in self.iter(){
                        x.write_nbt(&mut w,e)?;
                    }
                    Ok(())
                }
            }
        )*
    };
}

#[derive(Debug)]
pub struct ConvertError(&'static str);
impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cannot convert `{}` into given type", self.0)
    }
}
impl Error for ConvertError {}

#[derive(Clone, Copy)]
pub enum Endian {
    Little,
    Big,
}

_gen_enum! {
    End(()) = 0,
    Byte(i8) = 1,
    Short(i16) = 2,
    Int(i32) = 3,
    Long(i64) = 4,
    Float(f32) = 5,
    Double(f64) = 6,
    ByteArray(Vec<i8>) = 7,
    String(String) = 8,
    List(List) = 9,
    Compound(Compound) = 10,
    IntArray(Vec<i32>) = 11,
    LongArray(Vec<i64>) = 12,
}

pub trait NbtValue: Sized + Into<Value> {
    fn read_nbt<R: Read>(r: R, e: Endian) -> IResult<Self>;
    fn write_nbt<W: Write>(&self, w: W, e: Endian) -> IResult<()>;
}

_impl_nbtval_for_num!(i8, i16, i32, i64, f32, f64);

_impl_nbtval_for_numarr!(i8, i32, i64);

impl NbtValue for () {
    #[inline]
    fn read_nbt<R: Read>(_: R, _: Endian) -> IResult<Self> {
        Ok(())
    }

    #[inline]
    fn write_nbt<W: Write>(&self, _: W, _: Endian) -> IResult<()> {
        Ok(())
    }
}

impl NbtValue for String {
    fn read_nbt<R: Read>(mut r: R, e: Endian) -> IResult<Self> {
        let size = _get_num!(u16, &mut r, e);
        let mut v = Vec::with_capacity(size as usize);
        r.read_exact(&mut v)?;
        Ok(String::from_utf8_lossy(&v).to_string())
    }

    fn write_nbt<W: Write>(&self, mut w: W, e: Endian) -> IResult<()> {
        w.write(&{
            let b = self.len() as u16;
            match e {
                Endian::Big => b.to_be_bytes(),
                Endian::Little => b.to_le_bytes(),
            }
        })?;
        w.write(self.as_bytes())?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct List {
    pub type_id: u8,
    pub vec: Vec<Value>,
}

impl NbtValue for List {
    fn read_nbt<R: Read>(mut r: R, e: Endian) -> IResult<Self> {
        let id = _get_num!(u8, &mut r, e);
        let mut v = Vec::with_capacity(i32::read_nbt(&mut r, e)? as usize);
        for _ in 0..v.len() {
            v.push(Value::from_reader_raw(id, &mut r, e)?);
        }
        Ok(List {
            type_id: id,
            vec: v,
        })
    }

    fn write_nbt<W: Write>(&self, mut w: W, e: Endian) -> IResult<()> {
        if self.type_id > 12 {
            panic!("Type id cannot greater than 12");
        }
        w.write_all(&[self.type_id])?;
        (self.vec.len() as i32).write_nbt(&mut w, e)?;

        for (index, x) in self.vec.iter().enumerate() {
            x.to_writer_raw(Some(self.type_id), &mut w, e)
                .map_err(|x| {
                    IError::new(
                        ErrorKind::InvalidData,
                        format!(
                            "err occurred at index {}: {}",
                            index,
                            x.into_inner().unwrap()
                        ),
                    )
                })?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Compound {
    pub name: String,
    pub map: HashMap<String, Value>,
}

impl NbtValue for Compound {
    fn read_nbt<R: Read>(mut r: R, e: Endian) -> IResult<Self> {
        let name = String::read_nbt(&mut r, e)?;
        let mut map = HashMap::new();
        loop {
            match _get_num!(u8, &mut r, e) {
                0 => break,
                type_id => {
                    let name = String::read_nbt(&mut r, e)?;
                    let val = Value::from_reader_raw(type_id, &mut r, e)?;
                    map.insert(name, val);
                }
            }
        }
        Ok(Compound { name, map })
    }

    fn write_nbt<W: Write>(&self, mut w: W, e: Endian) -> IResult<()> {
        self.name.write_nbt(&mut w, e)?;
        for (index, x) in self.map.iter().enumerate() {
            match x {
                (name, Value::End(_)) => {
                    return Err(IError::new(
                        ErrorKind::InvalidData,
                        format!(
                        "err occurred at index {}: Cannot contain TAG_End in Compond (key: `{}`)",
                        index, name
                    ),
                    ))
                }
                (name, val) => {
                    w.write_all(&[val.id()])?;
                    name.write_nbt(&mut w, e)?;
                    val.to_writer_raw(None, &mut w, e)?;
                }
            }
        }
        w.write_all(&[0])?; //TAG_End
        Ok(())
    }
}
