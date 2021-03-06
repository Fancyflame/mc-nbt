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
        $r.read_exact(&mut buf).expect("error!");
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

            ///Create new `Value`, using given parametsr.
            ///The same as `inner.into::<Value>()`
            #[inline]
            pub fn new<T:Into<Value>>(inner:T)->Value{
                inner.into()
            }

            ///Gets the identity of a value
            ///```
            ///let v=Value::new(0i16); //TAG_Short
            ///assert_eq!(v.id(),2);
            ///```
            pub const fn id(&self)->u8{
                match self{
                    $(
                        Value::$name(_)=>$id,
                    )*
                }
            }

            ///Gets the name of `id`, if `id` doesn't match
            ///any tags, returns `None`
            ///```
            ///assert!(Value::name(2),"TAG_Short");
            ///```
            pub const fn name(id:u8)->Option<&'static str>{
                match id{
                    0=>Some("TAG_End"),
                    $(
                        $id=>Some(concat!("TAG_",stringify!($name))),
                    )*
                    _=>None
                }
            }

            ///Reads one byte from the given `r`
            fn read_id<R:Read>(r:&mut R)->IResult<u8>{
                let mut buf=[0u8;1];
                r.read_exact(&mut buf).expect("error!");
                if buf[0]>12 {
                    Err(IError::new(
                        ErrorKind::InvalidData,
                        format!("Unrecognized tag id: `{}`",buf[0])
                    ))
                }else{
                    Ok(buf[0])
                }
            }

            ///Reads a complete NBT Value from `r`.
            ///```
            ///todo!();
            ///```
            #[inline]
            pub fn from_reader<R:Read>(mut r: &mut R, e:Endian)->IResult<Value>{
                Value::from_reader_raw(Value::read_id(r).expect("error!"), &mut r, e)
            }

            fn from_reader_raw<R:Read>(id:u8, r: &mut R, e:Endian)->IResult<Value>{
                Ok(match id{
                    $(
                        $id=><$inner>::read_nbt(r,e).expect("error!").into(),
                    )*
                    _=>return Err(IError::new(
                        ErrorKind::InvalidData,
                        format!("Invalid type id: `{}`",id)
                    ))
                })
            }

            #[inline]
            pub fn to_writer<W:Write>(&self,mut w:W,e:Endian)->IResult<()>{
                self.to_writer_raw(None,&mut w,e)
            }

            fn to_writer_raw<W:Write>(&self, check_id:Option<u8>, w:&mut W, e:Endian)->IResult<()>{
                let err_id;
                match self{
                    $(
                        Value::$name(val)=>{
                            match check_id{
                                Some(id) if id != $id =>{
                                    //????????????
                                    err_id=$id;
                                }
                                _=>{
                                    val.write_nbt(w,e).expect("error!");
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
                fn try_into(self)->std::result::Result<$inner,Self::Error>{
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
                fn read_nbt<R:Read>(r:&mut R,e:Endian)->IResult<Self>{
                    Ok(_get_num!($num,r,e))
                }

                fn write_nbt<W:Write>(&self,w:&mut W,e:Endian)->IResult<()>{
                    w.write_all(&match e{
                        Endian::Big=>self.to_be_bytes(),
                        Endian::Little=>self.to_le_bytes()
                    }).expect("error!");
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
                fn read_nbt<R: Read>(r: &mut R, e: Endian) -> IResult<Self> {
                    let size = i32::read_nbt(r, e).expect("error!");
                    let mut v = Self::with_capacity(size as usize);
                    for _ in 0..size{
                        v.push(<$num>::read_nbt(r, e).expect("error!"));
                    }
                    Ok(v)
                }

                fn write_nbt<W: Write>(&self, w: &mut W, e: Endian) -> IResult<()> {
                    (self.len() as i32).write_nbt(w,e).expect("error!");
                    for x in self.iter(){
                        x.write_nbt(w,e).expect("error!");
                    }
                    Ok(())
                }
            }
        )*
    };
}

pub type Compound = HashMap<String, Value>;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Clone, Debug)]
pub struct ConvertError(&'static str);

#[derive(Clone, Debug)]
pub struct List {
    pub type_id: u8,
    pub vec: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct Root {
    title: String,
    value: Compound,
}

#[derive(Clone, Copy)]
pub enum Endian {
    Little,
    Big,
}

#[derive(Debug)]
pub enum ParseError {
    IoError(IError),
    UnexpectedEndTag,
    InvalidId(u8),
    TagMismatched { expect: u8, found: u8 },
}

_gen_enum! {
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
    fn read_nbt<R: Read>(r: &mut R, e: Endian) -> IResult<Self>;
    fn write_nbt<W: Write>(&self, w: &mut W, e: Endian) -> IResult<()>;
}

_impl_nbtval_for_num!(i8, i16, i32, i64, f32, f64);

_impl_nbtval_for_numarr!(i8, i32, i64);

impl NbtValue for String {
    fn read_nbt<R: Read>(r: &mut R, e: Endian) -> IResult<Self> {
        let size = _get_num!(u16, r, e);
        let mut v = vec![0u8; size as usize];
        dbg!(size);
        r.read_exact(&mut v).expect("error!");
        String::from_utf8(v).map_err(|e| {
            IError::new(
                ErrorKind::InvalidData,
                format!(
                    "String is not utf8 encoded. try convert forcedly: {:?}",
                    String::from_utf8_lossy(e.as_bytes())
                ),
            )
        })
    }

    fn write_nbt<W: Write>(&self, w: &mut W, e: Endian) -> IResult<()> {
        w.write_all(&{
            let b = self.len() as u16;
            match e {
                Endian::Big => b.to_be_bytes(),
                Endian::Little => b.to_le_bytes(),
            }
        })
        .expect("error!");
        w.write_all(self.as_bytes()).expect("error!");
        Ok(())
    }
}

impl NbtValue for List {
    fn read_nbt<R: Read>(r: &mut R, e: Endian) -> IResult<Self> {
        let id = Value::read_id(r).expect("error!");
        let mut v = Vec::with_capacity(i32::read_nbt(r, e).expect("error!") as usize);
        for _ in 0..v.len() {
            v.push(Value::from_reader_raw(id, r, e).expect("error!"));
        }
        Ok(List {
            type_id: id,
            vec: v,
        })
    }

    fn write_nbt<W: Write>(&self, w: &mut W, e: Endian) -> IResult<()> {
        if self.type_id > 12 {
            panic!("Type id cannot greater than 12");
        }
        w.write_all(&[self.type_id]).expect("error!");
        (self.vec.len() as i32).write_nbt(w, e).expect("error!");

        for (index, x) in self.vec.iter().enumerate() {
            x.to_writer_raw(Some(self.type_id), w, e)
                .map_err(|x| {
                    IError::new(
                        ErrorKind::InvalidData,
                        format!(
                            "err occurred at index {}: {}",
                            index,
                            x.into_inner().unwrap()
                        ),
                    )
                })
                .expect("error!");
        }
        Ok(())
    }
}

impl NbtValue for Compound {
    fn read_nbt<R: Read>(r: &mut R, e: Endian) -> IResult<Self> {
        let mut map = HashMap::new();
        println!("open");
        loop {
            match Value::read_id(r).expect("error!") {
                0 => break,
                type_id => {
                    let name = String::read_nbt(r, e).expect("error!");
                    println!("insert: {}, {:?}", name, Value::name(type_id));
                    let val = Value::from_reader_raw(type_id, r, e).expect("error!");
                    map.insert(name, val);
                }
            }
        }
        println!("close");
        Ok(map)
    }

    fn write_nbt<W: Write>(&self, w: &mut W, e: Endian) -> IResult<()> {
        for x in self.iter() {
            let (name, val) = x;
            w.write_all(&[val.id()]).expect("error!");
            name.write_nbt(w, e).expect("error!");
            val.to_writer_raw(None, w, e).expect("error!");
        }
        w.write_all(&[0]).expect("error!"); //TAG_End
        Ok(())
    }
}

impl Root {
    fn from_reader<R: Read>(r: &mut R, e: Endian) -> Result<Self> {
        {
            let id = Value::read_id(r)?;
            if id != 10 {
                return Err(ParseError::TagMismatched {
                    expect: 10,
                    found: id,
                });
            }
        }
        todo!();
    }
}

impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cannot convert `{}` into given type", self.0)
    }
}

impl Error for ConvertError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidId(id) => write!(
                f,
                "Invalid id was read: {}. An id must equal or less than 12.",
                id
            ),
            ParseError::IoError(ref err) => {
                write!(f, "An IO error was occurred");
                match err.kind() {
                    ErrorKind::UnexpectedEof => write!(
                        f,
                        ": Read unexpected end of stream, maybe the data was damaged."
                    ),
                    _ => write!(f, ", error info: {}", err),
                }
            }
            ParseError::TagMismatched { expect, found } => {
                write!(
                    f,
                    "Tag read was mismatched: expect {}, found {}",
                    Value::name(*expect).expect("This id was invalid before mismatched!"),
                    Value::name(*found).expect("This id was invalid before mismatched!")
                )
            }
            ParseError::UnexpectedEndTag => {
                write!(f, "End tag can only appear at the end of a compond.")
            }
        }
    }
}

impl Error for ParseError {}

impl From<IError> for ParseError {
    #[inline]
    fn from(e: IError) -> Self {
        ParseError::IoError(e)
    }
}
