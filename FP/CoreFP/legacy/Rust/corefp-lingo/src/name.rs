macro_rules! name {
    ($(($x:ident, $v:expr)),*) => {
        pub mod str {
            $( pub const $x:&str = $v; )*
        }
        pub mod string {
            use lazy_static::lazy_static;
            use std::prelude::v1::*;
            lazy_static! {
                $(
                    pub static ref $x: String = String::from($v);
                )*
            }
        }
        pub mod value {
            use crate::*;
            use lazy_static::lazy_static;
            lazy_static! {
                $(
                    pub static ref $x: Value = Value::new(CoreValue::Symbol(String::from($v)));
                )*
            }
        }
    };
}
name!((CORE,"核"),
    (ID,"識別子"),
    (EXP,"式"),
    (EXCEPTION_NODEF,"無定義"),
    (EXCEPTION_TYPEMISMATCH, "類不匹配"),
    (BINARY_LE_NAT, "小端二進位自然數"),
    (CHAR, "字符"),
    (STRING, "字串"),
    (TRUE, "陽"),
    (FALSE, "陰")
);