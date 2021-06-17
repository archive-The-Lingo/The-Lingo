macro_rules! name {
    ($(($x:ident, $v:expr)),*) => {
        pub mod str {
            $( pub const $x:&str = $v; )*
        }
        pub mod string {
            use lazy_static::lazy_static;
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
    (EXCEPTION_NODEF,"無定義")
);