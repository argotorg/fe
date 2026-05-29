use fe_shape_address::ShapeDescribe;

#[derive(Clone, ShapeDescribe)]
#[shape(kind = "bad")]
struct SkipWithoutReason {
    #[shape(skip)]
    value: String,
}

fn main() {}
