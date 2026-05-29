use fe_shape_address::ShapeDescribe;

#[derive(Clone, ShapeDescribe)]
#[shape(kind = "bad")]
struct MissingFieldPolicy {
    value: String,
}

fn main() {}
