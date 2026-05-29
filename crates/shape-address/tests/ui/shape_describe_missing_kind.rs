use fe_shape_address::ShapeDescribe;

#[derive(Clone, ShapeDescribe)]
struct MissingKind {
    #[shape(structure)]
    value: String,
}

fn main() {}
