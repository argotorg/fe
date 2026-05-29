use fe_shape_address::ShapeDescribe;

#[derive(Clone, ShapeDescribe)]
#[shape(kind = "bad")]
struct ConflictingDimensions {
    #[shape(structure, names)]
    value: String,
}

fn main() {}
