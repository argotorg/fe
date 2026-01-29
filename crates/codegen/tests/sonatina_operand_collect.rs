use sonatina_ir::{
    ValueId,
    inst::{Inst, evm::EvmCreate2},
    isa::evm::Evm,
    isa::Isa,
    builder::ModuleBuilder,
    func_cursor::InstInserter,
    I256, Signature, Type,
};
use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

#[test]
fn evm_create2_collects_all_operands() {
    let triple = TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    );
    let isa = sonatina_ir::isa::evm::Evm::new(triple);
    let is = isa.inst_set();

    let inst = EvmCreate2::new(is, ValueId(1), ValueId(2), ValueId(3), ValueId(4));
    assert_eq!(inst.collect_values().len(), 4);
}

#[test]
fn make_imm_value_is_interned() {
    let triple = TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    );
    let isa = Evm::new(triple);
    let ctx = sonatina_ir::module::ModuleCtx::new(&isa);
    let builder = ModuleBuilder::new(ctx);

    let sig = Signature::new("f", sonatina_ir::Linkage::Public, &[], Type::Unit);
    let func_ref = builder.declare_function(sig).unwrap();
    let mut fb = builder.func_builder::<InstInserter>(func_ref);
    let entry = fb.append_block();
    fb.switch_to_block(entry);

    let a = fb.make_imm_value(I256::zero());
    let b = fb.make_imm_value(I256::zero());
    assert_eq!(a, b, "expected identical immediates to be interned");
}
