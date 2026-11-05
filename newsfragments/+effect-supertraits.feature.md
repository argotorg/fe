A trait effect now provides its supertraits, and theirs in turn: in a function that `uses (evm: mut Super)`, a call that needs `Ctx` or `Call` finds them. A supertrait does not provide its subtraits.
