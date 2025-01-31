module tests::evm {
    import types.* from "../types/types"
    import primitives.* from "../primitives/primitives"
    import tests::setup.* from "./setup"

    run evm::rollBlockNumberTest = {
        val vm = evm::construct(addresses.mapBy(addr => 10))
        val nextVm = evm::roll(vm, 1)
        assert(nextVm.block.number == 1)
    }

    run evm::okCallTest = {
        val vm = evm::construct(addresses.mapBy(addr => 10))

        val result = evm::call(vm, "alice", "bob", 10, "")
        if (isOk(result)) {
            val nextVm = result.ok 
            and {
                assert(nextVm.balances.get("alice") == 0),
                assert(nextVm.balances.get("bob") == 20),
            }
        } else {
            assert(false)
        } 
    }

    run evm::failCallTest = {
        val vm = evm::construct(addresses.mapBy(addr => 10))

        val result = evm::call(vm, "alice", "bob", 11, "")
        if (isOk(result)) {
            assert(false)
        } else {
            assert(result.err != "")
        } 
    }
}
