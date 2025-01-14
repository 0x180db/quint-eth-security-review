module test {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"

    pure val addresses = Set("alice", "bob")

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

    run erc20::failCallTest = {
        val vm = evm::construct(addresses.mapBy(addr => 10))

        val result = evm::call(vm, "alice", "bob", 11, "")
        if (isOk(result)) {
            assert(false)
        } else {
            assert(result.err != "")
        } 
    }

    run erc20::okTransferTest = {
        val token = erc20::construct(
            "damn",
            addresses.mapBy(addr => finney(10)),
            decimalFinney, 
        )

        val result = erc20::transfer(token, "alice", "bob", finney(1))

        if (isOk(result)) {
            val nextToken = result.ok 
            and {
                assert(nextToken.balances.get("alice") == finney(9)),
                assert(nextToken.balances.get("bob") == finney(11)),
                assert(nextToken.totalSupply == finney(20)),
            }
        } else {
            assert(false)
        }
    }
    
    run erc20::okMintTest = {
        val token = erc20::construct(
            "damn",
            addresses.mapBy(addr => finney(10)),
            decimalFinney, 
        )

        val result = erc20::mint(token, "alice", finney(1))

        if (isOk(result)) {
            val nextToken = result.ok 
            and {
                assert(nextToken.balances.get("alice") == finney(11)),
                assert(nextToken.totalSupply == finney(21)),
            }
        } else {
            assert(false)
        }
    }

    run erc20::okBurnTest = {
        val token = erc20::construct(
            "damn",
            addresses.mapBy(addr => finney(10)),
            decimalFinney, 
        )

        val result = erc20::burn(token, "alice", finney(1))

        if (isOk(result)) {
            val nextToken = result.ok 
            and {
                assert(nextToken.balances.get("alice") == finney(9)),
                assert(nextToken.totalSupply == finney(19)),
            }
        } else {
            assert(false)
        }
    }
}
