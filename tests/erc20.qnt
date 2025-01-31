module tests::erc20 {
    import types.* from "../types/types"
    import primitives.* from "../primitives/primitives"
    import tests::setup.* from "./setup"

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
