module test {
    import primitives.* from "./primitives/primitives"
    /* import tests::erc20 from "./tests/erc20" */
    /* import tests::erc721 from "./tests/erc721" */
    import evm.* from "./evm"

    run evm::okNewEvmTest = {
        val evm = newEvm 
        assert(evm.state.keys().size() == 1)
    }

    run evm::okDealByTest = {
        val evm = 
            newEvm
                .create(5, "alice")
                .dealBy(1, (balance) => balance + 5)
        assert(evm.balance(1) == 10)
    }

    run evm::okCallTest = {
        val evm = 
            newEvm
                .create(10, "alice")
                .prank(1, 5)
                .call(0, "")
        and { 
            assert(evm.state.keys().size() == 2),
            assert(evm.target == 0),
            assert(evm.msg().sender == 0),
            assert(evm.msg().value == 0),
            assert(evm.balance(0) == 5),
            assert(evm.balance(1) == 5),
        }
    }

    run evm::okDeployContractTest = {
        val tokenContract = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(tokenContract), 0, "", false)
    
        assert(evm.state.keys().size() == 2)
    }
    
    run evm::okGetContractStateTest = {
        val initilalContractState = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(initilalContractState), 0, "", false)
        
        val contractState = erc20::cast(evm.account(1))
 
        assert(contractState.symbol == "ERC20")
    }

    run evm::okReturnSetOfEoaAddressTest = {
        val tokenContract = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(tokenContract), 0, "", false)
    
        assert(evm.eoas() == Set(0))
    }

    run evm::okReturnSetOfContractAddressTest = {
        val tokenContract = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(tokenContract), 0, "", false)
    
        assert(evm.contracts() == Set(1))
    }
}
