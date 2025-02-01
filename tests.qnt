module test {
    import primitives.* from "./primitives/primitives"
    /* import tests::erc20 from "./tests/erc20" */
    /* import tests::erc721 from "./tests/erc721" */
    import evm.* from "./evm"

    run store::okNewEvmTest = {
        val evm = newEvm 
        assert(evm.state.keys().size() == 1)
    }

    run store::okCallTest = {
        val evm = 
            newEvm
                .create(10)
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

    run store::okDeployContractTest = {
        val tokenContract = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(tokenContract), 0, false)
    
        assert(evm.state.keys().size() == 2)
    }
    
    run store::okGetContractStateTest = {
        val initilalContractState = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(initilalContractState), 0, false)
        
        val contractState = erc20::cast(evm.address(1))
 
        assert(contractState.symbol == "ERC20")
    }

    run store::okReturnEoasSetContractTest = {
        val tokenContract = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(tokenContract), 0, false)
    
        assert(evm.eoas() == Set(0))
    }

    run store::okReturnContractsSetContractTest = {
        val tokenContract = erc20::construct("ERC20", Map(), 6)

        val evm = 
            newEvm
                .deploy(Erc20(tokenContract), 0, false)
    
        assert(evm.contracts() == Set(1))
    }
}
