module main {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"
    import evm.* from "./evm"
   
    pure val initialBalance = finney(10)
    pure val maxHeight = 3

    var evm: Evm

    action init = {
        evm' = 
            newEvm
                .create(0, "deployer")
                /* .deploy(Erc20(), 0, ) */
                .create(initialBalance, "alice")
                .create(initialBalance, "bob")
    }
    
    action step = any {
        all {
            evm.pc == "Evm::Call",
            all {
                nondet caller = evm.eoas().oneOf()
                any {
                    all {
                        evm.block.number < maxHeight,
                        evm' = evm.with("pc", "Evm::AdvanceChain")
                    },
                    all {
                        caller != 0,
                        evm' = evm.prank(caller, 0).with("pc", "Evm::Transfer")
                    }
                }
            }
        },
        all {
            evm.pc != "Evm::Call",
            any {
                evm::Transfer,
                evm::AdvanceChain
            }
        }
    }

    action evm::Transfer: bool = all {
        evm.pc == "Evm::Transfer",
        nondet target = 
            evm
                .eoas()
                .exclude(Set(0, evm.address("deployer")))
                .oneOf()
        nondet value = bounds(evm.balance(evm.caller)).oneOf()

        commit(
            evm.prank(evm.caller, value).call(target, "")
        )
    }

    action evm::AdvanceChain: bool = all {
        evm.pc == "Evm::AdvanceChain",
        commit({
            ...evm.prank(0,0).roll(evm.block.number + 1),
            pc: "Evm::Call",
        })
    }

    action commit(nextEvm: Evm): bool = all {
        nextEvm.pc != evmStatusRevert,
        evm' = nextEvm
    }

    action keep = all {
        evm' = { 
            ...evm,
            pc: "Evm::Call",
        }
    }

    // ___ SAFETY PROPERTIES ___
    /* pure def noNegativeBalances(balances: Address -> Uint): bool = */
    /*     balances.keys().forall( */
    /*         addr => isUint(balances.get(addr))) */

    /* val noNegativeEtherBalancesInv: bool = */
    /*     noNegativeBalances(state.evm.balances) */
    
    /* val zeroAddressNoEtherInv: bool = */
    /*     state.evm.balances.get(zeroAddress) == 0 */
   
    /* val noUnacceptableTransfersInv: bool = */
    /*     state.log.indices().forall(i => */
    /*         match (state.log[i]) { */ 
    /*             | EventTransferEther(data) => and { */
    /*                     isUint(data.amount), */
    /*                     data.recipient != zeroAddress, */
    /*                 } */
    /*             | _ => true */
    /*         } */
    /*     ) */

    /* val safetyOk = all { */
    /*     noNegativeEtherBalancesInv, */
    /*     zeroAddressNoEtherInv, */
    /*     noUnacceptableTransfersInv, */
    /* } */

    val safetyOk = true

    // __ LIVENESS PROPERTIES __
    temporal livenessOk = and {
        not(always(evm.block.number == 0))
    }
}
