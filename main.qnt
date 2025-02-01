module main {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"
    import evm.* from "./evm"
   
    pure val initialBalance = finney(10)
    pure val maxHeight = 10

    var evm: Evm

    action init = {
        evm' = newEvm
    }
    
    action step = {
        if (evm.block.number < maxHeight) {
            any {
                all {
                    evm.pc == "Evm::Idle",
                    all {
                        nondet caller = evm.eoas().oneOf()
                        any {
                            all {
                                evm' = evm.prank(caller, 0).call(0, "Evm::AdvanceChain")
                            }
                        }
                    }
                },
                all {
                    evm.pc != "Evm::Idle",
                    any {
                        /* evm::Transfer, */
                        evm::AdvanceChain
                    }
                }
            }
        } else { keep }
    }

    /* action evm::Transfer: bool = { */
    /*     nondet sender = actors.oneOf() */
    /*     nondet target = addresses.exclude(Set(sender, zeroAddress)).oneOf() */
    /*     nondet value = 0.to(evm::balanceOf(state.evm, sender)).oneOf() */

    /*     val result = evm::call(state.evm, sender, target, value, "") */

    /*     all { */
    /*         isOk(result), */
    /*         state' = { */
    /*             ...state, */
    /*             op: "Evm::Transfer", */
    /*             evm: result.ok, */
    /*             log: state.log.append( */
    /*                 EventTransferEther({sender: sender, recipient: target, amount: value})), */
    /*         } */
    /*     } */
    /* } */

    action evm::AdvanceChain: bool = all {
        evm.pc == "Evm::AdvanceChain",
        evm' = {
            ...evm.prank(0,0).roll(evm.block.number + 1),
            pc: "Evm::Idle",
        }
    }

    action keep = all {
        evm' = { 
            ...evm,
            pc: "Evm::Idle",
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
        always(eventually(evm.block.number == maxHeight))
    }
}
