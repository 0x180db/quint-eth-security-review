module main {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"
    import evm.* from "./evm"
   
    pure val initialBalance = finney(10)
    pure val maxHeight = 3

    var evm: Evm
    
    val usdt = 
        erc20::construct("USDT", Map(1 -> finney(10)), decimalFinney)

    action init = {
        evm' = 
            newEvm
                .create(0, "deployer")
                .create(initialBalance, "alice")
                .create(initialBalance, "bob")
                .deploy(Erc20(usdt), 0, "usdt", false)
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
                        evm.balance(caller) > 0,
                        evm' = evm.prank(caller, 0).with("pc", "Evm::Transfer")
                    },
                    all {
                        val transferableTokens = filterErc20Contracts(evm, (state) => {
                            erc20::balanceOf(state, caller) > 0
                        })
                        all {
                            transferableTokens.size() > 0,
                            nondet target = transferableTokens.oneOf()
                            evm' = evm.prank(caller, 0).call(target, "Erc20::Transfer")
                        }
                    }
                }
            }
        },
        all {
            evm.pc != "Evm::Call",
            any {
                erc20::Transfer,
                evm::Transfer,
                evm::AdvanceChain
            }
        }
    }

    action erc20::Transfer: bool = all {
        evm.pc == "Erc20::Transfer",
        nondet recipient = 
            evm
                .eoas()
                .exclude(Set(0, evm.address("deployer")))
                .oneOf()
        val token = erc20::cast(evm.account(evm.target))
        nondet value = bounds(erc20::balanceOf(token, evm.caller)).oneOf()
        all {
            value > 0,
            commit(
                erc20::safeTransfer(evm, recipient, value)
            )
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
        commit(
            evm.roll(evm.block.number + 1),
        )
    }

    action commit(nextEvm: Evm): bool = all {
        nextEvm.pc != evmStatusRevert,
        evm' = nextEvm.purge()
    }

    action keep = all {
        evm' = { 
            ...evm,
            pc: "Evm::Call",
        }
    }
    
    pure def filterErc20Contracts(
        evm: Evm,
        pred: (types::erc20::State) => bool
    ): Set[Uint] = {
        evm.contracts().fold(Set(), (result, addr) => {
            match evm.account(addr) {
                | Contract(c) =>
                    match c.state {
                        | Erc20(state) => 
                            if (pred(state)) { 
                                result.union(Set(c.address))
                            } else { result }
                        | _ => result
                    }
                | _ => result
            }
        })
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
