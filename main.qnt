module main {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"
    import store as Store from "./store" 
   
    pure val initialBalance = finney(10)
    pure val maxHeight = 5

    pure val this = "deployer"
    pure val contracts = Set()
    pure val actors = Set(this, "alice", "bob", "chuck")
    pure val addresses = contracts.union(actors).union(Set(zeroAddress))

    val balances: Address -> Uint = 
        addresses
            .mapBy(addr => if (addr.in(actors)) initialBalance else 0)

    var state: Store::State

    action init = {
        state' = Store::construct(balances)
    }
    
    action step = {
        if (state.evm.block.number < maxHeight) {
            any {
                evm::Transfer,
                evm::AdvanceChain
            }
        } else { keep }
    }

    action evm::Transfer: bool = {
        nondet sender = actors.oneOf()
        nondet target = addresses.exclude(Set(sender, zeroAddress)).oneOf()
        nondet value = 0.to(evm::balanceOf(state.evm, sender)).oneOf()

        val result = evm::call(state.evm, sender, target, value, "")

        all {
            isOk(result),
            state' = {
                ...state,
                op: "Evm::Transfer",
                evm: result.ok,
                log: state.log.append(
                    EventTransferEther({sender: sender, recipient: target, amount: value})),
            }
        }
    }

    action evm::AdvanceChain: bool = all {
        state.evm.block.number < maxHeight,
        state' = {
            ...state,
            op: "Evm::AdvanceChain",
            evm: evm::roll(state.evm, 1),
        }
    }

    action keep = all {
        state' = { 
            ...state,
            op: "idle",
        }
    }

    // ___ SAFETY PROPERTIES ___
    pure def noNegativeBalances(balances: Address -> Uint): bool =
        balances.keys().forall(
            addr => isUint(balances.get(addr)))

    val noNegativeEtherBalancesInv: bool =
        noNegativeBalances(state.evm.balances)
    
    val zeroAddressNoEtherInv: bool =
        state.evm.balances.get(zeroAddress) == 0
   
    val noUnacceptableTransfersInv: bool =
        state.log.indices().forall(i =>
            match (state.log[i]) { 
                | EventTransferEther(data) => and {
                        isUint(data.amount),
                        data.recipient != zeroAddress,
                    }
                | _ => true
            }
        )

    val safetyOk = all {
        noNegativeEtherBalancesInv,
        zeroAddressNoEtherInv,
        noUnacceptableTransfersInv,
    }

    // __ LIVENESS PROPERTIES __
    temporal livenessOk = and {
        always(eventually(state.evm.block.number == maxHeight))
    }
}
