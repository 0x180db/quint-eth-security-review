module evm {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"
   
    pure val idle = "Evm::Idle"

    type Msg = {
        sender: Address,
        value: Uint
    }

    type Block = {
        number: Uint,
    }

    type ExternallyOwnedAccount = {
        address: Uint,
        balance: Uint,
        isPayable: bool,
    }

    type ContractState =
            | Erc20(types::erc20::State)
            | Erc721(types::erc721::State)

    type ContractAccount = {
        address: Uint,
        balance: Uint,
        isPayable: bool,
        state: ContractState,
    }

    type Account =
        | Eoa(ExternallyOwnedAccount)
        | Contract(ContractAccount)
   
    type Evm = {
        block: Block,
        state: Address -> Account,
        log: List[Event],
        pc: str,
        caller: Address,
        target: Address,
        value: Uint,
    }

    pure def newEvm: Evm = {
        block: {number: 0},
        state: Map(0 -> Eoa({address: 0, balance: 0, isPayable: true})),
        log: List(),
        pc: idle,
        caller: 0,
        target: 0,
        value: 0,
    }

    pure def roll(
        evm: Evm,
        number: Uint, 
    ): Evm = {
        ...evm,
        block: {
            ...evm.block,
            number: number
        }
    }

    pure def create(
        evm: Evm,
        balance: Uint
    ): Evm = {
        val nextAddress = evm.state.keys().size()
        {
            ...evm,
            state: evm.state.put(
                nextAddress,
                Eoa({address: nextAddress, balance: balance, isPayable: true})
            )
        }
    }

    pure def deploy(
        evm: Evm,
        state: ContractState,
        balance: Uint, 
        isPayable: bool,
    ): Evm = {
        val nextAddress = evm.state.keys().size()
        {
            ...evm,
            state: evm.state.put(
                nextAddress,
                Contract({
                    address: nextAddress,
                    balance: balance,
                    state: state,
                    isPayable: isPayable
                })
            )
        }
    }

    pure def prank(
        evm: Evm,
        caller: Address,
        value: Uint,
    ): Evm = {
        ...evm,
        caller: caller,
        value: value
    }

    pure def call(
        evm: Evm,
        target: Address,
        method: str
    ): Evm = {
        val callerBalance = evm.balance(evm.caller)

        val isTransferPossible = and {
            evm.value > 0,
            callerBalance >= evm.value,
            not(evm.isContract(evm.target))
        }

        if (isTransferPossible) {
            val targetBalance = evm.balance(evm.target)
            evm
                .deal(evm.caller, callerBalance - evm.value)
                .deal(evm.target, targetBalance + evm.value )
                .with("caller", 0)
                .with("value", 0)
                .with("pc", idle)
        } else {
            evm
                .with("pc", method)
                .with("target", target)
        }
    }

    pure def balance(
        evm: Evm,
        addr: Address,
    ): Uint = {
        val account = evm.address(addr)
        match account {
            | Eoa(acc) => acc.balance
            | Contract(acc) => acc.balance
            | _ => 0
        }
    }
    
    pure def deal(
        evm: Evm,
        addr: Address,
        newBalance: Uint,
    ): Evm = {
        val account = evm.address(addr)
        val nextAccount = 
            match account {
                | Eoa(acc) => Eoa(acc.with("balance", newBalance))
                | Contract(acc) => Contract(acc.with("balance", newBalance))
                | _ => account
            }
        {
            ...evm,
            state: evm.state.set(addr, nextAccount)    
        }
    }

    pure def address(
        evm: Evm,
        addr: Address
    ): Account = {
        if (addr.in(evm.state.keys())) {
            evm.state.get(addr)
        } else {
            evm.state.get(0)
        }
    }
    
    pure def isContract(
        evm: Evm,
        addr: Address
    ): bool = {
        val account = evm.address(addr)
        match account {
            | Eoa(acc) => false
            | Contract(acc) => true
            | _ => false
        }
    } 

    pure def eoas(
        evm: Evm
    ): Set[Uint] = {
        evm.state.keys().fold(Set(), (result, addr) => {
            val account = evm.address(addr)
            match account {
                | Eoa(_) => result.union(Set(addr))
                | Contract(_) => result
                | _ => result
            }
        })
    }

    pure def contracts(
        evm: Evm
    ): Set[Uint] = {
        evm.state.keys().fold(Set(), (result, addr) => {
            val account = evm.address(addr)
            match account {
                | Eoa(_) => result
                | Contract(_) => result.union(Set(addr))
                | _ => result
            }
        })
    }

    pure def msg(
        evm: Evm
    ): Msg = {
        sender: evm.caller,
        value: evm.value
    }

    pure def cancel(
        evm: Evm
    ): Evm = {
        ...evm,
        caller: 0,
        target: 0,
        value: 0,
        pc: idle,
    }

    pure def erc20::cast(
        account: Account
    ): types::erc20::State = {
        val default = erc20::construct("", Map(), 0)
        match account {
            | Contract(acc) => {
                match acc.state {
                    | Erc20(state) => state
                    | _ => default
                }
            }
            | _ => default
        }
    }
    
}
