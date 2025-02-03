module evm {
    import types.* from "./types/types"
   
    pure val evmStatusCall = "Evm::Call"
    pure val evmStatusRevert = "Evm::Revert"

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
        totalSupply: Uint,
        state: Address -> Account,
        alias: str -> Address,
        log: List[Event],
        pc: str,
        caller: Address,
        target: Address,
        value: Uint,
    }

    pure def newEvm: Evm = {
        block: {number: 0},
        totalSupply: 0,
        state: Map(0 -> Eoa({address: 0, balance: 0, isPayable: true})),
        alias: Map("zero" -> 0),
        log: List(),
        pc: evmStatusCall,
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
        balance: Uint,
        alias: str,
    ): Evm = {
        val nextAddress = evm.state.keys().size()
        {
            ...evm,
            state: evm.state.put(
                nextAddress,
                Eoa({address: nextAddress, balance: balance, isPayable: true})
            ),
            totalSupply: evm.totalSupply + balance,
            alias: if (alias != "") evm.alias.put(alias, nextAddress) else evm.alias,
        }
    }

    pure def deploy(
        evm: Evm,
        state: ContractState,
        balance: Uint, 
        alias: str,
        isPayable: bool,
    ): Evm = {
        val nextAddress = evm.state.keys().size()
        {
            ...evm,
            totalSupply: evm.totalSupply + balance,
            alias: if (alias != "") evm.alias.put(alias, nextAddress) else evm.alias,
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

    pure def setState(
        evm: Evm,
        addr: Address,
        nextState: ContractState
    ): Evm = {
        val acc = evm.account(addr)
        val nextAccount = 
            match acc {
                | Contract(acc) => Contract(acc.with("state", nextState))
                | _ => acc
            }
        {
            ...evm,
            state: evm.state.set(addr, nextAccount)    
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
        if (evm.isContract(target)) {
            if ((method == "") and evm.isPayable(target)) {
                evm.transfer(evm.caller, target, evm.value)
            } else {
                evm
                    .with("pc", method)
                    .with("target", target)
            }
        } else {
            evm.transfer(evm.caller, target, evm.value)
        }
    }

    pure def transfer(
        evm: Evm,
        sender: Address,
        recipient: Address,
        value: Uint
    ): Evm = {
        val isTransferPossible = and {
            value > 0,
            evm.balance(sender) >= value,
        }

        if (isTransferPossible) {
            evm
                .dealBy(sender, (balance) => balance - value)
                .dealBy(recipient, (balance) => balance + value )
                .emit(EventTransferEther({
                    sender: sender, 
                    recipient: recipient,
                    amount: value
                }))
                .purge()
        else {
            evm.revert("evm: insufficient balance")
        }
    }

    pure def balance(
        evm: Evm,
        addr: Address,
    ): Uint = {
        val acc = evm.account(addr)
        match acc {
            | Eoa(a) => a.balance
            | Contract(a) => a.balance
            | _ => 0
        }
    }
    
    pure def deal(
        evm: Evm,
        addr: Address,
        newBalance: Uint,
    ): Evm = {
        val acc = evm.account(addr)
        val nextAccount = 
            match acc {
                | Eoa(acc) => Eoa(acc.with("balance", newBalance))
                | Contract(acc) => Contract(acc.with("balance", newBalance))
                | _ => acc
            }
        {
            ...evm,
            state: evm.state.set(addr, nextAccount)    
        }
    }

    pure def dealBy(
        evm: Evm,
        addr: Address,
        update: Uint => Uint,
    ): Evm = {
        val acc = evm.account(addr)
        val nextAccount = 
            match acc {
                | Eoa(acc) => Eoa(acc.with("balance", update(acc.balance)))
                | Contract(acc) => Contract(acc.with("balance", update(acc.balance)))
                | _ => acc
            }
        {
            ...evm,
            state: evm.state.set(addr, nextAccount)    
        }
    }

    pure def address(
        evm: Evm,
        alias: str,
    ): Uint = {
        evm.alias.get(alias)
    }

    pure def account(
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
        val acc = evm.account(addr)
        match acc {
            | Eoa(_) => false
            | Contract(_) => true
            | _ => false
        }
    }

    pure def isPayable(
        evm: Evm,
        addr: Address
    ): bool = {
        val acc = evm.account(addr)
        match acc {
            | Eoa(a) => a.isPayable
            | Contract(a) => a.isPayable 
            | _ => false
        }
    }

    pure def addresses(
        evm: Evm
    ): Set[Uint] = {
       evm.state.keys()
    }

    pure def eoas(
        evm: Evm
    ): Set[Uint] = {
        evm.state.keys().fold(Set(), (result, addr) => {
            val acc = evm.account(addr)
            match acc {
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
            val acc = evm.account(addr)
            match acc {
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

    pure def purge(
        evm: Evm
    ): Evm = {
        ...evm,
        caller: 0,
        target: 0,
        value: 0,
        pc: evmStatusCall,
    }

    pure def revert(
        evm: Evm,
        reason: str
    ): Evm = {
        ...evm.purge(),
        pc: evmStatusRevert,
    }

    pure def emit(
        evm: Evm,
        e: Event
    ): Evm = {
        ...evm,
        log: evm.log.append(e),
    }
}
