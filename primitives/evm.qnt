module primitives::evm {
    import types.* from "../types/types"

    type Block = {
        number: Uint,
    }

    type Message = {
        sender: Address,
        value: Uint,
    }

    type State = {
        block: Block,
        msg: Message,
        balances: Address -> Uint,
    }
    
    pure def construct(
        balances: Address -> Uint,
    ): State = {
        block: {number: 0},
        msg: {sender: zeroAddress, value: 0},
        balances: balances,
    }

    pure def balanceOf(
        state: State,
        addr: Address,
    ): Uint = {
        state.balances.get(addr)
    }

    pure def roll(state: State, n: int): State = {
        ...state,
        block: {
            ...state.block,
            number: state.block.number + n
        }
    }
    
    pure def transfer(
        state: State,
        sender: Address,
        recipient: Address,
        amount: Uint
    ): State = {
        ...state,
        balances: state.balances
            .setBy(sender, balance => balance - amount)
            .setBy(recipient, balance => balance + amount)
    }

    pure def withMessage(
        state: State,
        sender: Address,
        value: Uint,
    ): State = {
        ...state,
        msg: {
            sender: sender,
            value: value,
        }
    }

    pure def call(
        state: State,
        caller: Address,
        target: Address,
        value: Uint,
        method: str,
    ): Result[State] = {
        if (state.balanceOf(caller) < value) {
            revert(state, "evm: insufficient funds")
        } else {
            state
                .transfer(caller, target, value)
                .withMessage(caller, value)
                .success()
        }
    }
}
