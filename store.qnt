module store {
    import types.* from "./types/types"
    import primitives.* from "./primitives/primitives"

    type State = {
        op: str,
        evm: evm::State,
        log: List[Event]
    }
    
    pure def construct(
        balances: Address -> Uint,
    ): State = {
        op: "init",
        evm: evm::construct(balances),
        log: List(),
    }
    
    pure def emit(state: State, e: Event): State = {
        ...state,
        log: state.log.append(e),
    }
}
