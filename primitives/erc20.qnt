module primitives::erc20 {
    import types.* from "../types/types"

    type State = {
        symbol: str,
        balances: Address -> Uint,
        totalSupply: Uint,
        decimals: Uint,
    }
    
    pure def construct(
        symbol: str,
        balances: Address -> Uint,
        decimals: Uint,
    ): State = {
        symbol: symbol,
        balances: balances,
        totalSupply: balances.keys().fold(0, (acc, k) => acc + balances.get(k)),
        decimals: decimals,
    }

    pure def balanceOf(
        state: State,
        addr: Address,
    ): Uint = {
        state.balances.get(addr)
    }

    pure def _transfer(
        state: State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[State] = {
        if (_from == zeroAddress) {
            revert(state, "erc20: invalid sender")
        } else if (_to == zeroAddress) {
            revert(state, "erc20: invalid receiver")
        } else {
            _update(state, _from, _to, _value)
        }
    }

    pure def transfer(
        state: State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[State] = {
        _transfer(state, _from, _to, _value)
    }

    pure def _update(
        state: State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[State] = {
        // Handle minting case (_from is zero address)
        if (_from == zeroAddress) {
            success({
                ...state,
                totalSupply: state.totalSupply + _value,
                balances: state.balances.setBy(_to, current => current + _value)
            })
        } else if (_to == zeroAddress) {
            // Handle burning case (_to is zero address)
            val fromBalance = state.balances.get(_from)
            if (fromBalance < _value) {
                revert(state, "erc20: insufficient balance")
            } else {
                success({
                    ...state,
                    totalSupply: state.totalSupply - _value,
                    balances: state.balances.setBy(_from, current => current - _value)
                })
            }
        } else {
            // Handle normal transfer case
            val fromBalance = state.balances.get(_from)
            if (fromBalance < _value) {
                revert(state, "erc20: insufficient balance")
            } else {
                success({
                    ...state,
                    balances: state.balances
                        .setBy(_from, current => current - _value)
                        .setBy(_to, current => current + _value)
                })
            }
        }
    }
    
    pure def mint(
        state: State,
        _account: Address,
        _value: Uint,
    ): Result[State] = {
        if (_account == zeroAddress) {
            revert(state, "erc20: invalid receiver")
        } else {
            _update(state, zeroAddress, _account, _value)
        }
    }

    pure def burn(
        state: State,
        _account: Address,
        _value: Uint,
    ): Result[State] = {
        if (_account == zeroAddress) {
            revert(state, "erc20: invalid sender")
        } else {
            _update(state, _account, zeroAddress, _value)
        }
    }

}
