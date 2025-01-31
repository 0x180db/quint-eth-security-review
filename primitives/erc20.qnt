module primitives::erc20 {
    import types.* from "../types/types"
    
    pure def construct(
        symbol: str,
        balances: Address -> Uint,
        decimals: Uint,
    ): types::erc20::State = {
        symbol: symbol,
        balances: balances,
        totalSupply: balances.keys().fold(0, (acc, k) => acc + balances.get(k)),
        decimals: decimals,
    }

    pure def balanceOf(
        state: types::erc20::State,
        addr: Address,
    ): Uint = {
        state.balances.get(addr)
    }

    pure def _transfer(
        state: types::erc20::State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[types::erc20::State] = {
        if (_from == zeroAddress) {
            revert(state, "erc20: invalid sender")
        } else if (_to == zeroAddress) {
            revert(state, "erc20: invalid receiver")
        } else {
            _update(state, _from, _to, _value)
        }
    }

    pure def transfer(
        state: types::erc20::State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[types::erc20::State] = {
        _transfer(state, _from, _to, _value)
    }

    pure def _update(
        state: types::erc20::State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[types::erc20::State] = {
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
        state: types::erc20::State,
        _account: Address,
        _value: Uint,
    ): Result[types::erc20::State] = {
        if (_account == zeroAddress) {
            revert(state, "erc20: invalid receiver")
        } else {
            _update(state, zeroAddress, _account, _value)
        }
    }

    pure def burn(
        state: types::erc20::State,
        _account: Address,
        _value: Uint,
    ): Result[types::erc20::State] = {
        if (_account == zeroAddress) {
            revert(state, "erc20: invalid sender")
        } else {
            _update(state, _account, zeroAddress, _value)
        }
    }

}
