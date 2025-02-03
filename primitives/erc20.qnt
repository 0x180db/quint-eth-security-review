module primitives::erc20 {
    import types.* from "../types/types"
    import evm.* from "../evm"
 
    pure def cast(
        acc: Account
    ): types::erc20::State = {
        val default = construct("", Map(), 0)
        match acc {
            | Contract(acc) => {
                match acc.state {
                    | Erc20(state) => state
                    | _ => default
                }
            }
            | _ => default
        }
    }   

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
        if (addr.in(state.balances.keys())) {
            state.balances.get(addr)
        } else {
            0
        }
    }

    pure def safeTransfer(
        evm: Evm,
        _to: Address,
        _value: Uint,
    ): Evm = {
        val state = cast(evm.account(evm.target))
        val result = _transfer(state, evm.caller, _to, _value)
        
        if (isErr(result)) {
            evm.revert(result.err)
        } else {
            evm
                .setState(evm.target, Erc20(result.ok))
                .emit(
                    EventTransferErc20({
                        sender: evm.caller,
                        recipient: _to,
                        amount: _value    
                    })   
                )
        }
    }

    pure def safeTransferFrom(
        evm: Evm,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Evm = {
        val state = cast(evm.account(evm.target))
        val result = _transfer(state, _from, _to, _value)
        
        if (isErr(result)) {
            evm.revert(result.err)
        } else {
            evm
                .setState(evm.target, Erc20(result.ok))
                .emit(
                    EventTransferErc20({
                        sender: _from,
                        recipient: _to,
                        amount: _value    
                    })   
                )
        }
    }

    pure def _transfer(
        state: types::erc20::State,
        _from: Address,
        _to: Address,
        _value: Uint,
    ): Result[types::erc20::State] = {
        if (_from == zeroAddress) {
            failure(state, "erc20: invalid sender")
        } else if (_to == zeroAddress) {
            failure(state, "erc20: invalid receiver")
        } else {
            _update(state, _from, _to, _value)
        }
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
                balances: state.balances.safeSetBy(_to, 0, current => current + _value)
            })
        } else if (_to == zeroAddress) {
            // Handle burning case (_to is zero address)
            val fromBalance = state.balances.getOrElse(_from, 0)
            if (fromBalance < _value) {
                failure(state, "erc20: insufficient balance")
            } else {
                success({
                    ...state,
                    totalSupply: state.totalSupply - _value,
                    balances: state.balances.safeSetBy(_from, 0, current => current - _value)
                })
            }
        } else {
            // Handle normal transfer case
            val fromBalance = state.balances.getOrElse(_from, 0)
            if (fromBalance < _value) {
                failure(state, "erc20: insufficient balance")
            } else {
                success({
                    ...state,
                    balances: state.balances
                        .safeSetBy(_from, 0, current => current - _value)
                        .safeSetBy(_to, 0, current => current + _value)
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
            failure(state, "erc20: invalid receiver")
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
            failure(state, "erc20: invalid sender")
        } else {
            _update(state, _account, zeroAddress, _value)
        }
    }
}
