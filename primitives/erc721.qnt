module primitives::erc721 {
    import types.* from "../types/types"

    pure def construct(
        symbol: str,
        balances: Address -> Uint,
        owners: Uint -> Address,
    ): types::erc721::State = {
        symbol: symbol,
        balances: balances,
        owners: owners
    }

    pure def balanceOf(
        state: types::erc721::State,
        owner: Address,
    ): Result[Uint] = {
        if (owner == zeroAddress) {
            revert(0, "erc721: invalid owner")
        } else {
            success(state.balances.get(owner))
        }
    }
    
    pure def ownerOf(
        state: types::erc721::State,
        tokenId: Uint,
    ): Result[Address] = {
        _requireOwned(state, tokenId)
    }

    pure def totalSupply(
        state: types::erc721::State
    ): Uint = {
        state.balances.keys().fold(0, (acc, k) => acc + state.balances.get(k))       
    }

    pure def safeSetOwner(
        owners: Uint -> Address,
        _tokenId: Uint,
        _owner: Address
    ): Uint -> Address = {
        if (_tokenId.in(owners.keys())) {
            owners.set(_tokenId, _owner)
        } else {
            owners.put(_tokenId, _owner)
        }
    }

    pure def _update(
        state: types::erc721::State, 
        _to: Address,
        _tokenId: Uint,
        _auth: Address,
    ): (Result[types::erc721::State], Address) = {
        val _from = _ownerOf(state, _tokenId)
        val authErr = 
            if (_auth != zeroAddress) {
                _checkAuthorized(_from, _auth, _tokenId)
            } else { "" }

        if (authErr != "") {
            (revert(state, authErr), zeroAddress)
        } else {
            val nextFromBalance =
                if (_from != zeroAddress) {
                    state.balances.get(_from) - 1
                } else {
                    state.balances.get(_from)
                }
            val nextToBalance =
                if (_to != zeroAddress) {
                    state.balances.get(_to) + 1
                } else {
                    state.balances.get(_to)
                }
            
            val nexttypes::erc721::State = {
                ...state,
                balances: state.balances
                            .set(_from, nextFromBalance)
                            .set(_to, nextToBalance),
                owners: safeSetOwner(state.owners, _tokenId, _to)
            }

            (success(nexttypes::erc721::State), _from)
        }
    }

    pure def transferFrom(
        state: types::erc721::State, 
        _from: Address,
        _to: Address,
        _tokenId: Uint,
    ): Result[types::erc721::State] = {
        if (_to == zeroAddress) {
            revert(state, "erc721: invalid receiver")
        } else {
            val result = _update(state, _to, _tokenId, _from)  
            
            if (isErr(result._1)) {
                revert(state, result._1.err)
            } else if (result._2 != _from) {
                revert(state, "erc721: incorrect owner")
            } else {
                success(result._1.ok)
            }
        }
    }

    pure def safeTransferFrom(
        state: types::erc721::State, 
        _from: Address,
        _to: Address,
        _tokenId: Uint,
    ): Result[types::erc721::State] = {
        transferFrom(state, _from, _to, _tokenId)
    }
    
    pure def _ownerOf(
        state: types::erc721::State,
        tokenId: Uint,
    ): Address = {
        if (tokenId.in(state.owners.keys())) {
            state.owners.get(tokenId)
        } else {
            zeroAddress
        }
    }

    pure def _requireOwned(
        state: types::erc721::State,
        tokenId: Uint,
    ): Result[Address] = {
        val owner = _ownerOf(state, tokenId)
        if (owner == zeroAddress) {
            revert(owner, "erc721: non existent token")
        } else {
            success(owner)
        }
    }

    pure def _isAuthorized(
        owner: Address,
        spender: Address, 
        tokenId: Uint,
    ): bool = {
        spender != zeroAddress and owner == spender 
    }

    pure def _checkAuthorized(
        owner: Address,
        spender: Address, 
        tokenId: Uint,
    ): str = {
        if (not(_isAuthorized(owner, spender, tokenId))) {
            if (owner == zeroAddress) {
                "erc721: non existent token"
            } else {
                "erc721: insufficient approval"
            }
        } else {
            ""
        }
    }
    
    pure def mint(
        state: types::erc721::State, 
        _to: Address,
        _tokenId: Uint,
    ): Result[types::erc721::State] = {
        if (_to == zeroAddress) {
            revert(state, "erc721: invalid receiver")
        } else {
            val result = _update(state, _to, _tokenId, zeroAddress)  
            
            if (isErr(result._1)) {
                revert(state, result._1.err)
            } else if (result._2 != zeroAddress) {
                revert(state, "erc721: invalid sender")
            } else {
                success(result._1.ok)
            }
        }
    }
}
