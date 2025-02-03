module primitives::erc721 {
    import types.* from "../types/types"
    import evm.* from "../evm"

    pure def cast(
        acc: Account
    ): types::erc721::State = {
        val default = construct("", Map(), Map())
        match acc {
            | Contract(acc) => {
                match acc.state {
                    | Erc721(state) => state
                    | _ => default
                }
            }
            | _ => default
        }
    }

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
        if (owner == 0) {
            failure(0, "erc721: invalid owner")
        } else {
            success(state.balances.getOrElse(owner, 0))
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
        state.balances.keys().fold(0, (acc, k) => acc + state.balances.getOrElse(k, 0))       
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
            if (_auth != 0) {
                _checkAuthorized(_from, _auth, _tokenId)
            } else { "" }

        if (authErr != "") {
            (failure(state, authErr), 0)
        } else {
            val nextState = {
                ...state,
                balances: state.balances
                            .safeSetBy(_from, 0, (balance) => 
                                if (_from != 0) balance - 1 else balance
                            )
                            .safeSetBy(_to, 0, (balance) => 
                                if (_from != 0) balance + 1 else balance
                            ),
                owners: safeSetOwner(state.owners, _tokenId, _to)
            }

            (success(nextState), _from)
        }
    }

    pure def transferFrom(
        evm: Evm,
        _from: Address,
        _to: Address,
        _tokenId: Uint,
    ): Evm = {
        val state = cast(evm.account(evm.target))

        if _to == 0) {
            evm.revert("erc721: invalid receiver")
        } else {
            val result = _update(state, _to, _tokenId, _from)  
            
            if (isErr(result._1)) {
                evm.revert(result._1.err)
            } else if (result._2 != _from) {
                evm.revert("erc721: incorrect owner")
            } else {
                evm
                    .setState(evm.target, Erc721(result._1.ok))
                    .emit(
                        EventTransferErc721({
                            sender: _from,
                            recipient: _to,
                            tokenId: _tokenId
                        })   
                    )
            }
        }
    }

    pure def safeTransferFrom(
        evm: Evm,
        _from: Address,
        _to: Address,
        _tokenId: Uint,
    ): Evm = {
        transferFrom(evm, _from, _to, _tokenId)
    }
    
    pure def _ownerOf(
        state: types::erc721::State,
        tokenId: Uint,
    ): Address = {
        if (tokenId.in(state.owners.keys())) {
            state.owners.getOrElse(tokenId, 0)
        } else {
            0
        }
    }

    pure def mint(
        evm: Evm,
        _to: Address,
        _tokenId: Uint,
    ): Evm = {
        val state = cast(evm.account(evm.target))

        if (_to == 0) {
            evm.revert("erc721: invalid receiver")
        } else {
            val result = _update(state, _to, _tokenId, 0)  
            
            if (isErr(result._1)) {
                evm.revert(result._1.err)
            } else if (result._2 != 0) {
                evm.revert("erc721: invalid sender")
            } else {
                evm
                    .setState(evm.target, Erc721(result._1.ok))
                    .emit(
                        EventTransferErc721({
                            sender: 0,
                            recipient: _to,
                            tokenId: _tokenId
                        })   
                    )
            }
        }
    }

    pure def burn(
        evm: Evm,
        _tokenId: Uint,
    ): Evm = {
        val state = cast(evm.account(evm.target))

        val result = _update(state, 0, _tokenId, 0)  
        
        if (isErr(result._1)) {
            evm.revert(result._1.err)
        } else if (result._2 != 0) {
            evm.revert("erc721: Non existent token")
        } else {
            evm
                .setState(evm.target, Erc721(result._1.ok))
                .emit(
                    EventTransferErc721({
                        sender: 0,
                        recipient: 0,
                        tokenId: _tokenId
                    })
                )
        }
    }

    pure def _requireOwned(
        state: types::erc721::State,
        tokenId: Uint,
    ): Result[Address] = {
        val owner = _ownerOf(state, tokenId)
        if (owner == 0) {
            failure(owner, "erc721: non existent token")
        } else {
            success(owner)
        }
    }

    pure def _isAuthorized(
        owner: Address,
        spender: Address, 
        tokenId: Uint,
    ): bool = {
        spender != 0 and owner == spender 
    }

    pure def _checkAuthorized(
        owner: Address,
        spender: Address, 
        tokenId: Uint,
    ): str = {
        if (not(_isAuthorized(owner, spender, tokenId))) {
            if (owner == 0) {
                "erc721: non existent token"
            } else {
                "erc721: insufficient approval"
            }
        } else {
            ""
        }
    }
    
}
