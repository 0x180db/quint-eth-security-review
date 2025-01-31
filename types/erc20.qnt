module types::erc20 {
    import types::common.* from "./common"
    
    type State = {
        symbol: str,
        balances: Address -> Uint,
        totalSupply: Uint,
        decimals: Uint,
    }

    type EventDataTransferErc20 = {
        sender: Address,
        recipient: Address,
        amount: Uint,
    }
}
