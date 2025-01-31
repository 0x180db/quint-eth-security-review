module types::erc721 {
    import types::common.* from "./common"

    type State = {
        symbol: str,
        owners: Uint -> Address,
        balances: Address -> Uint,
    }

    type EventDataTransferErc721 = {
        sender: Address,
        recipient: Address,
        tokenId: Uint,
    }
}
