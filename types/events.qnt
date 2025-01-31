module types::events {
    import types::common.* from "./common"
    import types::evm from "./evm"
    import types::erc20 from "./erc20"
    import types::erc721 from "./erc721"

    type Event =
        | EventTransferEther(types::evm::EventDataTransferEther)
        | EventTransferErc20(types::erc20::EventDataTransferErc20)
        | EventTransferErc721(types::erc721::EventDataTransferErc721)
}
