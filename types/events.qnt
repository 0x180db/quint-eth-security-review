module types::events {
    import types::common.* from "./common"
    import types::evm from "./evm"

    type Event =
        | EventTransferEther(types::evm::EventDataTransferEther)
}
