module types::evm {
    import types::common.* from "./common"
    
    type EventDataTransferEther = {
        sender: Address,
        recipient: Address,
        amount: Uint,
    }
}
