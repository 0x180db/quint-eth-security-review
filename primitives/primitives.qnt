module primitives {
    import primitives::evm.* from "./evm"
    export primitives::evm as evm

    import primitives::erc20 from "./erc20"
    export primitives::erc20 as erc20
}
