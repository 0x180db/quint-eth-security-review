module primitives {
    import primitives::evm.* from "./evm"
    export primitives::evm as evm

    import primitives::erc20 from "./erc20"
    export primitives::erc20 as erc20

    import primitives::erc721 from "./erc721"
    export primitives::erc721 as erc721
}
