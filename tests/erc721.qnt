module tests::erc721 {
    import types.* from "../types/types"
    import primitives.* from "../primitives/primitives"
    import tests::setup.* from "./setup"

    run erc721::okTransferTest = {
        val initialOwners = Map(1 -> "alice")
        val initialBalances = Map(zeroAddress ->0, "alice" -> 1, "bob" -> 0)

        val token = erc721::construct(
            "NFT",
            initialBalances,
            initialOwners
        )

        val result = erc721::transferFrom(token, "alice", "bob", 1)

        if (isOk(result)) {
            val nextToken = result.ok
            and {
                assert(nextToken.balances.get("alice") == 0),
                assert(nextToken.balances.get("bob") == 1),
                assert(nextToken.owners.get(1) == "bob"),
            }
        } else {
            assert(false)
        }
    }

    run erc721::okMintTest = {
        val initialOwners = Map()
        val initialBalances = Map(zeroAddress -> 0, "alice" -> 0)

        val token = erc721::construct(
            "NFT",
            initialBalances,
            initialOwners
        )

        val result = erc721::mint(token, "alice", 1)

        if (isOk(result)) {
            val nextToken = result.ok
            and {
                assert(nextToken.balances.get("alice") == 1),
                assert(nextToken.owners.get(1) == "alice"),
                assert(erc721::totalSupply(nextToken) == 1),
            }
        } else {
            assert(false)
        }
    }

    run erc721::failTransferToZeroTest = {
        val initialOwners = Map().put(1, "alice")
        val initialBalances = Map(zeroAddress -> 0, "alice" -> 1)

        val token = erc721::construct(
            "NFT",
            initialBalances,
            initialOwners
        )

        val result = erc721::transferFrom(token, "alice", zeroAddress, 1)
        assert(isErr(result))
    }

    run erc721::failUnauthorizedTransferTest = {
        val initialOwners = Map().put(1, "alice")
        val initialBalances = Map(zeroAddress ->0, "alice" -> 1, "bob" -> 0)

        val token = erc721::construct(
            "NFT",
            initialBalances,
            initialOwners
        )

        val result = erc721::transferFrom(token, "bob", "charlie", 1)
        assert(isErr(result))
    }

    run erc721::failMintToZeroTest = {
        val token = erc721::construct(
            "NFT",
            Map(),
            Map()
        )

        val result = erc721::mint(token, zeroAddress, 1)
        assert(isErr(result))
    }

    run erc721::balanceOfTest = {
        val initialOwners = Map().put(1, "alice")
        val initialBalances = Map(zeroAddress -> 0, "alice" -> 1)

        val token = erc721::construct(
            "NFT",
            initialBalances,
            initialOwners
        )

        val result = erc721::balanceOf(token, "alice")
        assert(isOk(result) and result.ok == 1)
    }

    run erc721::ownerOfTest = {
        val initialOwners = Map().put(1, "alice")
        val initialBalances = Map(zeroAddress -> 0, "alice" -> 1)

        val token = erc721::construct(
            "NFT",
            initialBalances,
            initialOwners
        )

        val result = erc721::ownerOf(token, 1)
        assert(isOk(result) and result.ok == "alice")
    }
}
