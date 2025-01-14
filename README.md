# Quint Ethereum Security Review

This repository contains a specification structure and set of [Quint](https://quint-lang.org/) modules for [Solidity](https://soliditylang.org/) smart contract security review.

## How to start security review with this template

1. Clone this repository
2. Create a new module in the `root` directory
3. Write your specification
4. Use the `types` directory to define custom types and events
5. Write tests in the `tests` module and run them to avoid trivial mistakes
6. Define `safety` and `liveness` properties in the `main` module
7. Run the model checker and find vulnerabilities!

## How to run model checker

```bash
quint run --invariant=safetyOk main.qnt
```

or symbolic execution via [Apalache](https://apalache-mc.org/)

```bash
quint verify --temporal=livenessOk main.qnt
```

## Follow me
More info about Quint and Formal Verification 

- Blog: [https://0x180db.dev](https://0x180db.dev)
- X (ex Twitter): [https://x.com/0x180db](https://x.com/0x180db)
