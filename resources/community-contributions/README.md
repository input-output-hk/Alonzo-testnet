# Community Contributions

This area is provided for community contributions.


*   [A general purpose oracle for JSON data](https://github.com/functionally/mantis-oracle/blob/main/ReadMe.md): This Plutus oracle reports structured data (namely, the `PlutuxTx.BuiltinData` type, which encompasses any JSON data that can be serialized as CBOR) to a transaction if the fee, as a quantity of a fungible token and/or ADA, is paid. It can be incorporated into other smart-contracts that use the oracle's value in their validation logic. A command-line tool for creating, writing, and deleting the oracle is provided, as is an extended example of the Oracle's use. The oracle also supports the Plutus Application Backend (PAB) and simulation. The diagram below shows a simple use case involving creating, reading, and writing the oracle.
<img alt="Example usage of the JSON oracle." src="https://raw.githubusercontent.com/functionally/mantis-oracle/main/transactions.png" width="600" align="right"/>
