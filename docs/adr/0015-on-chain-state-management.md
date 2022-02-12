# 15. On-Chain State Management

Date: 2022-02-04

## Status

:hammer_and_wrench:

## Context

The Hydra head protocol requires a few on-chain interactions. Those are governed by on-chain smart validator scripts which ensures the correct execution of the Hydra head state-machine. Each transition of the state machine takes the form of a transaction on-chain. As part of the Hydra nodes, we have to observe the chain and react to transitions accordingly, while maintaining a state across all distributed Hydra nodes.

## Decision

We can observe all transactions involved in the head life-cycle by solely looking at those transactions and some credentials known of the node (i.e. a verification key). One interesting detail to notice is how any transition can be observed _by itself_, out of any context. This is actually consequence of Cardano's smart-contract design which forces transactions validations to be deterministic and solely dependent on the surrounding transaction. This means that it is possible to write a pure function for observing Hydra On-Chain State Machine _transitions_.

## Consequence

* The direct chain component does not need to maintain some ad-hoc state and can get away with maintaining a simple list of observed transactions (or transitions).

- Accessing information from the state now requires a bit more of CPU since we pretty much need to refold the state for any request. This may not be much of a problem in practice because the sequence of transactions is relatively small (even in the worse case, it'll be about folding over a hundred of entries). It however simplifies a great deal the representation and maintenance of that the on-chain head state.

- The _transition_ can serve as a replacement for the `OnChainTx` but contains more details than mere `OnChainTx`. Some of those details are however irrelevant to the head logic; so we may still want to define some transformation `Transition -> OnChainTx` to strip out noise before passing the result to the head logic callback.

- In the light of upcoming work stream, we would want to also keep track of the slots at which transactions were found in blocks (i.e. `[(SlotNo, Tx)]`) to enable an easy(ier) management of rollbacks later on. This approach is very similar to [event-sourcing](https://docs.microsoft.com/en-us/azure/architecture/patterns/event-sourcing) where transactions play the role of events. Rolling back the state becomes as simple as dropping transactions beyond the point of rollback.
