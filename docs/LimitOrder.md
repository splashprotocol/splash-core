# Limit Order specification

Limit order allows to exchange one asset for another at a specified (or better) rate.

The order is implemented as two types of validators:
- Batch validator that must be present in each TX spending at least one order UTxO. Validates execution of all limit orders in transaction.
- Spending validator each order UTxO is guarded with. Either validates cancellation of the order or delegates validation to the batch validator.

Let $Lovelace$ denote Lovelace, $In$ denote output asset, $Out$ denote output asset.

Each order UTxO carries the following state variables in its datum:
* $In_t$ - Remaining amount of tradable input (in input asset units)
* $Lovelace_f$ - Remaining amount of execution fee (in Lovelace units)

And immutable parameters:
* $C$ - budget per execution step (amount that executor can use to cover TX fee at each execution step)
* $P$ - minimal exchange rate (Output/Input)

Limit order has three types of transitions:
1. Cancellation. An order can be cancelled at any time by an authorized party.
2. Partial fill. Part of $In_t$ is removed in exchange for $Out$. Structure of the order is preserved: address, immutable datum fields.
3. Terminal fill. Remainder of $In_t$ is removed in exchange for $Out$. An output to configured redeemer address is created.

A valid exchange must satisfy the following constraints:
$\left\{\begin{equation} 
  \begin{array}{l}
    \Delta Out \geq -\Delta In_t * P \text{ if Out} \neq \text{Lovelace} &\\
    \Delta Out + \Delta Lovelace_f - C \geq -\Delta In_t * P \text{ if Out} = \text{Lovelace} &\\
    \Delta Lovelace_f \leq \frac{\Delta In_t * Lovelace_{f^0}}{In_{t_0}} &\\
    \Delta In \geq \Delta In_t + \Delta In_f - C \text{ if In} = \text{Lovelace} &\\
    \Delta In \geq \Delta In_t \text{ if In} \neq \text{Lovelace} &\\
    \Delta Lovelace \geq \Delta In_f - C \text{ if In} \neq \text{Lovelace} \text{ and Out} \neq \text{Lovelace} &\\
  \end{array}
\end{equation}\right.$
where: \
$\Delta Out = Out_1 - Out_0$, \
$\Delta In = In_1 - In_0$, \
$\Delta Lovelace = Lovelace_1 - Lovelace_0$, \
$\Delta In_t = In_{t_1} - In_{t_0}$, \
$\Delta Lovelace_f = Lovelace_{f_1} - Lovelace_{f_0}$