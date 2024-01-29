# Spot Order specification

Spot Order is a simple swap order that can be configured in a way to mimic either Market or Limit order.

Validators for this type of order are distinguished by the type of assets exchanged:
- native-to-token: ADA exchanged for some token
- token-to-native: some token exchanged for ADA
- token-to-token: some token exchanged for another token

## native-to-token

This type of validator stores the following variables in its datum:\
$In_t$ - Remaining amount of tradable input
$In_f$ - Remaining amount of execution fee

And immutable parameters:\
$C$ - budget per execution step (amount that executor can use to cover TX fee at each execution step)
$P$ - minimal exchange price

Validation of exchange invariant boils down to:
$\left\{\begin{equation} 
  \begin{array}{l}
    \Delta Out \geq \Delta In_t * P &\\
    \Delta In_f \leq \frac{\Delta In_t * In_{f^0}}{In_{t_0}} &\\
    \Delta In \leq \Delta In_t + \Delta In_f + C &\\
  \end{array}
\end{equation}\right.$
, where \
$\Delta Out = Out_1 - Out_0$, \
$\Delta In = In_0 - In_1$, \
$\Delta In_t = In_{t_0} - In_{t_1}$, \
$\Delta In_f = In_{f_0} - In_{f_1}$