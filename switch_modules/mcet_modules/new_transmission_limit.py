"""
This module imposes a limit on total transmission additions during each period.
The limit is expressed as a fraction relative to the total amount of
transmission (MW * km) already existing in the system before the start of the
study. This module defines two new parameters and one expression:

`trans_expansion_limit_fraction[p in PERIODS]`: Provided by the user in
"new_transmission_limit.csv". Expressed as a fraction of existing transmission
(`trans_expansion_baseline_mw_km`, defined below). Setting this to zero will
prevent any new transmission; setting it to 1 would allow new construction in
period `p` of as much transmission as already exists at the start of the study.
The limit is set on the basis of total MW-km throughout the system, so it will
not limit transmission on individual corridors based on the quantity already in
place on that corridor.

`trans_expansion_baseline_mw_km`: Total MW-km of transmission in the power
system at the start of the study. Calculated from transmission corridor lengths
and capacities given in transmission_lines.csv.

`TxTotalMWkmAdded[p in PERIODS]`: Total MW-km of transmission added in period
`p`, calculated as the sum of `BuildTx[tx, p] * trans_length_km[tx]` for all
corridors `Tx` in `TRANSMISSION_LINES`.

This module also defines a constraint that requires that
`TxTotalMWkmAdded[p] <= trans_expansion_limit_fraction * trans_expansion_baseline_mw_km[p]`
for every period `p`.
"""

import os
from pyomo.environ import *


def define_components(m):
    # Maximum amount of transmission that can be added per year, as a fraction
    # relative to the total capacity (MW-km) in place at the start of the study
    m.trans_expansion_limit_fraction = Param(m.PERIODS, within=NonNegativeReals)

    # Total MW-km of transmission in place at start
    m.trans_expansion_baseline_mw_km = Param(
        within=NonNegativeReals,
        rule=lambda m: sum(
            m.existing_trans_cap[tx] * m.trans_length_km[tx]
            for tx in m.TRANSMISSION_LINES
        ),
    )

    # Total MW-km of transmission added in each period
    m.TxTotalMWkmAdded = Expression(
        m.PERIODS,
        rule=lambda m, p: sum(
            m.BuildTx[tx, p] * m.trans_length_km[tx] for tx in m.TRANSMISSION_LINES
        ),
    )

    # Ensure m.TxTotalMWkm[p] <= m.trans_limit_mw_km[p] for each period.
    m.Limit_Transmission_Expansion = Constraint(
        m.PERIODS,
        rule=lambda m, p: m.TxTotalMWkmAdded[p]
        <= m.trans_expansion_limit_fraction[p] * m.trans_expansion_baseline_mw_km,
    )


def load_inputs(m, switch_data, inputs_dir):
    """
    Import the cap on new transmission (as fraction of existing total MW-km)
    in each period.

    new_transmission_limit.csv
        PERIOD, trans_expansion_limit_fraction
    """

    switch_data.load_aug(
        filename=os.path.join(inputs_dir, "new_transmission_limit.csv"),
        param=(m.trans_expansion_limit_fraction,),
    )
