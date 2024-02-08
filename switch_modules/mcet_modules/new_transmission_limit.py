"""
This module imposes a limit on total transmission additions during each period.
The limit is expressed as a fraction relative to the total amount of
transmission (MW * km) already existing in the system before the start of the
study. This module defines the following parameters and expression:

`trans_expansion_baseline_mw_km`: Total MW-km of transmission in the power
system at the start of the study. Calculated from transmission corridor lengths
and capacities given in transmission_lines.csv.

`TxTotalMWkmAdded[p in PERIODS]`: Expression showing total MW-km of transmission
added in period `p`, calculated as the sum of `BuildTx[tx, p] *
trans_length_km[tx]` for all corridors `Tx` in `TRANSMISSION_LINES`.

`trans_expansion_limit_fraction[p in PERIODS]`: Can be provided by the user in
"new_transmission_limit.csv". Expressed as a fraction of existing transmission
(`trans_expansion_baseline_mw_km`, defined below). Setting this to zero will
prevent any new transmission; setting it to 1 would allow new construction in
period `p` of as much transmission as already exists at the start of the study.
The limit is set on the basis of total MW-km throughout the system, so it will
not limit transmission on individual corridors based on the quantity already in
place on that corridor. If this column is omitted or a value is missing ("."),
it will be treated as an infinite limit.

`trans_expansion_limit_mw_km[p in PERIODS]`: Can be provided by the user in
"new_transmission_limit.csv". Setting this to zero will prevent any new
transmission; setting it to a non-zero value limits total transmission expansion
in period `p` to the specified amount, in MW * km (MW of transfer capability *
km length of corridor). If this column is omitted or a value is missing ("."),
it will be treated as an infinite limit.

If both trans_expansion_limit_fraction and trans_expansion_limit_mw_km are
specified, both limits will be enforced, i.e., the lower one will take effect.

It can be helpful to run Switch with `--input-alias
new_transmission_limit.csv=none --save-expression TxTotalMWkmAdded` flags to
report how much transmission would be built without a cap. This value will be
saved in <outputs-dir>/TxTotalMWkmAdded.csv. Then you can construct a
<inputs-dir>/new_transmission_limit.csv file with the
trans_expansion_limit_mw_km column equal to some fraction of the baseline
additions, e.g., 0.5 * TxTotalMWkmAdded.
"""

import os
from pyomo.environ import *


def define_components(m):
    # Maximum amount of transmission that can be added per period, as a fraction
    # relative to the total capacity (MW-km) in place at the start of the study
    m.trans_expansion_limit_fraction = Param(
        m.PERIODS, within=NonNegativeReals, default=float("inf")
    )

    # Maximum amount of transmission that can be added per period, expressed in
    # units of MW * km
    m.trans_expansion_limit_mw_km = Param(
        m.PERIODS, within=NonNegativeReals, default=float("inf")
    )

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

    # Enforce trans_expansion_limit_fraction if provided
    m.Limit_Transmission_Expansion_Fraction = Constraint(
        m.PERIODS,
        rule=lambda m, p: (
            Constraint.Skip
            if m.trans_expansion_limit_fraction[p] == float("inf")
            else (
                m.TxTotalMWkmAdded[p]
                <= m.trans_expansion_limit_fraction[p]
                * m.trans_expansion_baseline_mw_km
            )
        ),
    )

    # Enforce trans_expansion_limit_mw_km if provided
    m.Limit_Transmission_Expansion_MWkm = Constraint(
        m.PERIODS,
        rule=lambda m, p: (
            Constraint.Skip
            if m.trans_expansion_limit_mw_km[p] == float("inf")
            else (m.TxTotalMWkmAdded[p] <= m.trans_expansion_limit_mw_km[p])
        ),
    )


def load_inputs(m, switch_data, inputs_dir):
    """
    Import the cap on new transmission (as fraction of existing total MW-km)
    in each period.

    new_transmission_limit.csv
        PERIOD, trans_expansion_limit_fraction, trans_expansion_limit_mw_km
    """

    switch_data.load_aug(
        filename=os.path.join(inputs_dir, "new_transmission_limit.csv"),
        optional=True,
        param=(m.trans_expansion_limit_fraction, m.trans_expansion_limit_mw_km),
    )
