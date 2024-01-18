import os
from pyomo.environ import (
    Set,
    Constraint,
    Var,
    Param,
    Expression,
    Reals,
    NonNegativeReals,
    Any,
)
from switch_model.utilities import unique_list


def define_components(m):
    """
    Adds components to a Switch model to implement several flexible load
    options.

    The model also requires the `dates` module.

    Flexible load options are specified in `flexible_loads.csv` in the inputs
    directory. This file contains one row for each set of zones and period when
    flexible loads are added. Each column defines one type of load to be added
    to those zones during that period.

    The first two columns identify the set of zones and the period where the
    adjustments should be applied. The first column, `ZONES` can be either a
    list of zones separated with semicolons ("North;East;South"), an individual
    zone ("North") or "all" to specify it should be applied to all zones.

    The remaining columns define the load adjustment rules, with one value for
    that zone set and study period. Each *_addition parameter (described in more
    detail below) specifies an addition of a particular type of load to the
    system in addition to the pre-existing `zone_demand_mw` defined in
    `loads.csv`. They are all defined as a decimal fraction relative to the
    total amount of `zone_demand_mw` for the year in that set of zones. In other
    words, they each define an additional total demand for electricity that is
    equal to the specified fraction times the total pre-existing demand for that
    set of zones for the period. Switch first allocates this total load addition
    among the zones in the zone set. It then allocates the addition for each
    zone among the timepoints of the period, subject to the rules outlined
    below.

    Values for `standard_load_addition` can be positive or negative (to define a
    load reduction) for, but all other adjustment columns must be 0 or positive.
    If not provided, these parameters have a default value of 0, which will have
    no effect on the model.

    `standard_load_addition[zs, p]`: Add the specified multiple of
    `zone_demand_mw[z, tp]` to all the zones `z` within the zone set `zs` during
    each timepoint `tp` in period `p`. This can positive or negative. It
    increases or decreases the standard load in each zone without changing its
    shape. Note that the same fraction is used for all zones in the zone set and
    timepoints in the period; Switch does not optimize the location of the
    additions or subtractions. Setting `standard_load_addition` to -1 would
    exactly cancel the `zone_demand_mw` in all zones in the zone set and setting
    it to 1 would double the `zone_demand_mw` in all zones.

    `baseload_load_addition[zs, p]`: This adds baseload power (constant all
    year) to the system. Switch optimally places baseload blocks in all zones in
    the zone set `zs`. The baseload blocks will have the same total MWh during
    the period as the sum of `zone_demand_mw` across all zones in the zone set.

    `daily_shiftable_load_addition[zs, p]`: This is used to add load that can be
    shifted within the day. Switch optimizes the location of this demand among
    zones as noted above. It also optimizes the timing of the demand, subject to
    a requirement that the total load added on each day in each zone must equal
    that zone's share of the total added on that day for the whole zone set. As
    an example, to make 30% of the standard load reschedulable within the same
    day within each zone, you could create one rule for each zone, with
    `standard_load_addition` = -0.3 and `daily_shiftable_load_addition` = 0.3 .

    `annual_shiftable_load_addition[zs, p]`: This is used to add load that can
    be shifted within the year. Switch optimizes the timing and location of the
    new demand, subject only to a requirement that the total load added in all
    timepoints in all zones in the zone set must equal the specified fraction of
    pre-existing load for the period for the whole zone set. This could be used,
    for example, to add load that can be shifted to any time of year but with a
    minimum utilization rate of 35% (specified as 0.35 in
    `annual_shiftable_load_min_cf`).

    `annual_shiftable_load_min_cf[z, p]`: Minimum utilization rate to apply to
    the annual shiftable load. This adds a constraint to the annual shiftable
    load, such that the minimum level of the new load in each zone in the zone
    set must be greater than or equal to the specified fraction of the peak
    level of the new load in that zone. (In future work, we could add a minimum
    load factor for the year [avg/peak] as well or instead.)

    Also see switch_model.balancing.demand_response.simple for a simpler example
    of flexible demand (that is equivalent to using
    daily_shiftable_load_addition with individual zones in each zone set, so
    load cannot be shifted across zones).
    """

    # sets of zones for which flexible load rules have been provided; loads
    # other than standard_load_addition will be allocated optimally across zones
    # in the set. Identifier for a zone set can be "all", "Zone1;Zone2" or "Zone1"
    m.FL_ZONE_SET_PERIODS = Set(
        dimen=2, within=Any, validate=lambda m, zs, p: p in m.PERIODS
    )

    # list of load zones in each zone set
    m.ZONES_IN_FL_ZONE_SET_PERIOD = Set(
        m.FL_ZONE_SET_PERIODS,
        within=m.LOAD_ZONES,
        # Convert zone set text identifier ("all", "Zone1;Zone2" or "Zone1")
        # into a list of zones.
        initialize=lambda m, zs, p: list(m.LOAD_ZONES)
        if zs.lower() in {"all", "."}
        else [z.strip() for z in zs.split(";")],
    )

    # set of all valid zone set - period - zone combinations; used to
    # split the required total load across zones
    m.FL_ZONE_SET_PERIOD_ZONES = Set(
        dimen=3,
        initialize=lambda m: [
            (zs, p, z)
            for (zs, p) in m.FL_ZONE_SET_PERIODS
            for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, p]
        ],
    )

    # set of all valid zone set - zone - timepoint combinations
    # Used to tallocate the loads defined for each period - zone set combo
    # into individual zones during individual timepoints
    m.FL_ZONE_SET_ZONE_TIMEPOINTS = Set(
        dimen=3,
        initialize=lambda m: [
            (zs, z, tp)
            for (zs, p) in m.FL_ZONE_SET_PERIODS
            for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, p]
            for tp in m.TPS_IN_PERIOD[p]
        ],
    )

    # set of all valid zone set - zone - date combinations
    # Used to enforce rules for daily flexbile loads
    m.FL_ZONE_SET_ZONE_DATES = Set(
        dimen=3,
        initialize=lambda m: [
            (zs, z, d)
            for (zs, p) in m.FL_ZONE_SET_PERIODS
            for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, p]
            for ts in m.TS_IN_PERIOD[p]
            for d in m.DATES_IN_TS[ts]
        ],
    )

    # indexed set to lookup all the zone_sets that include the specified load
    # zone in the specified period; used to aggregate flexible load by zone
    def init(m, z, p):
        try:
            d = m.FL_ZONE_SETS_FOR_ZONE_PERIOD_dict
        except AttributeError:
            d = m.FL_ZONE_SETS_FOR_ZONE_PERIOD_dict = {
                (z2, p2): [] for z2 in m.LOAD_ZONES for p2 in m.PERIODS
            }
            for zs, p2, z2 in m.FL_ZONE_SET_PERIOD_ZONES:
                d[(z2, p2)].append(zs)
        return d.pop((z, p))

    m.FL_ZONE_SETS_FOR_ZONE_PERIOD = Set(m.LOAD_ZONES, m.PERIODS, initialize=init)

    # calculate total of zone_demand_mw for each period and zone set; used in
    # some of the flexible load calculations later (it's much more efficient to
    # calculate it once than to repeat the calculation for each timepoint in the
    # constraints below)
    m.zone_set_period_total_load = Param(
        m.FL_ZONE_SET_PERIODS,
        rule=lambda m, zs, p: weighted_sum(m, m.zone_demand_mw, zs, p),
    )
    m.hours_in_period = Param(
        m.PERIODS, rule=lambda m, p: sum(m.tp_weight[tp] for tp in m.TPS_IN_PERIOD[p])
    )

    # Add a multiple of standard loads (zone_demand_mw)
    m.standard_load_addition = Param(m.FL_ZONE_SET_PERIODS, within=Reals, default=0.0)

    # no decisionmaking here, so we just define an expression for the load adjustment
    # note: this could be a Param, since it only depends on Params, but usually
    # load adjustments are Vars or Expressions, so we use an Expression here for
    # consistency.
    m.FlexibleLoadStandard = Expression(
        m.FL_ZONE_SET_ZONE_TIMEPOINTS,
        rule=lambda m, zs, z, tp: m.standard_load_addition[zs, m.tp_period[tp]]
        * m.zone_demand_mw[z, tp],
    )
    register_flexible_load(m.FlexibleLoadStandard)

    # add some baseload, allocated across zones
    m.baseload_load_addition = Param(
        m.FL_ZONE_SET_PERIODS, within=NonNegativeReals, default=0.0
    )
    # allocate the addition across zones
    m.FlexibleBaseloadZoneAllocation = Var(
        m.FL_ZONE_SET_PERIOD_ZONES, within=NonNegativeReals
    )
    # Require the right total amount of additions
    m.Enforce_Baseload_Total = Constraint(
        m.FL_ZONE_SET_PERIODS,
        rule=lambda m, zs, p: sum(
            m.FlexibleBaseloadZoneAllocation[zs, p, z]
            for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, p]
        )
        == m.baseload_load_addition[zs, p],
    )
    # calculate the extra load in each zone in each timepoint
    # (baseload addition for this zone * average standard load in this zone)
    m.FlexibleLoadBaseload = Expression(
        m.FL_ZONE_SET_ZONE_TIMEPOINTS,
        rule=lambda m, zs, z, tp: m.FlexibleBaseloadZoneAllocation[
            zs, m.tp_period[tp], z
        ]
        * m.zone_set_period_total_load[zs, m.tp_period[tp]]
        / m.hours_in_period[m.tp_period[tp]],
    )
    # add this load to the system
    register_flexible_load(m.FlexibleLoadBaseload)

    # Load that can be shifted within each day.
    m.daily_shiftable_load_addition = Param(
        m.FL_ZONE_SET_PERIODS, within=NonNegativeReals, default=0.0
    )
    # allocate the addition across zones
    m.FlexibleDailyShiftableZoneAllocation = Var(
        m.FL_ZONE_SET_PERIOD_ZONES, within=NonNegativeReals
    )
    # Require the right total amount of additions
    m.Enforce_DailyShiftable_Total = Constraint(
        m.FL_ZONE_SET_PERIODS,
        rule=lambda m, zs, p: sum(
            m.FlexibleDailyShiftableZoneAllocation[zs, p, z]
            for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, p]
        )
        == m.daily_shiftable_load_addition[zs, p],
    )
    # decision variable for how much daily shiftable load to add in each hour
    m.FlexibleLoadDailyShiftable = Var(
        m.FL_ZONE_SET_ZONE_TIMEPOINTS, within=NonNegativeReals
    )
    # make this match the chosen share of total zone_demand_mw during each
    # date
    m.Enforce_Daily_Shiftable_Load_Total = Constraint(
        m.FL_ZONE_SET_ZONE_DATES,
        rule=lambda m, zs, z, d: (
            sum(m.FlexibleLoadDailyShiftable[zs, z, tp] for tp in m.TPS_IN_DATE[d])
            == sum(
                m.FlexibleDailyShiftableZoneAllocation[zs, m.tp_period[tp], z]
                * sum(
                    m.zone_demand_mw[z2, tp]
                    for z2 in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, m.tp_period[tp]]
                )
                for tp in m.TPS_IN_DATE[d]
            )
        ),
    )
    # add this load to the system
    register_flexible_load(m.FlexibleLoadDailyShiftable)

    # loads that can be scheduled throughout the year with a minimum level per
    # timepoint Note: we currently only allow non-negative values for the
    # parameters; that could be changed if needed, but then we would probably
    # need to enforce sign consistency and adjust the range constraint.
    m.annual_shiftable_load_addition = Param(
        m.FL_ZONE_SET_PERIODS, within=NonNegativeReals, default=0.0
    )
    m.annual_shiftable_load_min_cf = Param(
        m.FL_ZONE_SET_PERIODS, within=NonNegativeReals, default=0.0
    )
    # allocate the addition across zones
    m.FlexibleAnnualShiftableZoneAllocation = Var(
        m.FL_ZONE_SET_PERIOD_ZONES, within=NonNegativeReals
    )
    # Require the right total amount of additions
    m.Enforce_AnnualShiftable_Total = Constraint(
        m.FL_ZONE_SET_PERIODS,
        rule=lambda m, zs, p: sum(
            m.FlexibleAnnualShiftableZoneAllocation[zs, p, z]
            for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zs, p]
        )
        == m.annual_shiftable_load_addition[zs, p],
    )
    # level of annual shiftable load in each zone during each timepoint
    m.FlexibleLoadAnnualShiftable = Var(
        m.FL_ZONE_SET_ZONE_TIMEPOINTS, within=NonNegativeReals
    )
    # highest level of annual shiftable load during the period in each zone
    m.FlexibleLoadAnnualShiftablePeak = Var(
        m.FL_ZONE_SET_PERIOD_ZONES, within=NonNegativeReals
    )
    # require load to fall in the allowed range
    m.Enforce_Annual_Shiftable_Load_Lower_Limit = Constraint(
        m.FL_ZONE_SET_ZONE_TIMEPOINTS,
        rule=lambda m, zs, z, tp: (
            m.FlexibleLoadAnnualShiftable[zs, z, tp]
            >= m.annual_shiftable_load_min_cf[zs, m.tp_period[tp]]
            * m.FlexibleLoadAnnualShiftablePeak[zs, m.tp_period[tp], z]
        ),
    )
    m.Enforce_Annual_Shiftable_Load_Upper_Limit = Constraint(
        m.FL_ZONE_SET_ZONE_TIMEPOINTS,
        rule=lambda m, zs, z, tp: (
            m.FlexibleLoadAnnualShiftable[zs, z, tp]
            <= m.FlexibleLoadAnnualShiftablePeak[zs, m.tp_period[tp], z]
        ),
    )
    # make this equal to the chosen share of total zone_demand_mw during each
    # period, in each zone
    m.Enforce_Annual_Shiftable_Load_Total = Constraint(
        m.FL_ZONE_SET_PERIOD_ZONES,
        rule=lambda m, zs, p, z: (
            sum(
                m.FlexibleLoadAnnualShiftable[zs, z, tp] * m.tp_weight[tp]
                for tp in m.TPS_IN_PERIOD[p]
            )
            == m.FlexibleAnnualShiftableZoneAllocation[zs, p, z]
            * m.zone_set_period_total_load[zs, p]
        ),
    )
    # add this load to the system
    register_flexible_load(m.FlexibleLoadAnnualShiftable)


def register_flexible_load(component):
    """
    Calculate total flexible load for each zone in each timepoint and add it to
    the model

    note: each flexible load component is indexed by zone_set, zone and
    timepoint, and uses the rules for that period and zone_set. So we cross-
    reference to find the relevant zone_sets for the current zone and period.
    """
    m = component.model()
    comp_name = component.name
    new_comp_name = comp_name + "Zonal"
    zonal_total = Expression(
        m.LOAD_ZONES,
        m.TIMEPOINTS,
        rule=lambda m, z, tp: sum(
            getattr(m, comp_name)[zs, z, tp]
            for zs in m.FL_ZONE_SETS_FOR_ZONE_PERIOD[z, m.tp_period[tp]]
        ),
    )
    setattr(m, new_comp_name, zonal_total)

    # Add the specified load to the distribution system if this model uses local
    # T&D calculations, otherwise to the main node.
    try:
        m.Distributed_Power_Withdrawals.append(new_comp_name)
    except AttributeError:
        m.Zone_Power_Withdrawals.append(new_comp_name)


def weighted_sum(m, var, zone_set, period):
    """
    Return the weighted sum of the specified variable across all zones and
    timepoints in the specified period and zone set.
    """
    return sum(
        var[z, tp] * m.tp_weight[tp]
        for z in m.ZONES_IN_FL_ZONE_SET_PERIOD[zone_set, period]
        for tp in m.TPS_IN_PERIOD[period]
    )


def load_inputs(m, switch_data, inputs_dir):
    """
    Import data describing flexible load options. The following file is expected
    in the input directory:

    flexible_loads_zonal.csv
        PERIOD, standard_load_addition, daily_shiftable_load_addition,
        baseload_load_addition, annual_shiftable_load_addition,
        annual_shiftable_load_min_cf

    Note that all of these columns are optional because they have default
    values, so you will not receive an error message if they are missing or
    misnamed (or if tp_dates.csv is missing or misnamed). So it is a good idea
    to verify that the model is producing the output you expect by checking the
    results in load_balance.csv in the outputs directory.
    """

    switch_data.load_aug(
        filename=os.path.join(inputs_dir, "flexible_loads.csv"),
        index=m.FL_ZONE_SET_PERIODS,
        param=(
            m.standard_load_addition,
            m.daily_shiftable_load_addition,
            m.baseload_load_addition,
            m.annual_shiftable_load_addition,
            m.annual_shiftable_load_min_cf,
        ),
    )

    switch_data.load_aug(
        optional=True,
        filename=os.path.join(inputs_dir, "tp_dates.csv"),
        param=(m.tp_date,),
    )
