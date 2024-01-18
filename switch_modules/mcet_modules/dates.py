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
    Adds components to a Switch model to group timepoints into dates. This is
    needed by the MCET flexible_loads and hydrogen modules.

    `tp_date[tp]`: date ID for timepoint `tp`, read from tp_dates.csv. If not
    provided, we use the timeseries that holds this timepoint (`tp_ts[tp]`).
    This should be provided if multiple days (e.g. a full year) are treated as a
    single timeseries and you use the hydrogen module or if you use the
    `flexible_loads` module and specify a non-zero value for
    `daily_shiftable_load_addition`.

    `DATES`: set of all dates used in this model, baed on `tp_date`. Will be
    equivalent to `TIMESERIES` if `tp_date` is not provided.

    `TPS_IN_DATE[d in DATES]`: indexed set of all TIMEPOINTS that fall in DATE
    d, derived from `tp_date`. Will be equivalent to `TPS_IN_TS[ts in
    TIMESERIES]` if `tp_date` is not provided.
    """

    # Use a custom timepoint -> date mapping if tp_dates.csv is provided.
    # Otherwise just use the timeseries as the date ID (more common). Custom
    # dates can be useful when running a single timeseries for the whole year,
    # but you still need to identify dates within the year for the flexible load
    # calculations.
    m.tp_date = Param(m.TIMEPOINTS, default=lambda m, tp: m.tp_ts[tp], within=Any)
    m.DATES = Set(
        initialize=lambda m: unique_list(m.tp_date[tp] for tp in m.TIMEPOINTS)
    )

    # Efficiently construct the set of timepoints within each date
    def TPS_IN_DATE_init(m, d):
        try:
            dd = m.TPS_IN_DATE_dict
        except AttributeError:
            # haven't constructed the date dictionary yet (first call to this
            # function); build it now
            dd = m.TPS_IN_DATE_dict = dict()
            for tp in m.TIMEPOINTS:
                # create an empty list for the date holding this timepoint (if
                # needed), then append this timepoint
                dd.setdefault(m.tp_date[tp], []).append(tp)
        return dd.pop(d)  # use pop to free memory as we go

    m.TPS_IN_DATE = Set(m.DATES, initialize=TPS_IN_DATE_init)

    # Efficiently construct the set of dates within each timeseries
    # If any date spans multiple timeseries, it will be an error
    def DATES_IN_TS_init(m, ts):
        try:
            dd = m.DATES_IN_TS_dict
        except AttributeError:
            # haven't constructed the date dictionary yet (first call to this
            # function); build it now
            dd = m.DATES_IN_TS_dict = dict()
            date_ts = dict()  # cache of already known ts for each date
            for tp in m.TIMEPOINTS:
                d = m.tp_date[tp]
                _ts = m.tp_ts[tp]
                if _ts != date_ts.setdefault(d, _ts):
                    # was previously assigned to a different date
                    raise ValueError(
                        "Timepoints from different timeseries have been "
                        f"assigned to the same date {d} in tp_dates.csv. "
                        "This is not allowed."
                    )
                # create a new list for the timeseries holding this
                # timepoint (if needed), then add this date
                dd.setdefault(_ts, []).append(d)
        # return all unique dates for this timeseries
        return unique_list(dd.pop(ts))

    m.DATES_IN_TS = Set(m.TIMESERIES, initialize=DATES_IN_TS_init)


def load_inputs(m, switch_data, inputs_dir):
    """
    The following files is used if available in the input directory:

    tp_dates.csv (optional)
        TIMEPOINT, tp_date

    Note that the tp_date column is optional because it has default values, so
    you will not receive an error message if it is  missing or misnamed (or if
    tp_dates.csv is missing or misnamed). So it is a good idea to verify that
    the model is producing the output you expect by checking the results in the
    outputs directory.
    """

    switch_data.load_aug(
        optional=True,
        filename=os.path.join(inputs_dir, "tp_dates.csv"),
        param=(m.tp_date,),
    )
