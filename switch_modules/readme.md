This directory stores Switch model optional modules developed for the MCET
project. These can represent flexible demand, hydrogen and limits on new
transmission. There is an additional module that can be used to break long
timeseries into dates, which is needed when using daytime shiftable demand or
hydrogen with long timeseries (e.g., a single 8760-hour series for the year
instead of separate timeseries for each day).

To use a module, you can either add it to your modules.txt file (in the root
directory or in the inputs directory), or specify `--include-module <module
name>` on the command line or in options.txt or in scenarios.txt (if using
`switch solve-scenarios`). For simplicity in this readme, we assume you will
use the `--include-module` command on the command line.

You can make all these modules available for your model by copying the
`mcet_modules` directory from this folder to your main model folder (the outer
folder that contains your inputs directory; this is also the location where you
run `switch solve`). Once you do that, you can refer to these modules as
`mcet_modules.<something>` and Switch will find them, because Python
automatically looks for packages and modules in the directory where you run
`switch solve` or `switch solve-scenarios` from.

These modules are included:

- `mcet_modules.dates`: defines dates within each timeseries; must be loaded
  before `flexible_loads` or `hydrogen` module if your model has multi-day
  timeseries
- `mcet_modules.flexible_loads`: defines flexible loads that can be shifted in
  time and location
- `mcet_modules.hydrogen`: allows construction of stationary hydrogen storage
  facilities that produce hydrogen and later use it to produce electricity at
  another time of the year
- `mcet_modules.new_transmission_limit`: limits total transmission growth each
  period, expressed as a percentage of the total MW-km of transmission capacity
  in place at the start of the study


See the documentation at the top of each module for an explanation of how they
work and what capabilities they add to the model. Also see the `load_inputs`
function at the bottom of each module for a description of any data files that
they need in your inputs directory.

The `3zone_toy_inputs` directory in this folder contains example input data
files for all these modules. You can also use that to test the modules by
running `switch solve` in the current directory. Since `options.txt` in this
example directory specifies `--inputs-dir 3zone_toy_inputs`, you can run the
commands below to test the modules, either in this directory (with the sample
inputs) or in your directory (with your own inputs):

```
# flexible load that can be moved across zones
switch solve --outputs-dir outputs_flex_all_zones --include-modules mcet_modules.dates mcet_modules.flexible_loads

# flexible load that can be rescheduled but not moved between zones
switch solve --outputs-dir outputs_flex_by_zone --include-modules mcet_modules.dates mcet_modules.flexible_loads --input-alias flexible_loads.csv=flexible_loads_by_zone.csv

# hydrogen production and usage in each zone (useful in zero-emission scenarios)
switch solve --outputs-dir outputs_hydrogen --include-modules mcet_modules.dates mcet_modules.hydrogen

# limit transmission growth to 25% of starting level in each period
switch solve --outputs-dir outputs_trans_limit_25 --include-module mcet_modules.new_transmission_limit
```

You can also run `switch solve-scenarios` in this sample directory to run all
the scenarios listed above. This will read scenario definitions from
`scenarios.txt` and then run each one if it hasn't been run already.
`scenarios.txt` contains one line per scenario, with a scenario name and the
command line arguments to use when running that scenario. You can also run
`switch solve-scenarios --scenario <scenario_name>` to run an individual
scenario.


