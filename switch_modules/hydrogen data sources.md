Create a hydrogen_liquid.csv file for production and storage of liquid hydrogen

Regions without access to suitable salt caverns should use this version instead
of the cavern-based system represented in the original hydrogen.csv. Liquid
hydrogen tanks could potentially be built in any region.

Sources:

Kendall Mongird, Vilayanur Viswanathan, Jan Alam, Charlie Vartanian, Vincent Sprenkle, "2020 Grid Energy Storage Technology Cost and Performance Assessment", Pacific Northwest National Laboratory, Publication No. DOE/PA-0204, December 2020, https://www.pnnl.gov/sites/default/files/media/file/Hydrogen_Methodology.pdf

Chad A. Hunter, Michael M. Penev, Evan P. Reznicek, Joshua Eichman, Neha Rustagi, Samuel F. Baldwin, "Techno-economic analysis of long-duration energy storage and flexible power generation technologies to support high-variable renewable energy grids [Supplemental information]", Joule 5 (8): 2077-2101, August 2021. https://doi.org/10.1016/j.joule.2021.06.018

The Engineering ToolBox, "Fuels - Higher and Lower Calorific Values", [online] Available at: https://www.engineeringtoolbox.com/fuels-higher-calorific-values-d_169.html [Accessed 2024-02-07].

Argonne National Lab (ANL), "Hydrogen Delivery Scenario Analysis Model (HDSAM)" [online] Available at https://hdsam.es.anl.gov/index.php?content=hdsam [accessed 2024-02-07].

Elizabeth Connelly, Michael Penev, Amgad Elgowainy and Chad Hunter, "Current Status of Hydrogen Liquefaction Costs", DOE Hydrogen and Fuel Cells Program Record, August 6, 2019, https://www.hydrogen.energy.gov/docs/hydrogenprogramlibraries/pdfs/19001_hydrogen_liquefaction_costs.pdf

Wade A. Amos, "Costs of Storing and Transporting Hydrogen", NREL/TP-570-25106, November 1998, https://www.nrel.gov/docs/fy99osti/25106.pdf.

Notes:

hydrogen_electrolyzer_capital_cost_per_mw: 2030 moderate estimate from (Mongird, p. 5), includes electrolyzer (437$/kW), rectifier (94 $/kW), compressor (39.3 $/kW) and 50% of C&C and grid integration (1.06+16.3 $/kW)

hydrogen_electrolyzer_fixed_cost_per_mw_year: (Mongird, p. 5), excludes property tax, insurance, licensing and permitting

hydrogen_electrolyzer_kg_per_mwh: 72.5% HHV efficiency from (Hunter, Table S26) and 39.4 kWh/kg HHV from Engingeering Toolbox: (0.725 MWh H2 / MWh e-) (1000 kWh / MWh) (1 kg H2 / 39.4 kWh H2) = (18.40 kg H2 / MWh e-)

hydrogen_electrolyzer_life_years: (Mongird, p. 4) note: stack refurbishment is included in the O&M cost, so this is balance of plant

hydrogen_electrolyzer_variable_cost_per_kg: (Mongird, p. 5): 0.8 $/MWh for stack refurbishment plus 0.5 $/MWh for "basic variable O&M"

hydrogen_fuel_cell_capital_cost_per_mw: 2030 moderate estimate from (Mongird, p. 5), includes fuel cell, inverter and 50% of C&C and grid integration

hydrogen_fuel_cell_fixed_cost_per_mw_year: (Mongird, p. 5), excludes property tax, insurance, licensing and permitting

hydrogen_fuel_cell_life_years: (Mongird, p. 7) not very clear, since they cite 20-25 years for individual components but 30 years for system overall; key point is this life is separate from the fuel cell stacks, whose refurbishment cost is included in variable O&M.

hydrogen_fuel_cell_mwh_per_kg: 50.7% HHV efficiency from (Hunter, Table S26) and 39.4 kWh/kg HHV from Engingeering Toolbox: (0.507 MWh e- / MWh H2) (1 MWh / 1000 kWh) (39.4 kWh H2 / kg H2) = (0.01998 MWh e- / kg H2)

hydrogen_fuel_cell_variable_cost_per_mwh: (Mongird, p. 5): 0.8 $/MWh for stack refurbishment plus 0.5 $/MWh for "basic variable O&M"

hydrogen_liquefier_capital_cost_per_kg_per_hour: (Connelly, Table 2) cost for 27,000 kg/day (smallish) facility: ($104,000,000 / 27,000 kg/day) (24 h/day) = $92,444 / kg/hr (note: (Amos p. 18) shows $25600 for a 36,000 kg/day facility in 1995$, which is $43930 in 2020$.)

hydrogen_liquefier_fixed_cost_per_kg_hour_year: assumed free, since we skip the liquefication stage and store compressed hydrogen instead

hydrogen_liquefier_life_years: (Connelly, Table 1): 40 years

hydrogen_liquefier_mwh_per_kg: (Connelly, Figure 3) approx. value for 27,000 kg/day facility = 11 kWh/kg. (Connelly p. 3 cites a range of 10-20 kWh/kg and a minimum possible of 2.88 kWh/kg). (Amos p. 23) shows a range of 8-12 kWh/kg.

hydrogen_liquefier_variable_cost_per_kg: assumed free, since we skip the liquefication stage and store compressed hydrogen instead

liquid_hydrogen_tank_capital_cost_per_kg: maximum size per tank (11,000 m3) from (HDSAM 'Liquid H2 Terminal'!B48); cost per tank and per m3 from (HDSAM 'Liquid H2 Terminal'!C137); kg/m3 = g/liter from (HDSAM 'Physical Property Data'!$N$29); 2016 -> 2020 inflator (1.46/1.36) from (HDSAM 'Feedstock & Utility Prices'!L163:L167): [(5646600 2016$/tank)(1 tank / 11000 m3) + (3100 2016$/m3)] (1 m3 / 70.8 kg) (1.46 2020$/1.36 2016$) = 54.79 2020$/kg. Note, (Wade p. 20) reports a smaller tank (300,000 kg) would cost 18$/kg in 1995$, which is $30.89/kg in 2020$.

liquid_hydrogen_tank_life_years: assume 30 years, same as rest of facility

liquid_hydrogen_tank_minimum_size_kg: assume 0