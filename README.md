# Presidents Name Data
_Data on presidents from west Africa_

Countries covered:

- Benin
- Burkina Faso
- Cote d'Ivoire
- The Gambia
- Ghana
- Guinea
- Guinea-Bissau
- Liberia
- Mali
- Mauritania
- Niger
- Nigeria
- Senegal
- Sierra Leone
- Togo

The data come from [world states men](http://www.worldstatesmen.org/) website. 
Each script shows how the data has been processed for each country. 
Here are the main variables in the dataframe obtained at the end of the datawrangling step:

- `country`: the country name
- `start_day` / `end_day`: resp. the start and end day of each president
- `start_month` / `end_month`: resp. The start and end month of each president
- `start_year` / `end_year`: resp. The start and end year of each president
- `president_name`: The name of the president
- `military` / `provisional`: **0** (No) / **1** (Yes) / **2** 
(First military or prov. and then head of state) indicating if the government 
has been a military one or a provisional or a mix between military 
(resp. provisional) and normal rule.

Thanks for your time.
