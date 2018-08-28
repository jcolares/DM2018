# Datasets
**vrawu17.Rdata** - combinação do vra com o wu, mas está com alguns erros. Não utilizar por enquanto.

**vra.RData** - é o vra fornecido pelo professor Ogasawara, acrescido de duas colunas que identificam voos cancelados ou com atrasos superiores a 15 minutos e atrasos provocados exclusivamente pelo clima no aeroporto de origem.

**vra-wu-small.RData** - arquivo fornecido pelo professor.

**wd.Rdata** - arquivo com dados meteorológicos de todos os aeroportos brasileros de 1/1/14 a 31/12/17. A desrição dos campos segue abaixo. *Atenção! Os tipos de dados não foram tratados!*

## Variable Description 
[ASOS User's Guide](https://www.weather.gov/asos/) has detailed information about these data variables. **The value "M"** represents either value that was reported as missing or a value that was set to missing after meeting some general quality control check, or a value that was never reported by the sensor. The METAR format makes it difficult to determine which of the three cases may have happened.

- station:	three or four character site identifier
- valid:	timestamp of the observation
- tmpf:	Air Temperature in Fahrenheit, typically @ 2 meters
- dwpf:	Dew Point Temperature in Fahrenheit, typically @ 2 meters
- relh:	Relative Humidity in %
- drct:	Wind Direction in degrees from north
- sknt:	Wind Speed in knots
- p01i:	One hour precipitation for the period from the observation time to the time of the previous hourly precipitation reset. This varies slightly by site. Values are in inches. This value may or may not contain frozen precipitation melted by some device on the sensor or estimated by some other means. Unfortunately, we do not know of an authoritative database denoting which station has which sensor.
- alti:	Pressure altimeter in inches
- mslp:	Sea Level Pressure in millibar
- vsby:	Visibility in miles
- gust:	Wind Gust in knots
- skyc1:	Sky Level 1 Coverage
- skyc2:	Sky Level 2 Coverage
- skyc3:	Sky Level 3 Coverage
- skyc4:	Sky Level 4 Coverage
- skyl1:	Sky Level 1 Altitude in feet
- skyl2:	Sky Level 2 Altitude in feet
- skyl3:	Sky Level 3 Altitude in feet
- skyl4:	Sky Level 4 Altitude in feet
- wxcodes*:	Present Weather Codes (space seperated)
- metar*:	unprocessed reported observation in METAR format

Obs. As variáveis marcadas com * não foram baixadas (para diminuir o tamanho do arquivo).

Dados obtidos em https://mesonet.agron.iastate.edu/request/download.phtml 
