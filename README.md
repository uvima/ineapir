
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ineapir <img src="man/figures/hex_logo.png" align="right" width = "120"/>

<!-- badges: start -->
<!-- badges: end -->

## Overview

inepair provides a set of functions to obtain open data and metadata
published by the National Statistics Institute of Spain
([INE](https://www.ine.es/en/index.htm)). The data is obtained thanks to
calls to the INE API service which allows access via URL requests to all
the available statistical information published by INE.

## Installation

You can install the development version of **ineapir** like so:

``` r
#install.packages("remotes")
remotes::install_github("uvima/ineapir")
```

## Data request examples

The data is only associated with the series object and these can be
grouped together into statistical tables. The field named ‘*Valor*’ is
the only one that contains data. The rest of the fields are necessary
for the data to be well defined.

### Obtaining data from a series

To get the last data of a series it is necessary to pass the `codSeries`
argument which is the identification code of the series.

``` r
library(ineapir)
series <- get_data_series(codSeries = "IPC251856")
series$Data
#>          Fecha FK_TipoDato FK_Periodo Anyo Valor Secreto
#> 1 1.682892e+12           3          5 2023   3.2   FALSE
```

To get the last n data from a series it is necessary to pass the `nlast`
argument as well.

``` r
series <- get_data_series(codSeries = "IPC251856", nlast = 5)
series$Data
#>          Fecha FK_TipoDato FK_Periodo Anyo Valor Secreto
#> 1 1.672528e+12           1          1 2023   5.9   FALSE
#> 2 1.675206e+12           1          2 2023   6.0   FALSE
#> 3 1.677625e+12           1          3 2023   3.3   FALSE
#> 4 1.680300e+12           1          4 2023   4.1   FALSE
#> 5 1.682892e+12           3          5 2023   3.2   FALSE
```

Additionally, it is possible to obtain data from a series between two
dates. The date must have and specific format (*yyyy/mm/dd*). If the end
date is not specified we obtain all the data from the start date.

``` r
series <- get_data_series(codSeries = "IPC251856", dateStart = "2023/01/01", 
                          dateEnd = "2023/04/01")
series$Data
#>          Fecha FK_TipoDato FK_Periodo Anyo Valor Secreto
#> 1 1.672528e+12           1          1 2023   5.9   FALSE
#> 2 1.675206e+12           1          2 2023   6.0   FALSE
#> 3 1.677625e+12           1          3 2023   3.3   FALSE
#> 4 1.680300e+12           1          4 2023   4.1   FALSE
```

### Obtaining data from a table

To get all the data of a table it is necessary to pass the `idTable`
argument which is the identification code of the table.

``` r
table <- get_data_table(idTable = 50902)
table[1,c("COD", "Nombre")]
#>         COD                                   Nombre
#> 1 IPC251852 Total Nacional. Índice general. Índice.
head(table$Data[[1]])
#>          Fecha FK_TipoDato FK_Periodo Anyo   Valor Secreto
#> 1 1.680300e+12           1          4 2023 111.773   FALSE
#> 2 1.677625e+12           1          3 2023 111.111   FALSE
#> 3 1.675206e+12           1          2 2023 110.703   FALSE
#> 4 1.672528e+12           1          1 2023 109.668   FALSE
#> 5 1.669849e+12           1         12 2022 109.899   FALSE
#> 6 1.667257e+12           1         11 2022 109.734   FALSE
```

To get the last n data from a table it is necessary to pass the `nlast`
argument as well.

## Metadata request examples

Structural metadata are objects that describe both time series and
statistical tables and allow their definition. All these database
objects have an associated identifier that is essential for the correct
use of the service.

### Obtaining statistical operations

The database contains information about all short-term statistical
operations, those with a periodicity for disseminating results of less
than a year, as well as some structural statistical operations.

``` r
operations <- get_metadata_operations()
head(operations)
#>   Id Cod_IOE                                                 Nombre Codigo  Url
#> 1  4   30147           Estadística de Efectos de Comercio Impagados     EI <NA>
#> 2  6   30211                     Índice de Coste Laboral Armonizado   ICLA <NA>
#> 3  7   30168 Estadística de Transmisión de Derechos de la Propiedad   ETDP <NA>
#> 4 10   30256                                    Indicadores Urbanos     UA <NA>
#> 5 13   30219                Estadística del Procedimiento Concursal    EPC <NA>
#> 6 14   30182                Índices de Precios del Sector Servicios    IPS <NA>
```

An operation can be identify by a numerical code (‘*Id*’), an alphabetic
code (‘*Codigo*’) or by the code of the statistical operation in the
Inventory of Statistical Operations (IOE + ‘*Cod_IOE*’). To obtain
information about only one operation we have to pass the `operation`
argument with one of these codes.

``` r
operation <- get_metadata_operations(operation = "IPC")
as.data.frame(operation)
#>   Id Cod_IOE                             Nombre Codigo
#> 1 25   30138 Índice de Precios de Consumo (IPC)    IPC
```

### Obtaining variables

We can get all the variables of the system.

``` r
variables <- get_metadata_variables(lang = "EN")
head(variables)
#>    Id                            Nombre Codigo
#> 1 349                   Regional totals    NAC
#> 2 954                             Total       
#> 3  70 Autonomous Communities and Cities   CCAA
#> 4 955      Crops, pastures and orchards       
#> 5 115                         Provinces   PROV
#> 6 956               UAA and Other Lands
```

A variable can be identify by a numerical code (‘*Id*’). In addition, if
we pass the `operation` argument we obtain the variables used in an
operation.

``` r
variables <- get_metadata_variables(operation = "IPC", lang = "EN")
head(variables)
#>    Id                            Nombre Codigo
#> 1   3                      Type of data       
#> 2  70 Autonomous Communities and Cities   CCAA
#> 3 115                         Provinces   PROV
#> 4 269               Special groups 2001       
#> 5 270                     Headings 2001       
#> 6 349                   Regional totals    NAC
```

### Obtaining values

To get all the values that a variable can take it is necessary to pass
the `variable` argument which is the identifier of the variable.

``` r
values <- get_metadata_values(variable = 3, lang = "EN")
head(values)
#>   Id Fk_Variable                                      Nombre Codigo
#> 1 70           3                                  Gross data       
#> 2 71           3 Seasonal and calendar effects adjusted data       
#> 3 72           3                                   Base data       
#> 4 73           3                    Quarterly variation rate       
#> 5 74           3                            Annual variation       
#> 6 75           3                                       Euros
```

A value can be identify by a numerical code (‘*Id*’). In addition, if we
pass the `operation` argument as well we obtain the values that the
variable takes in that particular operation.

``` r
values <- get_metadata_values(operation = "IPC", variable = 3, lang = "EN")
head(values)
#>   Id Fk_Variable                 Nombre Codigo
#> 1 72           3              Base data       
#> 2 74           3       Annual variation       
#> 3 83           3                  Index       
#> 4 84           3 Monthly variation rate       
#> 5 85           3   Annual average index      M
#> 6 86           3       Annual variation
```

### Obtaining tables

We can get the tables associated with an statistical operation.

``` r
tables <- get_metadata_tables(operation = "IPC")
head(tables[,c("Id","Nombre")])
#>      Id                                                                 Nombre
#> 1 24077                    Índice general nacional. Series desde enero de 1961
#> 2 25331                             Ponderaciones: general y de grupos ECOICOP
#> 3 35083        Índices nacionales: Componentes para el análisis de la COVID-19
#> 4 49130                        Índices nacionales: general y de grupos ECOICOP
#> 5 50902                        Índices nacionales: general y de grupos ECOICOP
#> 6 50908 Índices nacionales a impuestos constantes: general y de grupos ECOICOP
```

A table is defined by different groups or selection combo boxes and each
of them by the values that one or several variables take. To obtain the
variables and values present in a table first we have to query the
groups that define the table:

``` r
groups <- get_metadata_table_groups(idTable = 50902)
head(groups)
#>       Id         Nombre
#> 1 110889 Grupos ECOICOP
#> 2 110890   Tipo de dato
```

Once we have the identification codes of the groups, we can query the
values for an specific group.

``` r
values <- get_metadata_table_Values(idTable = 50902, idGroup = 110889)
head(values, 4)
#>       Id Fk_Variable                             Nombre Codigo
#> 1 304092         762                     Índice general     00
#> 2 304093         762 Alimentos y bebidas no alcohólicas     01
#> 3 304094         762       Bebidas alcohólicas y tabaco     02
#> 4 304095         762                  Vestido y calzado     03
```

### Obtaining series

The data is only associated with the series object. To obtain
information about a particular series it is necessary to pass the
`codSeries` argument which is the identification code of the series.

``` r
series <- get_metadata_series(codSeries = "IPC251856")
as.data.frame(series)
#>       Id       COD FK_Operacion
#> 1 251856 IPC251856           25
#>                                              Nombre Decimales FK_Periodicidad
#> 1 Total Nacional. Índice general. Variación anual.          1               1
#>   FK_Publicacion FK_Clasificacion FK_Escala FK_Unidad
#> 1              8               90         1       135
```

To get the values and variables that define a series it is necessary to
pass the `codSeries` argument as well.

``` r
values <- get_metadata_series_values(codSeries = "IPC251856")
head(values)
#>       Id Fk_Variable          Nombre Codigo
#> 1  16473         349  Total Nacional     00
#> 2 304092         762  Índice general     00
#> 3     74           3 Variación anual
```

To get all the series that define a table it is necessary to pass the
`idTable` argument which is the identification code of the table.

``` r
series <- get_metadata_series_table(idTable = 50902)
head(series[,c("COD", "Nombre")], 4)
#>         COD                                                          Nombre
#> 1 IPC251852                        Total Nacional. Índice general. Índice. 
#> 2 IPC251855             Total Nacional. Índice general. Variación mensual. 
#> 3 IPC251856               Total Nacional. Índice general. Variación anual. 
#> 4 IPC251858 Total Nacional. Índice general. Variación en lo que va de año.
```
