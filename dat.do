* Structural Break
clear all

use "/Users/hectorbahamonde/research/Tobalaba/dat.dta"

format Date %d
tsset Date

regress Departures_i Date, vce(robust)

* Structural Break

estat sbsingle

estat sbknown, break(td(26mar2020))

* https://www.stata.com/stata14/structural-breaks/
