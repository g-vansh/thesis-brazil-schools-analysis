// Set Working Directory
cd U:/Research/Thesis/thesis-brazil-schools-analysis

// Import Data
import delimited using data/BrazilEduPanel_School.csv, varnames(1) delim(",") bindquote(strict)
compress
save data/schools.dta, replace

// Load Data
clear
use data/schools.dta

// Format Columns
drop UF CO_UF NO_ENTIDADE
rename CO_MUNICIPIO CODE_municipality
rename CO_ENTIDADE CODE_school
rename NU_ANO_CENSO panel_year
rename SIGLA NAME_state
rename MUNIC NAME_municipality
gen year = year(panel_year)
drop panel_year


// See if any years with status 0 exist where before those years they have a 1
// Filter Data To Include Relevant Info
bysort CO_SCHOOL(year): keep if STATUS == 1

// Convert the panel into cross-sectional data (measure different metrics)
