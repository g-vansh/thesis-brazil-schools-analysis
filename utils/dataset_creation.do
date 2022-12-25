// S		et Working Directory
cd U:/Research/Thesis/thesis-brazil-schools-analysis

// Import Data
import delimited using data/BrazilEduPanel_School.csv, varnames(1) delim(",") bindquote(strict)
compress
save data/schools.dta, replace

// Load Data
clear
use data/schools.dta

// Format Columns
drop uf co_uf no_entidade
rename co_municipio code_municipality
rename co_entidade code_school
rename nu_ano_censo year
rename sigla name_state
rename munic name_municipality

// Drop if school has less than 10 years or is inactive CHANGE Criteria if doin 2005-211
drop if status == "Inactive"
bysort code_school(year): keep if _N >= 10

// Clean Columns
drop tp_situacao_funcionamento tp_dependencia tp_localizacao
gen rural = 0
replace rural = 1 if urbana == 0
rename urbana urban
rename federal federal_school
rename estadual state_school
rename municipal municipal_school
rename privada private_school

// Drop tp_categoria_escola_privada (Private School Categories) for now
drop tp_categoria_escola_privada status

// By School_code, fill constant NA values
foreach v of varlist code_municipality name_municipality name_state federal_school state_school municipal_school private_school urban rural {
	di `v'
	sort code_school year, stable
	by code_school: gen long obsno = _n
	by code_school: gen countnonmissing = sum(!missing(`v')) if !missing(`v')
	bysort code_school (countnonmissing): gen firstnonmissing = `v'[1]
	bysort code_school(year): replace `v' = firstnonmissing if missing(`v')
	drop obsno countnonmissing firstnonmissing
}

// Filter data down to 2005, 2007, 2009, 2011

// Create resouerce netrics


// Convert the panel into cross-sectional data (measure different metrics)
