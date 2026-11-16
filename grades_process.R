library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

grade <- read.csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FIU/historical_grades.csv")

# remove 0 credit and courses with more than 5
grade <- grade %>% filter(units < 5 & units > 0)

grade <- grade %>% filter(!(course_prefix %in% 
  c('ABT','ACG',"ADE","ADV", "AEB", "AFR", "AFS", "AGG", "AGR", "AML", "ANG", "APK", "ARA", "ARC", "ARE", "ART", "ASH", "ASL", "ASN", 
    "AST", "ATR", "BCH", "BCN", "BME", "BMS", "BOT", "BUL","CBH", "CCE", "CEG", "CHI", "CHS", "CHT", "CJC", "CJE", "CJL", "CLP", "CLS","CMN",
    "CPO", "CRW", "CWR", "CYP", "DAA", "DAN", "DIE", "DIG", "DSC", "EAB", "EAS", "EBD", "ECH", "ECP", "ECS", "ECT", "ECW", "EDA",'EDE', 'EDF',
    'EDG', 'EDH', 'EDP', 'EEC' ,'EEX', "EGL", "EGM", "EGS", "EIN", 'EMA', 'EME', 'EML', "ENL", "ENU", "ENV", "ESE", "ESI", "EVT", "EXP",
    "FAD", "FAS", "FES", "FFP", 'FIL','FIN', 'FLE', 'FOL', "FOS", "FOT", "FRE", "FRW", "FSS", "GEB", 'GER', 'GIS', 'GMS', "GRA" , "GRE", "HAI",
    "HBR", "HEE", 'HFT', 'HIM', 'HIS', 'HLP', 'HME', 'HMG', 'HSA', 'HSC', "HUN", "IDH", "IHS", "IND", "INP", "ISC", "ISM", "ISS", "ITA", "JOU",
    "JPN", "JRM", 'JST','KOR','LAA', 'LAE', 'LAS', 'LAW', 'LBS', 'LEI', 'LIN', 'MAE', 'MAN', 'MAR', 'MAT', 'MCB', 'MDC', "MDE", "MDI", "MDR",
    "MDS", "MET", 'MGF', 'MHS', 'MMC', 'MSL', 'MTG', 'MUC', 'MUE', 'MUG', 'MUM', 'MUN', 'MUO', 'MUR', 'MUS', 'MUT', 'MUY', 'MVB', 'MVJ', 'MVK',
    'MVP', 'MVS', 'MVV', 'MVW', 'NGR', 'NSE', 'NUR', 'OCB', 'ORI', 'OSE', 'OTH', 'PAS', 'PCB', 'PCO', 'PEM', 'PEO', 'PEP', 'PET', 'PGY', 'PHC',
    'PHH', 'PHM', 'PHP', 'PHT', 'PHZ', 'POR', 'POT', 'POW', 'PPE', 'PSB', 'PRT', 'PSN', 'PUP', 'PUR', 'QMB', 'RCS', 'RDG', 'RED', 'REE', 'RLG', 
    'RMI', "RTV", 'RUS', 'SAL', 'SCE', 'SCM', 'SDS', 'SMT', 'SOP', 'SOW', 'SPA', 'SPM', "SPN", 'SPS', 'SPT', 'SPW','SRK', 'SSE', 'SUR', 'SWS', 
    'SYA', 'SYD', 'SYP', 'TAX', 'TPA', 'TSL', 'TTE', 'URP', 'URS', 'VIC', 'WST', 'ZOO' )))

# map UCC Humanities Group 1
hum_ucc1_map <- data.frame(
  course_prefix = c("ARH", "HUM", "LIT", "MUL", "PHI", "THE"),
  course_num    = c("2000", "1020", "1000", "1010", "2010", "2000"),
  new_prefix    = "HUM",
  new_num       = "UCC1"
)

grade <- grade %>%
  left_join(hum_ucc1_map, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# map UCC Humanities Group 2
hum_ucc2_map <- data.frame(
  course_prefix = c("AFH", "ARC", "ARH", "ARH", "ENG", "EUH", "EUH", "EUH", "HUM", "IDS", "LAH", "MUH", "MUH", "PHI", "POT", "REL", "REL", "SPC", "SPC", "TPP", "WOH"),
  course_num    = c("2000", "2701", "2050", "2051", "2012", "2011", "2021", "2030", '2236', '2301', '2020', '2018', '2512', '2600', '2010', '2011', '2149', '2237', '2270', '2100', '2001'),
  new_prefix    = "HUM",
  new_num       = "UCC2"
)

grade <- grade %>%
  left_join(hum_ucc2_map, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# map UCC sciences grp 1 (only applicable to CS degree)
natsci_ucc1 <- data.frame(
  course_prefix = c("BSC", "CHM", "ESC", "PHY"),
  course_num    = c("2010", "1045", "1000", "2048"),
  new_prefix    = "NAT",
  new_num       = "UCC1"
)

grade <- grade %>%
  left_join(natsci_ucc1, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

natsci_ucc1lab <- data.frame(
  course_prefix = c("BSC", "CHM", "ESC", "PHY"),
  course_num    = c("2010L", "1045L", "1000L", "2048L"),
  new_prefix    = "NAT",
  new_num       = "UCC1L"
)

grade <- grade %>%
  left_join(natsci_ucc1lab, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)


natsci_ucc2 <- data.frame(
  course_prefix = c("BSC", "CHM", "GEO", "PHY", "GLY", 'GLY'),
  course_num    = c("2011", "1046", "3510", "2049", '1010', '3039'),
  new_prefix    = "NAT",
  new_num       = "UCC2"
)
# sciences group 2
grade <- grade %>%
  left_join(natsci_ucc2, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# social science grp 1 (civic lit)
socsci_ucc1 <- data.frame(
  course_prefix = c('AMH', 'AMH', 'POS'),
  course_num = c('2010', '2020', '2041'),
  new_prefix    = "SOC",
  new_num       = "UCC1"
)


grade <- grade %>%
  left_join(socsci_ucc1, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# social science grp 2 
socsci_ucc2 <- data.frame(
  course_prefix = c(
    "CCJ","COM","COM","CPO","CPO","CPO",
    "DEP","ECO","EVR","GEA","GEO","IDS",
    "INR","PAD","REL","SYG","SYO"
  ),
  course_num = c(
    "2020","1000","2417","2002","2102","2304",
    "2000","2023","1017","2000","2000","2183",
    "2001","2000","2300","2000","2101"
  ),
  new_prefix = "SOC",   
  new_num    = "UCC2" 
)

grade <- grade %>%
  left_join(socsci_ucc2, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# cs electives - foundation
cs_fnd <- data.frame(
  course_prefix = c('CAP', 'COP', 'COT', 'COT', 'COT', 'COT', 'MAD', 'MAD', 'MAD', 'MAD', 'MHF'),
  course_num = c('4506', '4534', '3510', '3541', '4521', '4601', '3301', '3401', '3512', '4203', '4302'),
  new_prefix = "COT",
  new_num    = "FND1" 
  )

grade <- grade %>%
  left_join(cs_fnd, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# cs electives - systems
cs_sys <- data.frame(
  course_prefix = c('CAP', 'CDA', 'CEN', 'CIS', 'COP', 'COP', 'COP', 'CTS', 'COT'),
  course_num = c('4453', '4625', '4083', '4731', '4520', '4604', '4751', '4408', '4431'),
  new_prefix = "COT",
  new_num    = "SYS1" 
)

grade <- grade %>%
  left_join(cs_sys, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# cs electives - applications
cs_app <- data.frame(
  course_prefix = c('CAI', 'CAI', 'CAI', 'CAP', 'CAP', 'CAP', 'CAP', 'CAP', 'CEN', 'CEN', 'COP', 'COP'),
  course_num = c('4002', '4105', '4304', '4052', '4104', '4710', '4770', '4830', '4021', '4072', '4226', '4655'),
  new_prefix = "COT",
  new_num    = "APP1" 
)

grade <- grade %>%
  left_join(cs_app, by = c("course_prefix", "course_num")) %>%
  mutate(
    course_prefix = if_else(!is.na(new_prefix), new_prefix, course_prefix),
    course_num    = if_else(!is.na(new_num),    new_num,    course_num)
  ) %>%
  select(-new_prefix, -new_num)

# remove extra prefixes unique(grade$course_prefix)
grade <- grade %>% filter(!(course_prefix %in% 
  c('LIT', 'ANT', 'MAA', 'EEE', 'EEL','PHY', 'PSY', 'CCJ', 'ECO', 'IDS', 'CHM','COM', 'REL', 'MAS', 'DEP', 'POS', 'INR', 'EVR', 'IDC', 'SYG', 'BSC', 'PAD', 'PHI', 'ARH', "TRA", 'MUH', 'CGN', 'CES', 'GEO', 'MAP', 'GLY', 'EUH', 'AMH', 'AFS', 'GEA', 'IPM', 'HOS', 'JPT', 'OCE', 'MUL', 'OCP', 'TPP', 'SYO', 'ENT', 'THE', 'ENG', 'WOH', 'LAH', 'CJJ', 'AFH', 'ENY', 'NSP', 'ISE')
  ))


# tech writing course sub
grade <- grade %>%
  mutate( course_num = if_else(course_prefix == "ENC" & course_num == "3213", "3249",course_num))


grade <- grade %>%
  mutate(
    course_prefix = if_else(course_prefix == "MAD" & course_num == "2104",
                            "COT",
                            course_prefix),
    course_num = if_else(course_prefix == "COT" & course_num == "2104",
                         "3100",
                         course_num)
  )


write.csv(grade, "C:/Users/cpanc/OneDrive/Documents/IDC6940/FIU/slim_grades2.csv", row.names = FALSE)
