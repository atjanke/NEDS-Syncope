# This is a link of the CCSR codes that were identified as exclusions

Exclusions <- 
  rbind(
  data.frame(Type='CSRS_Exclusions_Mapping', CCSR_Code=c(
  'NVS009',
  'INJ073',
  'INJ008',
  'INJ045',
  'SYM010',
  'MBD017',
  'MBD018',
  'MBD019',
  'MBD020',
  'MBD021',
  'MBD022',
  'MBD023',
  'MBD024',
  'MBD025')),
  data.frame(Type="CSRS_SAEs_Mapping", CCSR_Code=c(
  'CIR017',
  'Z950',
  'CIR009',
  'CIR013',
  'CIR014',
  'CIR021',
  'CIR027')),
  data.frame(Type="CSRS_Hx_Heart_Disease_Mapping",CCSR_Code=c(
  'CIR005',
  'CIR011',
  'CIR019')))