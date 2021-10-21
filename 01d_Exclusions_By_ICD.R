Exclusions <- Exclusions %>%
  mutate(Type = case_when(
        "I2510" == Code |
        "I252" == Code |
        "I509" == Code |
        "I110" == Code |
        "I4581" == Code |
        "I429" == Code |
        "I517" == Code |
        "I340" == Code |
        "I456" == Code |
        "I5022" == Code |
        "I071" == Code |
        "I130" == Code |
        "I119" == Code |
        "I422" == Code |
        "I5032" == Code |
        "I420" == Code |
        "I081" == Code |
        "I255" == Code |
        "I371" == Code |
        "I441" == Code |
        "I361" == Code |
        "I5020" == Code |
        "I5042" == Code |
        "I428" == Code |
        "I5030" == Code |
        "I351" == Code |
        "I350" == Code |
        "I482" == Code |
        "I519" == Code |
        "I700" == Code |
        "I4589" == Code |
        "I5189" == Code |
        "I083" == Code |
        "I459" == Code |
        "Z45018" == Code |
        "I25119" == Code |
        "I455" == Code |
        "I481" == Code |
        "I050" == Code |
        "I25810" == Code |
        "I080" == Code |
        "I5040" == Code |
        "I5082" == Code |
        "I059" == Code |
        "I082" == Code |
        "I259" == Code |
        "I2722" == Code |
        "I358" == Code |
        "I426" == Code |
        "I4430" == Code |
        "I501" == Code |
        "I50810" == Code |
        "I088" == Code |
        "I708" == Code |
        "I378" == Code |
        "I4439" == Code |
        "I484" == Code |
        "I5084" == Code |
        "I058" == Code |
        "I060" == Code |
        "I062" == Code |
        "I099" == Code |
        "I25811" == Code |
        "I2723" == Code |
        "I2781" == Code |
        "I2783" == Code |
        "I279" == Code |
        "I311" == Code |
        "I318" == Code |
        "I348" == Code |
        "I352" == Code |
        "I359" == Code |
        "I370" == Code |
        "I424" == Code |
        "I483" == Code |
        "Z950" == Code |
        "Z45010" == Code ~ "CSRS_Hx_Heart_Disease_Mapping",

        
        "K8000"== Code |
        "K8001"== Code |
        "K8010"== Code |
        "K8011"== Code |
        "K8012"== Code |
        "K8013"== Code |
        "K8018"== Code |
        "K8019"== Code |
        "K8021"== Code |
        "K8040"== Code |
        "K8041"== Code |
        "K8042"== Code |
        "K8043"== Code |
        "K8044"== Code |
        "K8045"== Code |
        "K8046"== Code |
        "K8047"== Code |
        "K8050"== Code |
        "K8051"== Code |
        "K8060"== Code |
        "K8061"== Code |
        "K8062"== Code |
        "K8063"== Code |
        "K8064"== Code |
        "K8065"== Code |
        "K8066"== Code |
        "K8067"== Code |
        "K8070"== Code |
        "K8071"== Code |
        "K352"== Code |
        "K3520"== Code |
        "K3521"== Code |
        "K353"== Code |
        "K3530"== Code |
        "K3531"== Code |
        "K3532"== Code |
        "K3533"== Code |
        "K3580"== Code |
        "K3589"== Code |
        "K35890"== Code |
        "K35891"== Code |
        "K36"== Code |
        "K37"== Code |
        "K388"== Code |
        "K389"== Code |
        "K5712"== Code |
        "K5700"== Code |
        "K5701"== Code |
        "K5711"== Code |
        "K5713"== Code |
        "K5720"== Code |
        "K5721"== Code |
        "K5731"== Code |
        "K5732"== Code |
        "K5733"== Code |
        "K5740"== Code |
        "K5741"== Code |
        "K5751"== Code |
        "K5752"== Code |
        "K5753"== Code |
        "K5780"== Code |
        "K5781"== Code |
        "K5791"== Code |
        "K5792"== Code |
        "K5793"== Code |
        "A047"== Code |
        "A0471"== Code |
        "A0472"== Code |
        "A062"== Code |
        "A09"== Code |
        "K50011"== Code |
        "K50012"== Code |
        "K50013"== Code |
        "K50014"== Code |
        "K50018"== Code |
        "K50019"== Code |
        "K50111"== Code |
        "K50112"== Code |
        "K50113"== Code |
        "K50114"== Code |
        "K50118"== Code |
        "K50119"== Code |
        "K50811"== Code |
        "K50812"== Code |
        "K50813"== Code |
        "K50814"== Code |
        "K50818"== Code |
        "K50819"== Code |
        "K50911"== Code |
        "K50912"== Code |
        "K50913"== Code |
        "K50914"== Code |
        "K50918"== Code |
        "K50919"== Code |
        "K51011"== Code |
        "K51012"== Code |
        "K51013"== Code |
        "K51014"== Code |
        "K51018"== Code |
        "K51019"== Code |
        "K51211"== Code |
        "K51212"== Code |
        "K51213"== Code |
        "K51214"== Code |
        "K51218"== Code |
        "K51219"== Code |
        "K51311"== Code |
        "K51312"== Code |
        "K51313"== Code |
        "K51314"== Code |
        "K51318"== Code |
        "K51319"== Code |
        "K51411"== Code |
        "K51412"== Code |
        "K51413"== Code |
        "K51414"== Code |
        "K51418"== Code |
        "K51419"== Code |
        "K5150"== Code |
        "K51511"== Code |
        "K51512"== Code |
        "K51513"== Code |
        "K51514"== Code |
        "K51518"== Code |
        "K51519"== Code |
        "K51811"== Code |
        "K51812"== Code |
        "K51813"== Code |
        "K51814"== Code |
        "K51818"== Code |
        "K51819"== Code |
        "K51911"== Code |
        "K51912"== Code |
        "K51913"== Code |
        "K51914"== Code |
        "K51918"== Code |
        "K51919"== Code |
        "K520"== Code |
        "K521"== Code |
        "K522"== Code |
        "K5221"== Code |
        "K5229"== Code |
        "B252"== Code |
        "B263"== Code |
        "K850"== Code |
        "K8500"== Code |
        "K8501"== Code |
        "K8502"== Code |
        "K851"== Code |
        "K8510"== Code |
        "K8511"== Code |
        "K8512"== Code |
        "K852"== Code |
        "K8520"== Code |
        "K8521"== Code |
        "K8522"== Code |
        "K853"== Code |
        "K8530"== Code |
        "K8531"== Code |
        "K8532"== Code |
        "K858"== Code |
        "K8580"== Code |
        "K8581"== Code |
        "K8582"== Code |
        "K859"== Code |
        "K8590"== Code |
        "K8591"== Code |
        "K8592"== Code |
        "K860"== Code |
        "K861"== Code |
        "C700"== Code |
        "C701"== Code |
        "C709"== Code |
        "C710"== Code |
        "C711"== Code |
        "C712"== Code |
        "C713"== Code |
        "C714"== Code |
        "C715"== Code |
        "C716"== Code |
        "C717"== Code |
        "C718"== Code |
        "C719"== Code |
        "C7931"== Code |
        "O000"== Code |
        "O0000"== Code |
        "O0001"== Code |
        "O001"== Code |
        "O0010"== Code |
        "O00101"== Code |
        "O00102"== Code |
        "O00109"== Code |
        "O0011"== Code |
        "O00111"== Code |
        "O00112"== Code |
        "O00119"== Code |
        "O002"== Code |
        "O0020"== Code |
        "O00201"== Code |
        "O00202"== Code |
        "O00209"== Code |
        "O0021"== Code |
        "O00211"== Code |
        "O00212"== Code |
        "O00219"== Code |
        "O008"== Code |
        "O0080"== Code |
        "O0081"== Code |
        "O009"== Code |
        "O0090"== Code |
        "O0091"== Code |
        "J930"== Code |
        "J9311"== Code |
        "J9382"== Code |
        "J9383"== Code |
        "J939"== Code |
        "J95811"== Code |
        "J9312"== Code |
        "R6520"== Code |
        "R6521"== Code |
        "T8112XA"== Code |
        "A419"== Code |
        "K315"== Code |
        "K560"== Code |
        "K561"== Code |
        "K562"== Code |
        "K563"== Code |
        "K565"== Code |
        "K5650"== Code |
        "K5651"== Code |
        "K5652"== Code |
        "K5660"== Code |
        "K56600"== Code |
        "K56601"== Code |
        "K56609"== Code |
        "K5669"== Code |
        "K56690"== Code |
        "K56691"== Code |
        "K56699"== Code |
        "I498" == Code |
        "I4891" == Code |
        "I499" == Code |
        "I471" == Code |
        "I480" == Code |
        "I472" == Code |
        "I495" == Code |
        "I2699" == Code |
        "I4892" == Code |
        "I69354" == Code |
        "I313" == Code |
        "I82409" == Code |
        "I442" == Code |
        "I69351" == Code |
        "I319" == Code |
        "I421" == Code |
        "I5023" == Code |
        "I214" == Code |
        "I209" == Code |
        "I6521" == Code |
        "I6522" == Code |
        "I469" == Code |
        "I4901" == Code |
        "I6529" == Code |
        "I69398" == Code |
        "I25110" == Code |
        "I38" == Code |
        "I639" == Code |
        "I248" == Code |
        "I5043" == Code |
        "I675" == Code |
        "I69954" == Code |
        "I161" == Code |
        "I219" == Code |
        "I21A1" == Code |
        "I776" == Code |
        "I200" == Code |
        "I249" == Code |
        "I253" == Code |
        "I5021" == Code |
        "I69334" == Code |
        "I169" == Code |
        "I208" == Code |
        "I25118" == Code |
        "I69392" == Code |
        "I69951" == Code |
        "I82402" == Code |
        "I8290" == Code |
        "I213" == Code |
        "I309" == Code |
        "I5041" == Code |
        "I513" == Code |
        "I674" == Code |
        "I69344" == Code |
        "I69352" == Code |
        "I69359" == Code |
        "I7100" == Code |
        "I729" == Code |
        "I7771" == Code |
        "I2109" == Code |
        "I2129" == Code |
        "I236" == Code |
        "I25111" == Code |
        "I300" == Code |
        "I5033" == Code |
        "I609" == Code |
        "I618" == Code |
        "I669" == Code |
        "I69311" == Code |
        "I69322" == Code |
        "I69331" == Code |
        "I69851" == Code |
        "I774" == Code |
        "I7774" == Code |
        "I81" == Code |
        "I820" == Code |
        "I82432" == Code |
        "I824Z1" == Code |
        "I824Z9" == Code |
        "I82621" == Code |
        "B3324" == Code |
        "I019" == Code |
        "I201" == Code |
        "I2119" == Code |
        "I241" == Code |
        "I2542" == Code |
        "I25700" == Code |
        "I2692" == Code |
        "I301" == Code |
        "I400" == Code |
        "I4902" == Code |
        "I5031" == Code |
        "I50813" == Code |
        "I514" == Code |
        "I5181" == Code |
        "I608" == Code |
        "I614" == Code |
        "I615" == Code |
        "I6200" == Code |
        "I629" == Code |
        "I638" == Code |
        "I6501" == Code |
        "I6502" == Code |
        "I651" == Code |
        "I6601" == Code |
        "I676" == Code |
        "I677" == Code |
        "I6782" == Code |
        "I69154" == Code |
        "I69391" == Code |
        "I69820" == Code |
        "I69822" == Code |
        "I69823" == Code |
        "I69859" == Code |
        "I69920" == Code |
        "I69922" == Code |
        "I69934" == Code |
        "I69998" == Code |
        "I7101" == Code |
        "I716" == Code |
        "I7409" == Code |
        "I742" == Code |
        "I745" == Code |
        "I749" == Code |
        "I82210" == Code |
        "I82290" == Code |
        "I823" == Code |
        "I82401" == Code |
        "I82403" == Code |
        "I82411" == Code |
        "I82412" == Code |
        "I82422" == Code |
        "I82491" == Code |
        "I824Z2" == Code |
        "I82612" == Code |
        "I82622" == Code |
        "I82623" == Code |
        "I82B11" == Code |
        "I953" == Code |
        "I9789" == Code |
        "J9601" == Code |
        "J9602" == Code |
        "J9621" == Code |
        "D62" == Code |
          "N179" == Code |
          "E860" == Code |
          "E876" == Code |
          "I959" == Code |
          "E1165" == Code
          
          ~ "CSRS_SAEs_Mapping",
    T ~ Type
  ))