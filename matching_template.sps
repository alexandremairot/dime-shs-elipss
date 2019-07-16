************************************************************************.
* name: matching_template.sps.
* author: Alexandre Mairot (alexandre.mairot@sciencespo.fr).
* title: template matching file syntax for data file ELIPSS.
* program: SPSS.
* version: 1.0.
************************************************************************.

DATASET CLOSE ALL.
OUTPUT CLOSE ALL.

* Address of the work folder.

FILE HANDLE work_folder /NAME="[adress of the work folder]".

****************************************************************.
* Part 1 : function to rename the variables.
***************************************************************.

DEFINE !PREVAR (vlist = !CHAREND('/')  
/prefix = !CMDEND )
!DO !vname !IN (!vlist) 
   !LET !nname = !CONCAT(!prefix, '_', !vname)
   RENAME VARIABLES (!vname = !nname).
!DOEND
!ENDDEFINE.

*********************************************************.
* Part 2 : Preparation of annual survey data for matching.
*********************************************************.

GET FILE= "[address of the survey file]"
/KEEP uid.
DATASET NAME uid_survey.
SORT CASES BY uid(A).
GET FILE="[address of the annual survey file]"
/KEEP uid [list of the variables].
DATASET NAME annual_survey.
SORT CASES BY uid(A).
GET FILE="[address of the profession recode file]"
/KEEP uid pcs6cjt.
DATASET NAME sicore_recode.
SORT CASES BY uid(A).
GET FILE="[address of the annual variables imputed file]"
/KEEP uid vague a1 a2a_rec a3c_rec pcs18 pcs6 b18_rec b18c typmen5 c8 e2a_rec e2auc e5 f1_rec g5 h1a h4 i1 i8 j1 k3.
DATASET NAME annual_imputation.
SORT CASES BY uid(A).
DATASET ACTIVATE uid_survey.
MATCH FILES /FILE=*
    /TABLE='annual_survey'
    /TABLE='sicore_recode'
    /TABLE='annual_imputation'
    /BY uid
.
EXECUTE.

* Add on the "no surveyed" item in the variables matching with the survey dataset.

RECODE [list of the variables included in the annual survey file 'annual_survey']
    (SYSMIS=[code of the "no survey" value]).
MISSING VALUES [list of the variables included in the annual survey file 'annual_survey' without missing values code]
    ([code of the "no survey" value] THRU HIGHEST).
ADD VALUE LABELS [list of the variables included in the annual survey file 'annual_survey']
    [code of the "no survey" value] "No surveyed".

RECODE pcs6cjt
    (SYSMIS=9996).
ADD VALUE LABELS pcs6cjt
    9996 "No surveyed".	
RECODE vague a1 a3c_rec b18c typmen5 c8 e5 f1_rec g5 h1a h4 i1 j1 k3
    (SYSMIS = 96).
ADD VALUE LABELS vague a1 a3c_rec b18c typmen5 c8 e5 f1_rec g5 h1a h4 i1 j1 k3
    96 "No surveyed".
MISSING VALUES vague (96 THRU HIGHEST). 
RECODE a2a_rec b18_rec e2a_rec e2auc i8
    (SYSMIS = 996).
ADD VALUE LABELS a2a_rec b18_rec e2a_rec e2auc i8
    996 "No surveyed". 
RECODE pcs6
    (SYSMIS = 9996).
ADD VALUE LABELS pcs6
    9996 "No surveyed".
RECODE pcs18
    (SYSMIS = 99996).
ADD VALUE LABELS pcs18
    99996 "No surveyed".
EXECUTE.

* saving the temporary file.

SAVE /OUTFILE "work_folder\annual_survey.sav"
/KEEP [list of the variables included in the annual survey file 'annual_survey' and the imputed variables].
DATASET CLOSE ALL.
GET FILE="work_folder\annual_survey.sav".
!PREVAR VLIST= A2A_rec [list of the variables included in the annual survey file 'annual_survey']
/PREFIX=[prefix for the annual survey variables].
!PREVAR VLIST= VAGUE A1 A3C_rec PCS18 PCS6 B18_rec B18C TYPMEN5
    C8 E2A_rec E2AUC E5 F1_rec G5 H1A H4 I1 I8 J1 K3
/PREFIX=[prefix for the annual survey variables imputed].
DATASET NAME annual_survey.
SORT CASES BY uid(A).

**************************************************.
* Part 3 : Preparation of census data for matching.
**************************************************.

GET FILE="[address of the census file]".
DATASET NAME census_variables.
SORT CASES BY uid(A).
!PREVAR VLIST=NUTS1 CATAEU2010 TAU2014 TUU2014
/PREFIX=[prefix for the census variables].

**********************************************************.
* Part 4 : Preparation of weighting variables for matching.
**********************************************************.

* Weighting of the survey. 

GET SAS DATA="[address of the weighting file]".
DATASET NAME weight.
SORT CASES BY uid(A).

VARIABLE LABELS
    [weighting variable name] 'Individual weight'.
EXECUTE.

VARIABLE LABELS sex "Sex of the respondant (calibration)".
VALUE LABELS sex
    1 "Male"
    2 "Female"
    9 "Missing value"
.
MISSING VALUES SEX  (9 THRU HIGHEST).
VARIABLE LABELS age1 "Ten-year age (pilot panel calibration)".
VALUE LABELS age1
    1 "18-24 years old"
    2 "25-34 years old"
    3 "35-44 years old"
    4 "45-54 years old"
    5 "55-64 years old"
    6 "65-79 years old"
    9 "Missing value"
.
MISSING VALUES AGE1 (9 THRU HIGHEST).
VARIABLE LABELS age2 "Ten-year age (calibration)".
VALUE LABELS age2
    1 "18-22 years old"
    2 "23-34 years old"
    3 "35-44 years old"
    4 "45-54 years old"
    5 "55-64 years old"
    6 "65-75 years old"
    7 "76-79 years old"
    9 "Missing value"
.
MISSING VALUES AGE2 (9 THRU HIGHEST).
VARIABLE LABELS nat "Nationality (calibration)".
VALUE LABELS nat
    1 "French"
    2 "Has acquired French nationality"
    3 "Foreign nationality"
    9 "Missing value"
.
MISSING VALUES NAT (9 THRU HIGHEST).
VARIABLE LABELS dipl "Diploma (calibration)".
VALUE LABELS dipl
    1 "None/CEP/BEPC"
    2 "CAP/BEP"
    3 "Bac/Bac+2"
    4 "Bac+3 and more"
    9 "Missing value"
.
MISSING VALUES DIPL  (9 THRU HIGHEST).
VARIABLE LABELS zeat "Research and National Development Zones (ZEAT) (calibration)".
VALUE LABELS zeat
    1 "Région parisienne"
    2 "Bassin parisien"
    3 "Nord"
    4 "Est"
    5 "Ouest"
    6 "Sud-ouest"
    7 "Centre-est"
    8 "Méditerranée"
    9 "Missing value"
.
MISSING VALUES zeat (9 THRU HIGHEST).
!PREVAR VLIST= SEXE AGE1 AGE2 NAT DIPL ZEAT   
/PREFIX=cal.

* Longitudinal weighting of the survey. 

GET SAS DATA="[address of the longitudinal weighting file]".
EXECUTE.
ALTER TYPE [longitudinal weighting variable name] (F16.14).
DATASET NAME longitudinal_weight.
VARIABLE LABELS
   [longitudinal weighting variable name] 'Individual longitudinal weight'
.
VARIABLE ATTRIBUTE VARIABLES=[longitudinal weighting variable name]  ATTRIBUTE=question[1]("Weighting variable").
SORT CASES BY uid(A).

*[...reproduce for each longitudinal weighting file...].

**************************************************.
* Part 5 : Preparation of survey file for matching.
**************************************************.

GET FILE="[address of the survey file]"
/KEEP UID [list of the variables].
EXECUTE.
DATASET NAME survey.
SORT CASES BY uid(A).
!PREVAR VLIST= [list of the variables]
/PREFIX=[prefix for the survey variables].

* Anonymization for the survey id.

GET FILE "[address of the anonymization id file]"
/KEEP UID UID_ANONYM.
DATASET NAME anonymization_file.
VARIABLE LABELS
   uid_anonym 'Id respondant'.
VARIABLE ATTRIBUTE VARIABLES= uid_anonym   ATTRIBUTE=question[1]('Random variable').
ALTER TYPE uid_anonym (F3.0).
SORT CASES BY uid(A).


*******************************************.
* Part 5 : Matching of the differents files.
*******************************************.

DATASET ACTIVATE anonymization_file.
MATCH FILES /TABLE=*
    /FILE='survey'
    /TABLE= 'annual_survey'
    /TABLE= 'census_variables'
    /TABLE= 'weight'
    /TABLE= 'longitudinal_weight'
    /BY uid.
EXECUTE.
SORT CASES BY uid_anonym (A).
DATAFILE ATTRIBUTE ATTRIBUTE=OriginalVersion('[number of version]') CreationDate('[creation date]').

SAVE /OUTFILE "[address of the final survey file with the matched variables]"
/DROP uid
/RENAME (uid_anonym =[name of the anonymazed id])
/COMPRESSED.
DATASET CLOSE ALL.

*************************************************.
* Part 5 : Export of the data file in multiformat.
*************************************************.

GET FILE "[address of the final survey file with the matched variables]".

SAVE TRANSLATE OUTFILE="[address of the final survey file with the matched variables]"
  /TYPE=CSV
  /ENCODING='UTF8'
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES
 /TEXTOPTIONS QUALIFIER='"' DELIMITER=';'.

SAVE TRANSLATE OUTFILE="[address of the final survey file with the matched variables]"
  /TYPE=SAS
  /VERSION=9
  /PLATFORM=WINDOWS
  /ENCODING='UTF8'
  /MAP
  /REPLACE
  /VALFILE="[address of the final survey file with the matched variables]".

SAVE TRANSLATE OUTFILE="[address of the final survey file with the matched variables]"
  /TYPE=STATA
  /VERSION=9
  /EDITION=SE
  /MAP
  /REPLACE.




