************************************************************************.
* name: ano_unique_wave_template.sps.
* author: Alexandre Mairot (alexandre.mairot@sciencespo.fr).
* title: template anonymization syntax for data file ELIPSS.
* program: SPSS.
* version: 1.0.
************************************************************************.

DATASET CLOSE ALL.
OUTPUT CLOSE ALL.

* Address of the survey files.

FILE HANDLE file_survey /NAME="[address of the data file]".
FILE HANDLE file_ano_id /NAME="[address of the anonymization id file]".

* Creation of the anonymization file for the 1st or the unique wave.

GET FILE="file_survey"
/KEEP uid.
DATASET NAME anonym.
SORT CASES BY uid(A).
COMPUTE alea_id=RV.CAUCHY(0,1).
EXECUTE.
DATASET ACTIVATE anonym.
SORT CASES BY alea_id(A).
COMPUTE UID_ANONYM=$CASENUM.
COMPUTE WAVE=1.
EXECUTE.
SORT CASES BY uid(A).
VARIABLE LABELS
   uid_anonym 'Id respondant'
   wave 'Wave of the survey'.
VARIABLE ATTRIBUTE VARIABLES= uid_anonym   ATTRIBUTE=question[1]('Random variable').
VARIABLE ATTRIBUTE VARIABLES= wave   ATTRIBUTE=question[1]('Id wave of the survey').
ALTER TYPE uid_anonym (F3.0).
SORT CASES BY uid (A).

* Saving the anonymization id file.

SAVE /OUTFILE "file_ano_id"
/KEEP uid wave uid_anonym
/COMPRESSED.
DATASET CLOSE ALL.
GET FILE="file_ano_id".