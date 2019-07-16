************************************************************************.
* name: ano_multi_wave_template.sps.
* author: Alexandre Mairot (alexandre.mairot@sciencespo.fr).
* title: template anonymization syntax for data file ELIPSS.
* program: SPSS.
* version: 1.0.
************************************************************************.

DATASET CLOSE ALL.
OUTPUT CLOSE ALL.

* Address of the survey files. 

FILE HANDLE file_survey /NAME="[address of the data file wave N]".
FILE HANDLE file_ano_id /NAME="[address of the anonymization id file wave N]".

* Address of the work folder.

FILE HANDLE work_folder /NAME="[adress of the work folder]".

* Address of the previous N-1 waves anonymization files.  

GET FILE="[address of the 1st wave anonymization file]".
SELECT IF (wave EQ 1).
SORT CASES BY uid(A).
DATASET NAME [the 1st wave id anonymization file].

*[...reproduce for each previous wave...].

************************************************************************.
* Part 1 : creation of the table of anonymization for the previous waves.
************************************************************************.

DATASET ACTIVATE [the 1st wave id anonymization file].
ADD FILES /FILE=*
    /FILE='[...add the N-1 previous waves id anonymization files...]'.
EXECUTE.
SORT CASES BY uid(A).
SAVE /OUTFILE "work_folder\uid_prev_round.sav"
/KEEP uid vague uid_anonym.
STRING key (A1).
COMPUTE key='A'.
EXECUTE.
AGGREGATE
  /OUTFILE "work_folder\uid_max.sav"
  /BREAK= key
  /uid_nbre=MAX(uid_anonym).
  
*******************************************************************.
* Part 2 : creation of the table of anonymization for all the waves.
*******************************************************************.  
 
GET FILE="file_survey"
/KEEP uid.
DATASET NAME anonym.
SORT CASES BY uid(A).
MATCH FILES /FILE=*
    /TABLE='[the 1st wave id anonymization file]'
   /BY uid.
EXECUTE.
SELECT IF (key NE 'A').
EXECUTE.
COMPUTE alea_id=RV.CAUCHY(0,1).
EXECUTE.
COMPUTE key='A'.
GET FILE="work_folder\uid_max.sav".
DATASET NAME uid_max.
DATASET ACTIVATE anonym.
MATCH FILES /FILE=*
   /TABLE='uid_max'
   /BY key.
EXECUTE.
DATASET ACTIVATE anonym.
SORT CASES BY alea_id(A).
COMPUTE UID_ANONYM=$CASENUM+uid_nbre.
COMPUTE VAGUE=[the number of the wave].
EXECUTE.
DELETE VARIABLES alea_id key uid_nbre.
DATASET CLOSE uid_max.
ERASE FILE="work_folder\uid_max.sav".
GET FILE="work_folder\uid_prev_round.sav".
DATASET NAME all_wave.
ADD FILES /FILE= *
    /FILE='anonym'.
EXECUTE.
SORT CASES BY uid(A).
SAVE /OUTFILE "work_folder\uid_all_wave.sav".
DATASET CLOSE ALL.
GET FILE="work_folder\uid_all_wave.sav".
DATASET NAME all_wave.
ERASE FILE="work_folder\uid_prev_round.sav".

**********************************************************************.
* Part 3 : creation of the table of anonymization for the current wave.
**********************************************************************. 

* Creation of the anonymization file.

GET FILE="file_survey"
/KEEP uid.
DATASET NAME anonym.
SORT CASES BY uid(A).
MATCH FILES /FILE=*
   /TABLE='all_wave'
   /BY uid.
EXECUTE.
VARIABLE LABELS
   uid_anonym 'Id respondant'
   wave 'Wave of the survey'.
VARIABLE ATTRIBUTE VARIABLES= uid_anonym   ATTRIBUTE=question[1]('Random variable').
VARIABLE ATTRIBUTE VARIABLES= wave   ATTRIBUTE=question[1]('Id wave of the survey').
ALTER TYPE uid_anonym (F3.0).
SORT CASES BY uid (A).

* Saving the anonymization id file.

SAVE /OUTFILE "file_ano_id"
/KEEP uid vague uid_anonym
/COMPRESSED.
DATASET CLOSE ALL.
GET FILE="file_ano_id".
ERASE FILE="work_folder\uid_all_wave.sav".
