*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIDT00267......................................*
DATA:  BEGIN OF STATUS_ZFIDT00267                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIDT00267                    .
CONTROLS: TCTRL_ZFIDT00267
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIDT00267                    .
TABLES: ZFIDT00267                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
