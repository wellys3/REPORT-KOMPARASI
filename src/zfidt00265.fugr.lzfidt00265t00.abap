*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIDT00265......................................*
DATA:  BEGIN OF STATUS_ZFIDT00265                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIDT00265                    .
CONTROLS: TCTRL_ZFIDT00265
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIDT00265                    .
TABLES: ZFIDT00265                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
