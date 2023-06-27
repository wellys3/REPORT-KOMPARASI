*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIDT00266......................................*
DATA:  BEGIN OF STATUS_ZFIDT00266                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIDT00266                    .
CONTROLS: TCTRL_ZFIDT00266
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIDT00266                    .
TABLES: ZFIDT00266                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
