            02  R04.
              03  R04-B09.
                04  R04-B09-STATE   PIC S9(9)   BINARY.
              03  R04-B10.
                04  R04-B10-STATE   PIC S9(9)   BINARY.
              03  R04-TSTDATA-G    OCCURS  25.
                04  R04-TSTDATA-STATE   PIC S9(09) BINARY.
                04  R04-TSTDATA.
                  06  R04-VALUE   PIC X(100).
              03  R04-TEDDATA-G    OCCURS  25.
                04  R04-TEDDATA-STATE   PIC S9(09) BINARY.
                04  R04-TEDDATA.
                  06  R04-VALUE   PIC X(100).
              03  R04-TPAGE-G      OCCURS  25.
                04  R04-TPAGE-STYLE   PIC X(20).
                04  R04-TPAGE         PIC S9(4).
              03  R04-TKENSU-G     OCCURS  25.
                04  R04-TKENSU-STYLE  PIC X(20).
                04  R04-TKENSU        PIC S9(4).
              03  R04-TCHK-G    OCCURS  25.
                04  R04-TCHK-STATE   PIC S9(9)   BINARY.
                04  R04-TCHK-STYLE   PIC X(20).
                04  R04-TCHK-VALUE   PIC X.
                04  R04-TCHK-LABEL   PIC X(41).
              03  R04-FILEKBN.
                04  R04-VALUE     PIC X(94).
              03  R04-CREYMD.
                04  R04-VALUE     PIC X(22).
              03  R04-KENSU.
                04  R04-VALUE   PIC 9(4).
              03  R04-PAGE.
                04  R04-VALUE   PIC 9(4).
              03  R04-PREFKBN-COMBO.
                04  R04-PREFKBN.
                  06  R04-VALUE   PIC X(6).
                04  R04-PREFKBN-COUNT   PIC S9(9)   BINARY.
                04  R04-PREFKBNLST  PIC X(6)   OCCURS  3 TIMES.
              03  R04-PRTJYUN-COMBO.
                04  R04-PRTJYUN.
                  06  R04-VALUE   PIC X(26).
                04  R04-PRTJYUN-COUNT   PIC S9(9)   BINARY.
                04  R04-PRTJYUNLST  PIC X(26)   OCCURS  5 TIMES.
