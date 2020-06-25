           02  W122.
              03  W122-SELNUM.
                04  W122-VALUE  PIC S9(2).
              03  W122-STYUKYMD.
                04  W122-VALUE  PIC X(10).
              03  W122-EDYUKYMD.
                04  W122-VALUE  PIC X(10).
              03  W122-GRP-NO.
                04  W122-VALUE  PIC S9(2).
              03  W122-KOHNUM.
                04  W122-VALUE  PIC X(3).
              03  W122-SEIDONAME.
                04  W122-VALUE  PIC X(50).
              03  W122-SCR1.
                04  W122-HEADLIST.
                  05  W122-HEADLIST-COUNT      PIC S9(9)   BINARY.
                  05  W122-HEADLIST-ROW        PIC S9(9)   BINARY.
                  05  W122-HEADLIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W122-HEADLIST-ITEM       OCCURS  10 TIMES.
                    06  W122-HEADLIST-TNO       PIC X(2).
                    06  W122-HEADLIST-TSTYUKYMD PIC X(10).
                    06  W122-HEADLIST-TEDYUKYMD PIC X(10).
                  05  W122-HEADLIST-SELECT   PIC X OCCURS  10 TIMES.
              03  W122-SCR3.
                04  W122-BODYLIST.
                  05  W122-BODYLIST-COUNT      PIC S9(9)   BINARY.
                  05  W122-BODYLIST-ROW        PIC S9(9)   BINARY.
                  05  W122-BODYLIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W122-BODYLIST-ITEM       OCCURS  50 TIMES.
                    06  W122-BODYLIST-TGRP       PIC X(2).
                    06  W122-BODYLIST-TKOHNUM    PIC X(3).
                    06  W122-BODYLIST-TSEIDONAME PIC X(50).
                  05  W122-BODYLIST-SELECT   PIC X OCCURS  50 TIMES.
              03  W122-KOHNUM-UP-STATE    PIC S9(9)   BINARY.
              03  W122-KOHNUM-DOWN-STATE  PIC S9(9)   BINARY.
