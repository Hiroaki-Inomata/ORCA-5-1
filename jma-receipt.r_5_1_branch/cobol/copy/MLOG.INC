            02  MLOG.
              03  MLOG-MESSAGE.
                04  MLOG-MES1    PIC X(200).
              03  MLOG-SELNUM01.
                04  MLOG-SEL1    PIC S9(3).
              03  MLOG-SCROLLEDWINDOW26.
                04  MLOG-HPOS    PIC S9(9)   BINARY.
                04  MLOG-VPOS    PIC S9(9)   BINARY.
                04  MLOG-LIST.
                  05  MLOG-LIST-COUNT   PIC S9(9)   BINARY.
                  05  MLOG-ROW PIC S9(9)   BINARY.
                  05  MLOG-ITEM-LOGIN      OCCURS  200 TIMES.
                    06  MLOG-ITEM-TNO       PIC X(2).
                    06  MLOG-ITEM-HOSPNUM   PIC X(2).
                    06  MLOG-ITEM-MINE      PIC X(2).
                    06  MLOG-ITEM-USER      PIC X(16).
                    06  MLOG-ITEM-HOST      PIC X(30).
                    06  MLOG-ITEM-SCREEN    PIC X(64).
                    06  MLOG-ITEM-GYOMU     PIC X(40).
                    06  MLOG-ITEM-START     PIC X(20).
                    06  MLOG-ITEM-LAST      PIC X(10).
                  05  MLOG-ITEM-SELECT
                          PIC X        OCCURS  200 TIMES.
              03  MLOG-IRYO-NO-COMBO.
                04  MLOG-IRYONO.
                  05  MLOG-VALUE      PIC X(100).
                04  MLOG-IRYONO-STATE   PIC S9(9)   BINARY.
                04  MLOG-IRYONO-COUNT   PIC S9(9)   BINARY.
                04  MLOG-IRYONO-ITEM    PIC X(100)   OCCURS  200 TIMES.
              03  MLOG-B05.
                04  MLOG-B05-STATE         PIC S9(9)   BINARY.
              03  MLOG-B06.
                04  MLOG-B06-STATE         PIC S9(9)   BINARY.
              03  MLOG-B09.
                04  MLOG-B09-STATE         PIC S9(9)   BINARY.
              03  MLOG-B10.
                04  MLOG-B10-STATE         PIC S9(9)   BINARY.
