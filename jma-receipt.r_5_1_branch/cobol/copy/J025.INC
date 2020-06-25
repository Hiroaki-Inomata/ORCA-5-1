        02  J025.
    ******  02  J025-KNMINO.
              03  J025-SCROLLEDWINDOW1.
                04  J025-SYUNOULIST.
                  05  J025-SYUNOULIST-COUNT  PIC S9(9)   BINARY.
                  05  J025-SYUNOULIST-ROW    PIC S9(9)   BINARY.
                  05  J025-SYUNOULIST-ROWATTR    PIC S9(9)   BINARY.
                  05  J025-ITEM       OCCURS  100 TIMES.
                    06  J025-DENPNUM PIC X(7).
                    06  J025-SRYKA   PIC X(6).
                    06  J025-HKNCOMBI   PIC X(50).
                    06  J025-FTNRATE    PIC X(3).
                    06  J025-DENPPRTYMD  PIC X(10).
                    06  J025-SRYYMD      PIC X(10).
                    06  J025-MAE-SKYMONEY PIC X(10).
                    06  J025-ATO-SKYMONEY PIC X(10).
                    06  J025-KBNMEI      PIC X(6).
                  05  J025-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  J025-ERRMSG.
                04  J025-ERRMSG-STYLE  PIC X(20).
                04  J025-ERRMSG-VALUE  PIC X(100).
