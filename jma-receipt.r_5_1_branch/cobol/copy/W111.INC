        02  W111.
     ****** 02  W111-FIXED32.
              03  W111-SCROLLEDWINDOW28.
                04  W111-MEILIST.
                  05  W111-MEILIST-COUNT  PIC S9(9)   BINARY.
                  05  W111-MEILIST-ROW    PIC S9(9)   BINARY.
                  05  W111-MEILIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W111-ITEM       OCCURS  100 TIMES.
                    06  W111-TRENNUM PIC X(3).
                    06  W111-TFORMID PIC X(20).
                    06  W111-TPRTNM  PIC X(16).
                    06  W111-TOFFSET-X PIC X(5).
                    06  W111-TOFFSET-Y PIC X(5).
                  05  W111-MEILIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  W111-FORMID.
                04  W111-VALUE  PIC X(20).
              03  W111-PRTNM.
                04  W111-VALUE  PIC X(16).
              03  W111-OFFSET-X.
                04  W111-VALUE  PIC X(5).
              03  W111-OFFSET-Y.
                04  W111-VALUE  PIC X(5).
              03  W111-SELNUM.
                04  W111-VALUE  PIC S9(3).
