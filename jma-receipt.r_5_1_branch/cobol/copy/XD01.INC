        02  XD01.
    ******  02  XD01-FIXED1.
              03  XD01-PTNUM.
                04  XD01-VALUE  PIC X(20).
              03  XD01-NAME.
                04  XD01-VALUE  PIC X(50).
              03  XD01-SEX.
                04  XD01-VALUE  PIC X(2).
              03  XD01-BIRTHDAY.
                04  XD01-VALUE  PIC X(10).
              03  XD01-SRYYMD.
                04  XD01-VALUE  PIC X(10).
              03  XD01-SCROLLEDWINDOW1.
                04  XD01-MLIST.
                  05  XD01-MLIST-COUNT  PIC S9(9)   BINARY.
                  05  XD01-MLIST-ROW    PIC S9(9)   BINARY.
                  05  XD01-MLIST-ROWATTR    PIC S9(9)   BINARY.
                  05  XD01-ITEM       OCCURS  10 TIMES.
                    06  XD01-TNUM    PIC X(3).
                    06  XD01-TSYORINM PIC X(30).
                    06  XD01-TSRYKA   PIC X(12).
                    06  XD01-TDRCD    PIC X(24).
                    06  XD01-THKNCOMBI PIC X(60).
                    06  XD01-TPRTNM    PIC X(16).
                  05  XD01-MLIST-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  XD01-SELNUM.
                04  XD01-VALUE  PIC S9(3).
              03  XD01-PRTNM.
                04  XD01-VALUE  PIC X(16).
              03  XD01-HKNCOMBI-COMB.
                04  XD01-HKNCOMBI.
                  05  XD01-VALUE  PIC X(65).
                04  XD01-HKNCOMBI-COUNT  PIC S9(9)   BINARY.
                04  XD01-HKNCOMBI-ITEM
                        PIC X(65)   OCCURS  30 TIMES.
              03  XD01-DRCD-COMB.
                04  XD01-DRCD.
                  05  XD01-VALUE  PIC X(35).
                04  XD01-DRCD-COUNT  PIC S9(9)   BINARY.
                04  XD01-DRCD-ITEM
                        PIC X(35)   OCCURS  99 TIMES.
              03  XD01-SRYKA-COMB.
                04  XD01-SRYKA.
                  05  XD01-VALUE  PIC X(15).
                04  XD01-SRYKA-COUNT  PIC S9(9)   BINARY.
                04  XD01-SRYKA-ITEM
                        PIC X(15)   OCCURS  40 TIMES.
              03  XD01-SYORINM-COMB.
                04  XD01-SYORINM.
                  05  XD01-VALUE  PIC X(30).
                04  XD01-SYORINM-COUNT  PIC S9(9)   BINARY.
                04  XD01-SYORINM-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
              03  XD01-SYORIMEI.
                04  XD01-VALUE  PIC X(40).
