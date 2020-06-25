        02  U012.
      ******02  U012-FIXED1.
              03  U012-SRYYMD.
                04  U012-VALUE  PIC X(30).
              03  U012-SRYNAIYO-COMB.
                04  U012-SRYNAIYO.
                  05  U012-VALUE  PIC X(21).
                04  U012-SRYNAIYO-COUNT  PIC S9(9)   BINARY.
                04  U012-SRYNAIYO-ITEM
                        PIC X(21)   OCCURS  100 TIMES.
              03  U012-SRYKA-COMB.
                04  U012-SRYKA.
                  05  U012-VALUE  PIC X(21).
                04  U012-SRYKA-COUNT  PIC S9(9)   BINARY.
                04  U012-SRYKA-ITEM
                        PIC X(21)   OCCURS  100 TIMES.
              03  U012-DRCD-COMB.
                04  U012-DRCD.
                  05  U012-VALUE  PIC X(29).
                04  U012-DRCD-COUNT  PIC S9(9)   BINARY.
                04  U012-DRCD-ITEM
                        PIC X(29)   OCCURS  100 TIMES.
              03  U012-SCROLLEDWINDOW1.
                04  U012-HPOS   PIC S9(9)   BINARY.
                04  U012-VPOS   PIC S9(9)   BINARY.
                04  U012-LIST.
                  05  U012-LIST-COUNT  PIC S9(9)   BINARY.
                  05  U012-ROW    PIC S9(9)   BINARY.
                  05  U012-ITEM       OCCURS  400 TIMES.
                    06  U012-LNO    PIC X(4).
                    06  U012-LPTNUM PIC X(20).
                    06  U012-LNAME  PIC X(50).
                    06  U012-LNENREI    PIC X(6).
                    06  U012-LNICKNAME  PIC X(50).
                    06  U012-LCONDITION PIC X(6).
                    06  U012-LSRYKAMEI  PIC X(6).
                    06  U012-LSRYNAIYOMEI PIC X(8).
                    06  U012-LDRCD      PIC X(24).
                    06  U012-LHKNCOMBI  PIC X(50).
                    06  U012-LUKETIME    PIC X(8).
                    06  U012-LYYKTIME    PIC X(6).
                    06  U012-LKAITIME    PIC X(6).
                  05  U012-LIST-SELECT
                          PIC X       OCCURS  400 TIMES.
              03  U012-MIDASI.
                04  U012-VALUE  PIC X(12).
              03  U012-NEXT.
                04  U012-NEXT-STYLE   PIC X(20).
                04  U012-NEXT-VALUE   PIC X(20).
