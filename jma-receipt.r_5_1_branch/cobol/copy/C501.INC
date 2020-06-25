        02  C501.
      ******02  C501-FIXED1.
              03  C501-SELNUM.
                04  C501-VALUE  PIC S9(3).
              03  C501-SCROLLEDWINDOW1.
                04  C501-BUNREILIST.
                  05  C501-BUNREI-COUNT  PIC S9(9)   BINARY.
                  05  C501-BUNREI-ROW PIC S9(9)   BINARY.
                  05  C501-BUNREI-ROWATTR PIC S9(9)   BINARY.
                  05  C501-BUNREI-ITEM       OCCURS  200 TIMES.
                    06  C501-TRENNUM PIC X(3).
                    06  C501-TCOMMENT PIC X(400).
                  05  C501-BUNREI-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  C501-NEXT.
                04  C501-NEXT-STYLE   PIC X(20).
                04  C501-NEXT-VALUE   PIC X(20).

