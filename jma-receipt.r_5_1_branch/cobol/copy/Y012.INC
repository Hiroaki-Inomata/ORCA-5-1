        02  Y012.
     *******02  Y012-FIXED1.
              03  Y012-BUNNUM.
                04  Y012-VALUE  PIC S9(3).
              03  Y012-MIDASI.
                04  Y012-VALUE  PIC X(17).
              03  Y012-HYOJUN.
                04  Y012-VALUE  PIC S9(1).
              03  Y012-COMMENT.
                04  Y012-VALUE  PIC X(80).
              03  Y012-SCROLLEDWINDOW1.
                04  Y012-HPOS   PIC S9(9)   BINARY.
                04  Y012-VPOS   PIC S9(9)   BINARY.
                04  Y012-BUNLIST.
                  05  Y012-BUNLIST-COUNT  PIC S9(9)   BINARY.
                  05  Y012-BUNLIST-ROW    PIC S9(9)   BINARY.
                  05  Y012-ITEM       OCCURS  100 TIMES.
                    06  Y012-LNUM     PIC X(3).
                    06  Y012-LHYOJI   PIC X(1).
                    06  Y012-LCOMMENT PIC X(80).
                  05  Y012-BUNLIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  Y012-B12.
                04  Y012-B12-STATE  PIC S9(9)   BINARY.
