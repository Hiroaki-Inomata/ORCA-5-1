        02  P02Y.
      ***** 02  P02Y-FIXED1.
              03  P02Y-SELNUM.
                04  P02Y-VALUE  PIC S9(3).
              03  P02Y-MIDASI.
                04  P02Y-VALUE  PIC X(22).
              03  P02Y-SRYCD.
                04  P02Y-VALUE  PIC X(9).
              03  P02Y-SRYMEI.
                04  P02Y-VALUE  PIC X(80).
              03  P02Y-SCROLLEDWINDOW1.
                04  P02Y-HPOS   PIC S9(9)   BINARY.
                04  P02Y-VPOS   PIC S9(9)   BINARY.
                04  P02Y-PTKINKILST.
                  05  P02Y-COUNT  PIC S9(9)   BINARY.
                  05  P02Y-ROW    PIC S9(9)   BINARY.
                  05  P02Y-ITEM       OCCURS  100 TIMES.
                    06  P02Y-TNUM   PIC X(3).
                    06  P02Y-TSRYCD PIC X(9).
                    06  P02Y-TINPCD PIC X(20).
                    06  P02Y-TNAME  PIC X(66).
                    06  P02Y-TYUKOEDYMD PIC X(10).
                    06  P02Y-TCHNGCD    PIC X(09).
                    06  P02Y-TTOUYOYMD  PIC X(9).
                    06  P02Y-TKINKSTYMD PIC X(9).
                  05  P02Y-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  P02Y-YUKOKIKAN.
                04  P02Y-VALUE  PIC X(24).
              03  P02Y-KEIKAMSG.
                04  P02Y-VALUE  PIC X(56).
              03  P02Y-INPUTCDMSG.
                04  P02Y-VALUE  PIC X(21).
              03  P02Y-KINKSTYMD.
                04  P02Y-VALUE  PIC X(11).
              03  P02Y-TOUYOYMD.
                04  P02Y-VALUE  PIC X(11).
              03  P02Y-GRPHOSPNUM-COMB.
                04  P02Y-GRPHOSPNUM.
                  05  P02Y-VALUE  PIC X(23).
                04  P02Y-GRPHOSPNUM-COUNT  PIC S9(9)   BINARY.
                04  P02Y-GRPHOSPNUM-ITEM
                        PIC X(23)   OCCURS  9 TIMES.
              03  P02Y-B03.
                04  P02Y-B03-STATE  PIC S9(9)   BINARY.
              03  P02Y-B08.
                04  P02Y-B08-STATE  PIC S9(9)   BINARY.
              03  P02Y-B12.
                04  P02Y-B12-STATE  PIC S9(9)   BINARY.
              03  P02Y-B09.
                04  P02Y-B09-STATE  PIC S9(9)   BINARY.
                04  P02Y-B09-LABEL  PIC X(20).
              03  P02Y-B10.
                04  P02Y-B10-STATE  PIC S9(9)   BINARY.
                04  P02Y-B10-LABEL  PIC X(20).
