        02  K061.
     ****** 02  K061-FIXED1.
              03  K061-SELNUM.
                04  K061-VALUE  PIC S9(3).
              03  K061-SRYCD-G.
                04  K061-SRYCD-STATE   PIC S9(9)   BINARY.
                04  K061-SRYCD  PIC X(9).
              03  K061-SRYMEI.
                04  K061-VALUE  PIC X(80).
              03  K061-SANYMD-G.
                04  K061-SANYMD-STATE   PIC S9(9)   BINARY.
                04  K061-SANYMD  PIC X(11).
              03  K061-SYOYMD-G.
                04  K061-SYOYMD-STATE   PIC S9(9)   BINARY.
                04  K061-SYOYMD  PIC X(11).
              03  K061-KAISU-G.
                04  K061-KAISU-STATE   PIC S9(9)   BINARY.
                04  K061-KAISU  PIC S9(3).
              03  K061-HKNCOMBI-G.
                04  K061-HKNCOMBI-STATE   PIC S9(9)   BINARY.
                04  K061-HKNCOMBI  PIC X(4).
              03  K061-SCROLLEDWINDOW1.
                04  K061-HPOS   PIC S9(9)   BINARY.
                04  K061-VPOS   PIC S9(9)   BINARY.
                04  K061-RRKLIST.
                  05  K061-RRKLIST-COUNT  PIC S9(9)   BINARY.
                  05  K061-RRKLIST-ROW    PIC S9(9)   BINARY.
                  05  K061-ITEM       OCCURS  100 TIMES.
                    06  K061-TNUM   PIC X(3).
                    06  K061-TSRYCD PIC X(9).
                    06  K061-TMEISYO PIC X(80).
                    06  K061-TSANYMD PIC X(9).
                    06  K061-TSYOYMD PIC X(9).
                    06  K061-TKAISU  PIC X(2).
                    06  K061-THKNCOMBI PIC X(20).
                    06  K061-TKUBUN    PIC X(6).
                    06  K061-TENDYMD   PIC X(16).
                  05  K061-RRKLIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  K061-MIDASI.
                04  K061-MIDASI-STYLE  PIC X(20).
                04  K061-MIDASI-VALUE  PIC X(160).
              03  K061-HKNCOMBIMEI.
                04  K061-VALUE  PIC X(30).
              03  K061-LSTYMD-G.
                04  K061-LSTYMD-STATE  PIC S9(9)   BINARY.
                04  K061-LSTYMD  PIC X(11).
              03  K061-MIDASI2.
                04  K061-VALUE  PIC X(10).
              03  K061-B08.
                04  K061-B08-STATE   PIC S9(9)   BINARY.
                04  K061-B08-LABEL   PIC X(16).
