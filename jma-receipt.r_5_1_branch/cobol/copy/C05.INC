            02  C05.
              03  C05-SELNUM1.
                04  C05-VALUE   PIC S9(5).
              03  C05-EDITBYOMEI.
                04  C05-VALUE   PIC X(100).
              03  C05-BYOMEIKEN.
                04  C05-VALUE   PIC X(30).
              03  C05-SCR-BYOMEILST.
                04  C05-BYOMEILST.
                  05  C05-COUNT        PIC S9(9)   BINARY.
                  05  C05-ROW          PIC S9(9)   BINARY.
                  05  C05-ROWATTR      PIC S9(9)   BINARY.
                  05  C05-ITEM         OCCURS  100 TIMES.
                    06  C05-TBANGO     PIC X(5).
                    06  C05-TSYUBET    PIC X(6).
                    06  C05-TIKOSAKICD PIC X(4).
                    06  C05-TBYOMEI    PIC X(54).
                    06  C05-THAISIYMD  PIC X(9).
                    06  C05-TBYOCD     PIC X(7).
                  05  C05-SELECT       PIC X       OCCURS  100 TIMES.
              03  C05-SELNUM2.
                04  C05-VALUE   PIC S9(2).
              03  C05-SCR-BYOMEICDLST.
                04  C05-BYOMEICDLST.
                  05  C05-BYOMEICDLST-COUNT   PIC S9(9)   BINARY.
                  05  C05-BYOMEICDLST-ROW     PIC S9(9)   BINARY.
                  05  C05-BYOMEICDLST-ROWATTR PIC S9(9)   BINARY.
                  05  C05-BYOMEICDLST-ITEM    OCCURS  21 TIMES.
                    06  C05-TBANGO1     PIC X(2).
                    06  C05-TBYOMEICD1  PIC X(7).
                    06  C05-TBYOMEI1    PIC X(80).
                  05  C05-BYOMEICDLST-SELECT  PIC X OCCURS  21 TIMES.
