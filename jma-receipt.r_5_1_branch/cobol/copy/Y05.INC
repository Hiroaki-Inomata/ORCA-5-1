        02  Y05.
    ******* 02  Y05-FIXED1.
              03  Y05-PTNUM.
                04  Y05-VALUE   PIC X(20).
              03  Y05-NAME.
                04  Y05-VALUE   PIC X(50).
              03  Y05-SEX.
                04  Y05-VALUE   PIC X(2).
              03  Y05-BIRTHDAY.
                04  Y05-VALUE   PIC X(10).
              03  Y05-SCROLLEDWINDOW1.
                04  Y05-TSLIST.
                  05  Y05-COUNT   PIC S9(9)   BINARY.
                  05  Y05-ROW PIC S9(9)   BINARY.
                  05  Y05-ROWATTR PIC S9(9)   BINARY.
                  05  Y05-ITEM        OCCURS  100 TIMES.
                    06  Y05-BANGO   PIC X(5).
                    06  Y05-YYKYMD  PIC X(14).
                    06  Y05-YYKTIME PIC X(5).
                    06  Y05-SRYKA   PIC X(20).
                    06  Y05-SINRYO  PIC X(24).
                    06  Y05-DRNAME  PIC X(50).
                    06  Y05-YYKKBN  PIC X(30).
                    06  Y05-RENRAKU PIC X(20).
                    06  Y05-RAIIN   PIC X(4).
                  05  Y05-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  Y05-MIDASI.
                04  Y05-VALUE   PIC X(18).
