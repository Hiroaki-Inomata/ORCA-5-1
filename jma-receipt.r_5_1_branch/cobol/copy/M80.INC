        02  M80.
     ****** 02  M80-FIXED1.
              03  M80-SCROLLEDWINDOW2.
                04  M80-HPOS    PIC S9(9)   BINARY.
                04  M80-VPOS    PIC S9(9)   BINARY.
                04  M80-LIST.
                  05  M80-LIST-COUNT   PIC S9(9)   BINARY.
                  05  M80-LIST-ROW PIC S9(9)   BINARY.
                  05  M80-ITEM        OCCURS  100 TIMES.
                    06  M80-TNUM      PIC X(4).
                    06  M80-TPGMEISYO PIC X(100).
                    06  M80-TPGNAME   PIC X(50).
                    06  M80-TVERSION  PIC X(20).
                    06  M80-TVERSION2 PIC X(20).
                    06  M80-TVENDOR   PIC X(20).
                    06  M80-TLINK     PIC X(10).
                  05  M80-LIST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  M80-SELNUM.
                04  M80-VALUE   PIC S9(4).
              03  M80-MSGPAGE.
                04  M80-MSGPAGE-STYLE   PIC X(20).
                04  M80-MSGPAGE-VALUE   PIC X(20).
              03  M80-SYORIMSG.
                04  M80-SYORIMSG-STYLE   PIC X(20).
                04  M80-SYORIMSG-VALUE   PIC X(60).
              03  M80-B03.
                04  M80-B03-STATE   PIC S9(9)   BINARY.
                04  M80-B03-LABEL   PIC X(10).
              03  M80-B09.
                04  M80-B09-STATE   PIC S9(9)   BINARY.
                04  M80-B09-LABEL   PIC X(10).
              03  M80-B10.
                04  M80-B10-STATE   PIC S9(9)   BINARY.
                04  M80-B10-LABEL   PIC X(10).
              03  M80-PANDATIMER1.
                04  M80-DURATION    PIC S9(9)   BINARY.
