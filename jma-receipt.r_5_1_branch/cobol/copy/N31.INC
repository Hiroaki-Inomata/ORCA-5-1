        02  N31.
      ****  02  N31-FIXED6.
              03  N31-B06.
                04  N31-B06-STATE   PIC S9(9)   BINARY.
              03  N31-B07.
                04  N31-B07-STATE   PIC S9(9)   BINARY.
              03  N31-POST.
                04  N31-VALUE   PIC X(7).
              03  N31-SCROLLEDWINDOW1.
                04  N31-CYOIKILST.
                  05  N31-LABEL554.
                    06  N31-VALUE   PIC X(2).
                  05  N31-CYOIKI-COUNT   PIC S9(9)   BINARY.
                  05  N31-CYOIKI-ROW PIC S9(9)   BINARY.
                  05  N31-ITEM        OCCURS  200 TIMES.
                    06  N31-CYO-HEN PIC X(2).
                    06  N31-CYO-CD  PIC X(4).
                    06  N31-CYO-POST PIC X(7).
                    06  N31-CYO-ADRS  PIC X(100).
                    06  N31-CYO-TOWNNAME2  PIC X(600).
                  05  N31-CYOIKI-SELECT
                          PIC X       OCCURS  200 TIMES.
                  05  N31-CYOIKI-FGCOLOR
                          PIC X(16)   OCCURS  200 TIMES.
                  05  N31-CYOIKI-BGCOLOR
                          PIC X(16)   OCCURS  200 TIMES.
              03  N31-SELNUM.
                04  N31-VALUE   PIC S9(3).
              03  N31-PREFCDCOMBO.
                04  N31-PREFCD.
                  05  N31-VALUE   PIC X(13).
                04  N31-PREFCD-COUNT   PIC S9(9)   BINARY.
                04  N31-PREFCD-ITEM
                        PIC X(13)   OCCURS  47 TIMES.
              03  N31-PREFNAME.
                04  N31-VALUE   PIC X(10).
              03  N31-PREFKANA.
                04  N31-VALUE   PIC X(20).
              03  N31-CITYKANA.
                04  N31-VALUE   PIC X(100).
              03  N31-TOWNKANA.
                04  N31-VALUE   PIC X(100).
              03  N31-CITYNAME.
                04  N31-VALUE   PIC X(50).
              03  N31-TOWNNAME.
                04  N31-VALUE   PIC X(50).
              03  N31-TOWNNAME2.
                04  N31-VALUE   PIC X(600).
              03  N31-NEXT.
                04  N31-NEXT-STYLE   PIC X(20).
                04  N31-NEXT-VALUE   PIC X(20).
