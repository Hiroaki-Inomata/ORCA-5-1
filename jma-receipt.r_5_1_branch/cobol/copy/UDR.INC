        02  UDR.
            03  UDR-TITLE   PIC X(256).
            03  UDR-POPUP-SUMMARY   PIC X(256).
            03  UDR-POPUP-BODY  PIC X(256).
            03  UDR-POPUP-ICON  PIC X(256).
            03  UDR-POPUP-TIMEOUT   PIC S9(9)   BINARY.
      ******02  UDR-FIXED1.
              03  UDR-B01.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B02.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B03.
                04  UDR-B03-STATE   PIC S9(9)   BINARY.
                04  UDR-B03-LABEL   PIC X(10).
              03  UDR-B04.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B05.
                04  UDR-B05-STATE   PIC S9(9)   BINARY.
                04  UDR-B05-LABEL   PIC X(10).
              03  UDR-B06.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B07.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B08.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B09.
                04  UDR-B09-STATE   PIC S9(9)   BINARY.
                04  UDR-B09-LABEL   PIC X(10).
              03  UDR-B10.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-B11.
                04  UDR-B11-STATE   PIC S9(9)   BINARY.
                04  UDR-B11-LABEL   PIC X(10).
              03  UDR-B12.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-SRYKA-COMB.
                04  UDR-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  UDR-SRYKA-ITEM
                        PIC X(21)   OCCURS  99 TIMES.
                04  UDR-SRYKA.
                  05  UDR-TEXTDATA    PIC X(21).
              03  UDR-SCROLLEDWINDOW1.
                04  UDR-DRLIST.
                  05  UDR-DRLIST-COUNT   PIC S9(9)   BINARY.
                  05  UDR-DRLIST-ROW PIC S9(9)   BINARY.
                  05  UDR-DRLIST-SELECTDATA
                          PIC X       OCCURS  200 TIMES.
                  05  UDR-DRLIST-FGCOLOR
                          PIC X(20)  OCCURS  200 TIMES.
                  05  UDR-DRLIST-BGCOLOR
                          PIC X(20)  OCCURS  200 TIMES.
                  05  UDR-ITEM        OCCURS  200 TIMES.
                    06  UDR-TNO     PIC X(3).
                    06  UDR-THNUM   PIC X(3).
                    06  UDR-TDRCD   PIC X(4).
                    06  UDR-TDRCDNAME   PIC X(80).
                    06  UDR-TSRYKA1     PIC X(20).
                    06  UDR-TSRYKA2     PIC X(20).
                    06  UDR-TSRYKA3     PIC X(20).
                    06  UDR-TSRYKA4     PIC X(20).
                    06  UDR-TSRYKA5     PIC X(20).
                    06  UDR-THYOUJI     PIC X(10).
              03  UDR-MIDASI.
                04  UDR-MIDASI-STYLE   PIC X(20).
                04  UDR-MIDASI-DATA    PIC X(30).
              03  UDR-MIDASI2.
                04  UDR-MIDASI2-STYLE   PIC X(20).
                04  UDR-MIDASI2-DATA    PIC X(50).
              03  UDR-MIDASI3.
                04  UDR-MIDASI3-STYLE   PIC X(20).
                04  UDR-MIDASI3-DATA    PIC X(100).
              03  UDR-NEXT.
                04  UDR-NEXT-STYLE   PIC X(20).
                04  UDR-NEXT-DATA    PIC X(20).
              03  UDR-BUP.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-BDW.
                04  UDR-STATE   PIC S9(9)   BINARY.
              03  UDR-SELNUM.
                04  UDR-NUMDATA PIC S9(3).
