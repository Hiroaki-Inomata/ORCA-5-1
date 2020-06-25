        02  W061.
      *     03  W061-TITLE  PIC X(256).
      *     03  W061-POPUP-SUMMARY  PIC X(256).
      *     03  W061-POPUP-BODY PIC X(256).
      *     03  W061-POPUP-ICON PIC X(256).
      *     03  W061-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ******02  W061-FIXED32.
              03  W061-B01.
                04  W061-B01-STATE  PIC S9(9)   BINARY.
              03  W061-B02.
                04  W061-B02-STATE  PIC S9(9)   BINARY.
              03  W061-B04.
                04  W061-B04-STATE  PIC S9(9)   BINARY.
              03  W061-B06.
                04  W061-B06-STATE  PIC S9(9)   BINARY.
              03  W061-B07.
                04  W061-B07-STATE  PIC S9(9)   BINARY.
              03  W061-B09.
                04  W061-B09-STATE  PIC S9(9)   BINARY.
              03  W061-B10.
                04  W061-B10-STATE  PIC S9(9)   BINARY.
              03  W061-B12.
                04  W061-B12-STATE  PIC S9(9)   BINARY.
                04  W061-B12-LABEL  PIC X(10).
              03  W061-SELNUM.
                04  W061-TEXTDATA   PIC X(4).
              03  W061-MIDASI1.
                04  W061-TEXTDATA   PIC X(100).
              03  W061-SCROLLEDWINDOW1.
                04  W061-DATALIST.
                  05  W061-DATALIST-COUNT  PIC S9(9)   BINARY.
                  05  W061-DATALIST-ROW    PIC S9(9)   BINARY.
                  05  W061-DATALIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W061-DATALIST-SELECTDATA
                          PIC X       OCCURS  100 TIMES.
                  05  W061-DATALIST-FGCOLOR
                          PIC X(20)   OCCURS  100 TIMES.
                  05  W061-DATALIST-BGCOLOR
                          PIC X(20)   OCCURS  100 TIMES.
                  05  W061-ITEM       OCCURS  100 TIMES.
                    06  W061-TNUM        PIC X(4).
                    06  W061-SYOKUCD     PIC X(4).
                    06  W061-NAME        PIC X(80).
                    06  W061-USERID      PIC X(16).
                    06  W061-STYUKYMD    PIC X(12).
                    06  W061-EDYUKYMD    PIC X(12).
                    06  W061-HIHHYOJI    PIC X(2).
                    06  W061-BIKOU       PIC X(12).
              03  W061-MIDASI2-G.
                04  W061-MIDASI2-STYLE  PIC X(20).
                04  W061-MIDASI2   PIC X(50).
              03  W061-EDYUYMD.
                04  W061-TEXTDATA   PIC X(12).
              03  W061-MIDASI3.
                04  W061-TEXTDATA   PIC X(30).
              03  W061-MIDASI4.
                04  W061-TEXTDATA   PIC X(100).
              03  W061-MIDASI5-G.
                04  W061-MIDASI5-STYLE  PIC X(20).
                04  W061-MIDASI5   PIC X(30).
