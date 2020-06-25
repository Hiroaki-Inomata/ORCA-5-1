            02  W18.
              03  W18-YUKOSTYMD.
                04  W18-VALUE   PIC X(9).
              03  W18-YUKOEDYMD.
                04  W18-VALUE   PIC X(9).
              03  W18-B02-STATE PIC S9(9)   BINARY.
              03  W18-B03-STATE PIC S9(9)   BINARY.
              03  W18-NOTE3.
                04  W18-PAGENO  PIC S9(9)   BINARY.
                04  W18-FI23.
                  05  W18-KBNCD-COMBO.
                    06  W18-KBNCD.
                      07  W18-VALUE   PIC X(21).
                    06  W18-KBNCD-COUNT   PIC S9(9)   BINARY.
                    06  W18-KBNCD-ITEM    PIC X(21)   OCCURS  99 TIMES.
                  05  W18-HOSPNAME1.
                    06  W18-VALUE   PIC X(80).
                  05  W18-HOSPNAME2.
                    06  W18-VALUE   PIC X(80).
                  05  W18-HOSPNAME3.
                    06  W18-VALUE   PIC X(80).
                  05  W18-KAISETUNAME.
                    06  W18-VALUE   PIC X(80).
                  05  W18-POST.
                    06  W18-VALUE   PIC X(7).
                  05  W18-ADRS1.
                    06  W18-VALUE   PIC X(76).
                  05  W18-ADRS2.
                    06  W18-VALUE   PIC X(76).
                  05  W18-ADRS3.
                    06  W18-VALUE   PIC X(76).
                  05  W18-TEL.
                    06  W18-VALUE   PIC X(15).
                  05  W18-FAX.
                    06  W18-VALUE   PIC X(15).
                04  W18-FI24.
                  05  W18-PRTKBN-COMBO  OCCURS  25.
                    06  W18-PRTKBN.
                      07  W18-VALUE   PIC X(21).
                    06  W18-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W18-PRTKBN-ITEM
                            PIC X(21)   OCCURS  99 TIMES.
              03  W18-HHOSPNAME.
                04  W18-VALUE   PIC X(60).
              03  W18-HKAISETUNAME.
                04  W18-VALUE   PIC X(60).
              03  W18-HADRS.
                04  W18-VALUE   PIC X(100).
              03  W18-HPOST.
                04  W18-VALUE   PIC X(7).
              03  W18-HTEL.
                04  W18-VALUE   PIC X(15).
              03  W18-HFAX.
                04  W18-VALUE   PIC X(15).
