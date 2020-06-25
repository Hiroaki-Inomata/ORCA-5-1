           02  R01.
              03  R01-B02-STATE   PIC S9(09) BINARY.
              03  R01-B04-STATE   PIC S9(09) BINARY.
              03  R01-B05-STATE   PIC S9(09) BINARY.
              03  R01-B05-VALUE   PIC X(8).
              03  R01-B08-STATE   PIC S9(09) BINARY.
              03  R01-B09-STATE   PIC S9(09) BINARY.
              03  R01-B22-STATE   PIC S9(09) BINARY.
              03  R01-SYORI-COMBO.
                04  R01-SYORI-STATE   PIC S9(09) BINARY.
                04  R01-SYORI.
                  05  R01-VALUE   PIC X(12).
                04  R01-SYORI-COUNT   PIC S9(9)   BINARY.
                04  R01-SYORILST
                        PIC X(12)   OCCURS  5 TIMES.
              03  R01-TEISEIYM-STATE   PIC S9(09) BINARY.
              03  R01-TEISEIYM.
                04  R01-VALUE   PIC X(7).
              03  R01-STJYUSINYMD-STATE   PIC S9(09) BINARY.
              03  R01-STJYUSINYMD.
                04  R01-VALUE   PIC X(10).
              03  R01-EDJYUSINYMD-STATE   PIC S9(09) BINARY.
              03  R01-EDJYUSINYMD.
                04  R01-VALUE   PIC X(10).
              03  R01-RENNUM-STATE   PIC S9(09) BINARY.
              03  R01-RENNUM.
                04  R01-VALUE   PIC S9(4).
              03  R01-SRYYM-STATE   PIC S9(09) BINARY.
              03  R01-SRYYM.
                04  R01-VALUE   PIC X(7).
              03  R01-PTNUM-STATE   PIC S9(09) BINARY.
              03  R01-PTNUM.
                04  R01-VALUE   PIC X(20).
              03  R01-NYUGAIKBN-STATE   PIC S9(09) BINARY.
              03  R01-NYUGAIKBN.
                04  R01-VALUE   PIC X(1).
              03  R01-NAME.
                04  R01-VALUE   PIC X(50).
              03  R01-SCROLLEDWINDOW1.
                04  R01-RECELST.
                  05  R01-COUNT   PIC S9(9)   BINARY.
                  05  R01-ROW PIC S9(9)   BINARY.
                  05  R01-ROWATTR PIC S9(9)   BINARY.
                  05  R01-ITEM        OCCURS  2000 TIMES.
                    06  R01-TRENNUM  PIC X(4).
                    06  R01-TTSTPTNUMKBN  PIC X(2).
                    06  R01-TPTNUM   PIC X(20).
                    06  R01-TNAME    PIC X(50).
                    06  R01-TSRYYM   PIC X(7).
                    06  R01-TNYUGAIKBN PIC X(1).
                    06  R01-TTAISYO  PIC X(2).
                    06  R01-TIHO     PIC X(4).
                    06  R01-TRSI     PIC X(4).
                    06  R01-TJBI     PIC X(4).
                    06  R01-TKOU     PIC X(4).
                    06  R01-TDAISAN  PIC X(4).
                  05  R01-SELECT  PIC X       OCCURS  2000 TIMES.
              03  R01-SYOKBN-STYLE   PIC X(20).
              03  R01-SYOKBN         PIC X(68).
              03  R01-LABEL.
                04  R01-VALUE   PIC X(10).
              03  R01-LABEL1.
                04  R01-VALUE   PIC X(12).
              03  R01-LABEL2.
                04  R01-VALUE   PIC X(14).
              03  R01-CREYMD.
                04  R01-VALUE   PIC X(22).
              03  R01-HYODAI-STYLE   PIC X(20).
              03  R01-HYODAI.
                04  R01-VALUE   PIC X(30).
              03  R01-JYOKEN1-STYLE   PIC X(20).
              03  R01-JYOKEN1.
                04  R01-VALUE   PIC X(50).
              03  R01-SRYYMH-STYLE   PIC X(20).
              03  R01-SRYYMH.
                04  R01-VALUE   PIC X(22).
              03  R01-NYUGAIKBNH-STYLE   PIC X(20).
              03  R01-NYUGAIKBNH.
                04  R01-VALUE   PIC X(12).
              03  R01-CREYMDH-STYLE   PIC X(20).
              03  R01-CREYMDH.
                04  R01-VALUE   PIC X(13).
              03  R01-KENSU.
                04  R01-VALUE   PIC X(9).
