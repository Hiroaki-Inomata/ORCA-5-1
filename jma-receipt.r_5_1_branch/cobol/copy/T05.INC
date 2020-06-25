          02  T05.
              03  T05-SELNUM.
                04  T05-VALUE   PIC S9(2).
              03  T05-FILEENTRY1.
                04  T05-OBJECTDATA  PIC X(36).
                04  T05-ENTRY1      PIC X(200).
              03  T05-ERRMSG1.
                04  T05-ERRMSG1-STYLE   PIC X(20).
                04  T05-ERRMSG1-VALUE   PIC X(120).
              03  T05-ERRMSG2.
                04  T05-ERRMSG2-STYLE   PIC X(20).
                04  T05-ERRMSG2-VALUE   PIC X(100).
              03  T05-SCROLLEDWINDOW1.
                04  T05-HPOS    PIC S9(9)   BINARY.
                04  T05-VPOS    PIC S9(9)   BINARY.
                04  T05-DATALST.
                  05  T05-DATALST-COUNT   PIC S9(9)   BINARY.
                  05  T05-DATALST-ROW     PIC S9(9)   BINARY.
                  05  T05-DATALST-ITEM        OCCURS  50 TIMES.
                    06  T05-DATALST-TRENNUM PIC X(2).
                    06  T05-DATALST-TFILENM PIC X(35).
                    06  T05-DATALST-TSRYYM  PIC X(6).
                    06  T05-DATALST-TSENDYM PIC X(9).
                    06  T05-DATALST-TUKAISU PIC X(3).
                    06  T05-DATALST-TSKAISU PIC X(3).
                    06  T05-DATALST-TDATA   PIC X(100).
                    06  T05-DATALST-TFLG3   PIC X(2).
                    06  T05-DATALST-TFLG2   PIC X(2).
                    06  T05-DATALST-TFLG4   PIC X(2).
                    06  T05-DATALST-TCNT    PIC X(5).
                    06  T05-DATALST-THOSPCD PIC X(2).
                  05  T05-DATALST-SELECT
                          PIC X       OCCURS  50 TIMES.
                  05  T05-DATALST-BGCOLOR
                          PIC X(16)   OCCURS  50 TIMES.
              03  T05-SELNAME1.
                04  T05-VALUE   PIC X(100).
              03  T05-SELNAME2.
                04  T05-VALUE   PIC X(10).
              03  T05-ERRDETAIL.
                04  T05-ERRDETAIL-STYLE   PIC X(20).
                04  T05-ERRDETAIL-VALUE   PIC X(100).
