              02  XC01.
                04  XC01-FIXED32.
                  05 XC01-B05-STATE   PIC S9(09) BINARY.
                  05  XC01-SELNUM.
                    06  XC01-VALUE  PIC S9(5).
                  05  XC01-ERRMSG-STYLE     PIC X(20).
                  05  XC01-ERRMSG           PIC X(60).
                  05  XC01-TITLE-STYLE      PIC X(20).
                  05  XC01-TITLE            PIC X(40).
                  05  XC01-PSAREA.
                    06  XC01-VALUE  PIC X(36).
                  05  XC01-SCROLLEDWINDOW26.
                    06  XC01-CLIST.
                      07  XC01-COUNT        PIC S9(9)   BINARY.
                      07  XC01-ROW          PIC S9(9)   BINARY.
                      07  XC01-ROWATTR      PIC S9(9)   BINARY.
                      07  XC01-ITEM       OCCURS  100 TIMES.
                        08  XC01-TNO    PIC X(5).
                        08  XC01-TPRT-TITLE PIC X(10).
                        08  XC01-PTNUM PIC X(20).
                        08  XC01-TSRYYM PIC X(10).
                        08  XC01-OKFLG  PIC X(01).
                        08  XC01-UPDFLG  PIC X(02).
                        08  XC01-PRTID   PIC X(20).
                      07  XC01-SELECT PIC X       OCCURS  100 TIMES.
                  05  XC01-MEMO-STATE   PIC S9(09) BINARY.
                  05  XC01-MEMO          PIC X(100).
                  05 XC01-DURATION   PIC S9(09) BINARY.
