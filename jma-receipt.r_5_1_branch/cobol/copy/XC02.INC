              02  XC02.
                04  XC02-FIXED32.
                  05 XC02-B05-STATE   PIC S9(09) BINARY.
                  05  XC02-SELNUM.
                    06  XC02-VALUE  PIC S9(5).
                  05  XC02-ERRMSG-STYLE     PIC X(20).
                  05  XC02-ERRMSG           PIC X(60).
                  05  XC02-TITLE-STYLE      PIC X(20).
                  05  XC02-TITLE            PIC X(40).
                  05  XC02-PSAREA.
                    06  XC02-VALUE  PIC X(36).
                  05  XC02-SCROLLEDWINDOW26.
                    06  XC02-CLIST.
                      07  XC02-COUNT        PIC S9(9)   BINARY.
                      07  XC02-ROW          PIC S9(9)   BINARY.
                      07  XC02-ROWATTR      PIC S9(9)   BINARY.
                      07  XC02-ITEM       OCCURS  100 TIMES.
                        08  XC02-TNO    PIC X(5).
                        08  XC02-TPRT-TITLE PIC X(10).
                        08  XC02-PTNUM PIC X(20).
                        08  XC02-TSRYYM PIC X(10).
                        08  XC02-OKFLG  PIC X(01).
                        08  XC02-UPDFLG  PIC X(02).
                        08  XC02-PRTID   PIC X(20).
                      07  XC02-SELECT PIC X       OCCURS  100 TIMES.
                  05  XC02-MEMO-STATE   PIC S9(09) BINARY.
                  05  XC02-MEMO          PIC X(100).
                  05 XC02-DURATION   PIC S9(09) BINARY.
