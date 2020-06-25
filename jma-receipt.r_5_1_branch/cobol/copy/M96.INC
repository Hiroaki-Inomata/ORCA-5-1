        02  M96.
    ******* 02  M96-FIXED3.
              03  M96-B02.
                04  M96-B02-VISIBLE     PIC X(01).
              03  M96-B05.
                04  M96-B05-LABEL       PIC X(10).
              03  M96-B10.
                04  M96-B10-VISIBLE     PIC X(01).
                04  M96-B10-STATE       PIC S9(9)   BINARY.
              03  M96-B11.
                04  M96-B11-VISIBLE     PIC X(01).
              03  M96-B12.
                04  M96-B12-VISIBLE     PIC X(01).
              03  M96-LIST1.
                04  M96-LIST1-COUNT   PIC S9(9)   BINARY.
                04  M96-LIST1-ROW PIC S9(9)   BINARY.
                04  M96-LIST1-ROWATTR PIC S9(9)   BINARY.
                04  M96-ITEM        OCCURS  1 TIMES.
                  05  M96-VERSION      PIC X(64).
                  05  M96-DBSVERSION1  PIC X(64).
                  05  M96-DBSVERSION2  PIC X(64).
                  05  M96-CREYMD       PIC X(9).
                04  M96-SELECT
                      PIC X       OCCURS  1 TIMES.
              03  M96-LIST2.
                04  M96-LIST2-COUNT   PIC S9(9)   BINARY.
                04  M96-LIST2-ROW PIC S9(9)   BINARY.
                04  M96-LIST2-ROWATTR PIC S9(9)   BINARY.
                04  M96-ITEM        OCCURS  2 TIMES.
                  05  M96-JOBNAME    PIC X(40).
                  05  M96-STARTYMD   PIC X(9).
                  05  M96-STARTTIME  PIC X(8).
                  05  M96-ENDYMD   PIC X(9).
                  05  M96-ENDTIME  PIC X(8).
                  05  M96-LASTYMD  PIC X(9).
                04  M96-SELECT
                      PIC X       OCCURS  2 TIMES.
              03  M96-SCROLLEDWINDOW1.
                04  M96-LIST3.
                  05  M96-LIST3-COUNT   PIC S9(9)   BINARY.
                  05  M96-LIST3-ROW PIC S9(9)   BINARY.
                  05  M96-LIST3-ROWATTR PIC S9(9)   BINARY.
                  05  M96-ITEM        OCCURS  99 TIMES.
                    06  M96-MASTNAME  PIC X(30).
                    06  M96-DBRVERSION1  PIC X(64).
                    06  M96-DBRVERSION2  PIC X(64).
                  05  M96-SELECT
                          PIC X       OCCURS  99 TIMES.
            03  M96-MSG1.
              04  M96-MSG1-STYLE PIC X(20).
              04  M96-MSG1-VALUE     PIC X(70).
            03  M96-MSG1PLUS.
              04  M96-MSG1PLUS-STYLE PIC X(20).
              04  M96-MSG1PLUS-VALUE     PIC X(70).
            03  M96-MSG2.
              04  M96-MSG2-STYLE PIC X(20).
              04  M96-MSG2-VALUE     PIC X(70).
            03  M96-MSG2PLUS.
              04  M96-MSG2PLUS-STYLE PIC X(20).
              04  M96-MSG2PLUS-VALUE     PIC X(70).
           03 M96-DURATION   PIC S9(09) BINARY.
            03  M96-CONTRACTMSG-G.
              04  M96-CONTRACTMSG-STYLE PIC X(20).
              04  M96-CONTRACTMSG       PIC X(10).
