        02  I42.
     ****** 02  I42-FIXED19.
              03  I42-PTNUM.
                04  I42-VALUE   PIC X(20).
              03  I42-KANANAME.
                04  I42-VALUE   PIC X(30).
              03  I42-NAME.
                04  I42-VALUE   PIC X(30).
              03  I42-BIRTHDAY.
                04  I42-VALUE   PIC X(9).
              03  I42-SEX.
                04  I42-VALUE   PIC X(2).
              03  I42-SCROLLEDWINDOW2.
                04  I42-SRYACTCNT.
                  05  I42-SRYACTCNT-COUNT   PIC S9(9)   BINARY.
                  05  I42-SRYACTCNT-ROW PIC S9(9)   BINARY.
                  05  I42-SRYACTCNT-ROWATTR PIC S9(9)   BINARY.
                  05  I42-SRYACTCNT-ITEM        OCCURS  100 TIMES.
                    06  I42-SRYACTCNT-YM    PIC X(7).
                    06  I42-SRYACTCNT-NAME  PIC X(40).
                    06  I42-SRYACTCNT-CNT    PIC X(5).
                  05  I42-SRYACTCNT-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  I42-SCROLLEDWINDOW1.
                04  I42-SRYACTYMD.
                  05  I42-SRYACTYMD-COUNT   PIC S9(9)   BINARY.
                  05  I42-SRYACTYMD-ROW       PIC S9(9)   BINARY.
                  05  I42-SRYACTYMD-ROWATTR   PIC S9(9)   BINARY.
                  05  I42-SRYACTYMD-ITEM        OCCURS  100 TIMES.
                    06  I42-SRYACTYMD-YMD  PIC X(10).
                    06  I42-SRYACTYMD-NAME  PIC X(40).
                  05  I42-SRYACTYMD-SELECT  PIC X   OCCURS  100 TIMES.
              03  I42-TENSU.
                04  I42-VALUE   PIC X(10).
              03  I42-RSITENSU.
                04  I42-VALUE   PIC X(10).
              03  I42-JIHIKIN.
                04  I42-VALUE   PIC X(10).
              03  I42-SRYYM.
                04  I42-VALUE   PIC X(6).
              03  I42-NYUGAIKBN-COMB.
                04  I42-NYUGAIKBN.
                  05  I42-VALUE   PIC X(10).
                04  I42-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  I42-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
              03  I42-SRYKA-COMB.
                04  I42-SRYKA.
                  05  I42-VALUE   PIC X(20).
                04  I42-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  I42-SRYKA-ITEM
                        PIC X(20)   OCCURS  41 TIMES.
              03  I42-HHKNCOMBI-COMB.
                04  I42-HHKNCOMBI.
                  05  I42-VALUE   PIC X(55).
                04  I42-HHKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  I42-HHKNCOMBI-ITEM
                        PIC X(55)   OCCURS  20 TIMES.
              03  I42-NYUTENSU.
                04  I42-VALUE   PIC X(10).
              03  I42-MIDASI.
                04  I42-MIDASI-STYLE   PIC X(20).
                04  I42-MIDASI-VALUE   PIC X(20).
              03  I42-MIDASIN.
                04  I42-VALUE   PIC X(10).
