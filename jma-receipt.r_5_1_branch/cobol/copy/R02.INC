        02  R02.
              04  R02-B13-STATE   PIC S9(09) BINARY.
              04  R02-B14-STATE   PIC S9(09) BINARY.
              04  R02-SYOKBN.
                05  R02-SYOKBN-VALUE   PIC X.
                05  R02-SYOKBN-LABEL   PIC X(9).
              04  R02-SCROLL.
                05  R02-SCR-HPOS      PIC S9(9)   BINARY.
                05  R02-SCR-VPOS      PIC S9(9)   BINARY.
              04  R02-TSTDATA-G    OCCURS  36.
                05  R02-TSTDATA-STATE   PIC S9(09) BINARY.
                05  R02-TSTDATA.
                  06  R02-VALUE   PIC X(100).
              04  R02-TEDDATA-G    OCCURS  36.
                05  R02-TEDDATA-STATE   PIC S9(09) BINARY.
                05  R02-TEDDATA.
                  06  R02-VALUE   PIC X(100).
              04  R02-TPRTNM-COMBO-G    OCCURS  36.
                05  R02-TPRTNM-COMBO.
                  06  R02-TPRTNM-STATE   PIC S9(09) BINARY.
                  06  R02-TPRTNM.
                    07  R02-VALUE   PIC X(19).
                  06  R02-TPRTNM-COUNT   PIC S9(9)   BINARY.
                  06  R02-TPRTNMLST  PIC X(19)   OCCURS  100 TIMES.
              04  R02-TPRTNM-N-COMBO-G    OCCURS  36.
                05  R02-TPRTNM-N-COMBO.
                  06  R02-TPRTNM-N-STATE   PIC S9(09) BINARY.
                  06  R02-TPRTNM-N.
                    07  R02-VALUE   PIC X(19).
                  06  R02-TPRTNM-N-COUNT   PIC S9(9)   BINARY.
                  06  R02-TPRTNM-NLST  PIC X(19)   OCCURS  100 TIMES.
              04  R02-TPAGE-G      OCCURS  36.
                05  R02-TPAGE-STYLE   PIC X(20).
                05  R02-TPAGE.
                  06  R02-VALUE   PIC 9(4).
              04  R02-TKENSU-G      OCCURS  36.
                05  R02-TKENSU-STYLE   PIC X(20).
                05  R02-TKENSU.
                  06  R02-VALUE   PIC 9(4).
              04  R02-TCHK-G       OCCURS  36.
                05  R02-TCHK.
                  06  R02-TCHK-STATE   PIC S9(09) BINARY.
                  06  R02-TCHK-STYLE   PIC X(20).
                  06  R02-TCHK-VALUE   PIC X.
                  06  R02-TCHK-LABEL   PIC X(40).
              04  R02-TPRTFLG-G      OCCURS  36.
                05  R02-TPRTFLG-STYLE   PIC X(20).
                05  R02-TPRTFLG         PIC X(2).
              04  R02-FILEKBN.
                05  R02-VALUE     PIC X(82).
              04  R02-MSG-STYLE   PIC X(20).
              04  R02-MSG         PIC X(42).
              04  R02-CREYMD.
                05  R02-VALUE     PIC X(22).
              04  R02-KENSU.
                05  R02-VALUE   PIC 9(4).
              04  R02-PAGE.
                05  R02-VALUE   PIC 9(4).
              04  R02-HKNJANAME-STYLE   PIC X(20).
              04  R02-HKNJANAME         PIC X(50).
              04  R02-PRTJYUN-COMBO.
                05  R02-PRTJYUN.
                  06  R02-VALUE   PIC X(33).
                05  R02-PRTJYUN-COUNT   PIC S9(9)   BINARY.
                05  R02-PRTJYUNLST  PIC X(33)   OCCURS  15 TIMES.
              04  R02-PRTJYUN1-COMBO.
                05  R02-PRTJYUN1-STATE   PIC S9(09) BINARY.
                05  R02-PRTJYUN1.
                  06  R02-VALUE   PIC X(33).
                05  R02-PRTJYUN1-COUNT   PIC S9(9)   BINARY.
                05  R02-PRTJYUN1LST  PIC X(33)   OCCURS  15 TIMES.
              04  R02-PRTJYUN2-COMBO.
                05  R02-PRTJYUN2-STATE   PIC S9(09) BINARY.
                05  R02-PRTJYUN2.
                  06  R02-VALUE   PIC X(33).
                05  R02-PRTJYUN2-COUNT   PIC S9(9)   BINARY.
                05  R02-PRTJYUN2LST  PIC X(33)   OCCURS  15 TIMES.
              04  R02-PREFKBN-COMBO.
                05  R02-PREFKBN-STATE   PIC S9(09) BINARY.
                05  R02-PREFKBN.
                  06  R02-VALUE   PIC X(20).
                05  R02-PREFKBN-COUNT   PIC S9(9)   BINARY.
                05  R02-PREFKBNLST  PIC X(20)   OCCURS  4 TIMES.
              04  R02-PRTKBN-COMBO.
                05  R02-PRTKBN.
                  06  R02-VALUE   PIC X(14).
                05  R02-PRTKBN-COUNT   PIC S9(9)   BINARY.
                05  R02-PRTKBNLST  PIC X(14)   OCCURS  3 TIMES.
              04  R02-SYUKA-MIDASI PIC X(12).
              04  R02-SYUKA-COMBO.
                05  R02-SYUKA-STATE   PIC S9(09) BINARY.
                05  R02-SYUKA.
                  06  R02-VALUE   PIC X(21).
                05  R02-SYUKA-COUNT   PIC S9(9)   BINARY.
                05  R02-SYUKALST  PIC X(21)   OCCURS  100 TIMES.
              04  R02-LABEL1       PIC X(06).
              04  R02-LABEL2       PIC X(06).
              04  R02-LABEL3       PIC X(06).
