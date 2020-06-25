        02  J02.
     *******02  J02-FIXED1.
              03  J02-PTNUM.
                04  J02-VALUE   PIC X(20).
              03  J02-SRYYM.
                04  J02-VALUE   PIC X(6).
              03  J02-KANANAME-G.
                04  J02-KANANAME-STYLE   PIC X(20).
                04  J02-KANANAME   PIC X(50).
              03  J02-NAME-G.
                04  J02-NAME-STYLE   PIC X(20).
                04  J02-NAME   PIC X(50).
              03  J02-BIRTHDAY.
                04  J02-VALUE   PIC X(9).
              03  J02-MEISYO.
                04  J02-VALUE   PIC X(50).
              03  J02-NUM.
                04  J02-VALUE   PIC X(3).
              03  J02-HKNCOMBI.
                04  J02-VALUE   PIC X(4).
              03  J02-HKNCOMBIMEI.
                04  J02-VALUE   PIC X(60).
              03  J02-SEX.
                04  J02-VALUE   PIC X(2).
              03  J02-DAY       OCCURS 31.
                04  J02-VALUE   PIC X(3).
              03  J02-RRKSELNUM.
                04  J02-VALUE   PIC X(3).
              03  J02-HENSRYYMD.
                04  J02-VALUE   PIC X(2).
              03  J02-SCROLLEDWINDOW1.
                04  J02-DATALIST.
                  05  J02-DATALIST-COUNT   PIC S9(9)   BINARY.
                  05  J02-DATALIST-ROW   PIC S9(9)   BINARY.
                  05  J02-DATALIST-ROWATTR   PIC S9(9)   BINARY.
                  05  J02-DATALIST-ITEM        OCCURS  400 TIMES.
                    06  J02-TNO          PIC X(3).
                    06  J02-TMEISYO      PIC X(80).
                    06  J02-TSURYO       PIC X(13).
                    06  J02-TTENSU       PIC X(7).
                    06  J02-THKNCOMBI    PIC X(4).
                    06  J02-TDAYGOK      PIC X(3).
                    06  J02-TDAY-G.
                        08  J02-TDAY    PIC X(3)  OCCURS  31.
                  05  J02-DATALIST-SELECT  PIC X   OCCURS  400 TIMES.
              03  J02-SCROLLEDWINDOW2.
                04  J02-RRKLIST.
                  05  J02-RRKLIST-COUNT   PIC S9(9)   BINARY.
                  05  J02-RRKLIST-ROW   PIC S9(9)   BINARY.
                  05  J02-RRKLIST-ROWATTR   PIC S9(9)   BINARY.
                  05  J02-ITEM        OCCURS  100 TIMES.
                    06  J02-RRKNO     PIC X(3).
                    06  J02-RRKYMD    PIC X(20).
                    06  J02-RRKSRYKA  PIC X(2).
                    06  J02-RRKHKNCOMBI   PIC X(4).
                  05  J02-RRKLIST-SELECT
                         PIC X       OCCURS  100 TIMES.
              03  J02-SRYKAMEI.
                04  J02-VALUE   PIC X(4).
              03  J02-NYUGAIKBNCOMB.
                04  J02-NYUGAIKBN.
                  05  J02-VALUE   PIC X(10).
                04  J02-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  J02-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
              03  J02-SRYKACOMB.
                04  J02-SRYKA.
                  05  J02-VALUE   PIC X(16).
                04  J02-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  J02-SRYKA-ITEM
                        PIC X(16)   OCCURS  100 TIMES.
              03  J02-SRYKBNCOMB.
                04  J02-SRYKBN.
                  05  J02-VALUE   PIC X(19).
                04  J02-SRYKBN-COUNT   PIC S9(9)   BINARY.
                04  J02-SRYKBN-ITEM
                        PIC X(19)    OCCURS  30 TIMES.
              03  J02-NYUINKIKAN.
                04  J02-VALUE   PIC X(36).
              03  J02-HHKNCOMBICOMB.
                04  J02-HHKNCOMBI.
                  05  J02-VALUE   PIC X(65).
                04  J02-HHKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  J02-HHKNCOMBI-ITEM
                        PIC X(65)   OCCURS  40 TIMES.
              03  J02-IKKATU.
                04  J02-VALUE   PIC X(99).
              03  J02-NYUINMSG1.
                04  J02-NYUINMSG1-STYLE   PIC X(20).
                04  J02-NYUINMSG1-VALUE   PIC X(50).
              03  J02-NYUINMSG2.
                04  J02-NYUINMSG2-STYLE   PIC X(20).
                04  J02-NYUINMSG2-VALUE   PIC X(50).
              03  J02-HKNCOMBMSG.
                04  J02-HKNCOMBMSG-STYLE   PIC X(20).
                04  J02-HKNCOMBMSG-VALUE   PIC X(36).
              03  J02-HKTMSG.
                04  J02-HKTMSG-STYLE   PIC X(20).
                04  J02-HKTMSG-VALUE   PIC X(100).
              03  J02-SYUMSG.
                04  J02-SYUMSG-STYLE   PIC X(20).
                04  J02-SYUMSG-VALUE   PIC X(20).
              03  J02-B04.
                04  J02-B04-STATE   PIC S9(9)   BINARY.
              03  J02-B05.
                04  J02-B05-STATE   PIC S9(9)   BINARY.
              03  J02-B08.
                04  J02-B08-STATE   PIC S9(9)   BINARY.
              03  J02-B12.
                04  J02-B12-STATE   PIC S9(9)   BINARY.
              03  J02-B02S.
                04  J02-B02S-STATE   PIC S9(9)   BINARY.
              03  J02-B04S.
                04  J02-B04S-STATE   PIC S9(9)   BINARY.
              03  J02-B09S.
                04  J02-B09S-STATE   PIC S9(9)   BINARY.
              03  J02-B10S.
                04  J02-B10S-STATE   PIC S9(9)   BINARY.
              03  J02-B11S.
                04  J02-B11S-STATE   PIC S9(9)   BINARY.
              03  J02-B01S.
                04  J02-B01S-STATE   PIC S9(9)   BINARY.

