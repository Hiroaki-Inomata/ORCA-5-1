        02  KT01.
     *****  02  KT01-FIXED2.
              03  KT01-PTNUM.
                04  KT01-VALUE  PIC X(20).
              03  KT01-KANANAME-G.
                04  KT01-KANANAME-STYLE  PIC X(20).
                04  KT01-KANANAME  PIC X(50).
              03  KT01-NAME-G.
                04  KT01-NAME-STYLE  PIC X(20).
                04  KT01-NAME  PIC X(50).
              03  KT01-BIRTHDAY.
                04  KT01-VALUE  PIC X(9).
              03  KT01-SEX.
                04  KT01-VALUE  PIC X(2).
              03  KT01-RATE.
                04  KT01-VALUE  PIC X(6).
              03  KT01-LASTYMD.
                04  KT01-VALUE  PIC X(24).
              03  KT01-SYOSINYMD.
                04  KT01-VALUE  PIC X(24).
              03  KT01-SRYYM.
                04  KT01-VALUE  PIC X(7).
              03  KT01-GOKEITEN.
                04  KT01-VALUE  PIC X(15).
              03  KT01-SRYKBN-G     OCCURS  23.
                04  KT01-SRYKBN-STYLE  PIC X(20).
                04  KT01-SRYKBN  PIC X(2).
              03  KT01-INPUTCD-G    OCCURS  23.
                04  KT01-INPUTCD  PIC X(54).
              03  KT01-MEISYO-G     OCCURS  23.
                04  KT01-MEISYO-STYLE  PIC X(20).
                04  KT01-MEISYO      PIC X(140).
              03  KT01-SURYO-G       OCCURS 23.
                04  KT01-SURYO-STYLE  PIC X(20).
                04  KT01-SURYO  PIC X(60).
              03  KT01-NENREI.
                04  KT01-VALUE  PIC X(6).
              03  KT01-INGAI.
                04  KT01-INGAI-VALUE  PIC X.
                04  KT01-INGAI-LABEL  PIC X(10).
              03  KT01-TEISEI.
                04  KT01-TEISEI-VALUE  PIC X.
                04  KT01-TEISEI-LABEL  PIC X(10).
              03  KT01-TABOOCOB.
                04  KT01-TABOO.
                  05  KT01-VALUE  PIC X(62).
                04  KT01-TABOO-COUNT  PIC S9(9)   BINARY.
                04  KT01-TABOO-ITEM
                        PIC X(62)   OCCURS  40 TIMES.
              03  KT01-HKNCOMBICOMB.
                04  KT01-HKNCOMBI.
                  05  KT01-VALUE  PIC X(65).
                04  KT01-HKNCOMBI-COUNT  PIC S9(9)   BINARY.
                04  KT01-HKNCOMBI-ITEM
                        PIC X(65)   OCCURS  30 TIMES.
              03  KT01-MODE.
                04  KT01-VALUE  PIC X(10).
              03  KT01-SCROLLEDWINDOW2.
                04  KT01-HPOS   PIC S9(9)   BINARY.
                04  KT01-VPOS   PIC S9(9)   BINARY.
                04  KT01-BYOMEILST.
                  05  KT01-BYOMEI-COUNT  PIC S9(9)   BINARY.
                  05  KT01-BYOMEI-ROW    PIC S9(9)   BINARY.
                  05  KT01-ITEM       OCCURS  100 TIMES.
                    06  KT01-BYOMEIKA PIC X(2).
                    06  KT01-BYOMEI   PIC X(22).
                  05  KT01-BYOMEI-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  KT01-LASTMID.
                04  KT01-VALUE  PIC X(20).
              03  KT01-SRYDD.
                04  KT01-VALUE  PIC X(2).
              03  KT01-JIKANMSG.
                04  KT01-JIKANMSG-STYLE  PIC X(20).
                04  KT01-JIKANMSG-VALUE  PIC X(20).
              03  KT01-KOHMSG.
                04  KT01-KOHMSG-STYLE  PIC X(20).
                04  KT01-KOHMSG-VALUE  PIC X(50).
              03  KT01-SRYKACOMB.
                04  KT01-SRYKA.
                  05  KT01-VALUE  PIC X(16).
                04  KT01-SRYKA-COUNT  PIC S9(9)   BINARY.
                04  KT01-SRYKA-ITEM
                        PIC X(16)   OCCURS  99 TIMES.
              03  KT01-DRNAME-COMB.
                04  KT01-DRNAME.
                  05  KT01-VALUE  PIC X(29).
                04  KT01-DRNAME-COUNT  PIC S9(9)   BINARY.
                04  KT01-DRNAME-ITEM
                        PIC X(29)   OCCURS  99 TIMES.
              03  KT01-SYORIMEI.
                04  KT01-SYORIMEI-STYLE  PIC X(20).
                04  KT01-SYORIMEI-VALUE  PIC X(12).
              03  KT01-GRPMEI.
                04  KT01-GRPMEI-STYLE  PIC X(20).
                04  KT01-GRPMEI-VALUE  PIC X(10).
              03  KT01-B12.
                04  KT01-B12-STATE  PIC S9(9)   BINARY.
              03  KT01-B01S.
                04  KT01-B01S-STATE  PIC S9(9)   BINARY.
              03  KT01-B08S.
                04  KT01-B08S-STATE  PIC S9(9)   BINARY.
              03  KT01-B09S.
                04  KT01-B09S-STATE  PIC S9(9)   BINARY.
              03  KT01-B05S.
                04  KT01-B05S-STATE  PIC S9(9)   BINARY.
              03  KT01-BDAY-G   OCCURS  31.
              04  KT01-BDAY.
                05  KT01-BDAY-STATE  PIC S9(9)   BINARY.
                05  KT01-BDAY-STYLE  PIC X(20).
                05  KT01-BDAY-LABEL  PIC X(4).
              03  KT01-MODE2.
                04  KT01-MODE2-STYLE   PIC X(20).
                04  KT01-MODE2-VALUE   PIC X(10).
              03  KT01-MODE3.
                04  KT01-MODE3-STYLE   PIC X(20).
                04  KT01-MODE3-VALUE   PIC X(10).

