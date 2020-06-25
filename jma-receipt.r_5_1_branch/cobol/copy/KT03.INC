        02  KT03.
      ****  02  KT03-FIXED5.
              03  KT03-PTNUM.
                04  KT03-VALUE  PIC X(20).
              03  KT03-SRYYM.
                04  KT03-VALUE  PIC X(7).
              03  KT03-KANANAME-G.
                04  KT03-KANANAME-STYLE  PIC X(20).
                04  KT03-KANANAME        PIC X(30).
              03  KT03-MEISYO.
                04  KT03-VALUE  PIC X(70).
              03  KT03-SEX.
                04  KT03-VALUE  PIC X(2).
              03  KT03-DAY-G    OCCURS  31.
                04  KT03-DAY.
                   05  KT03-VALUE    PIC S9(2).
              03  KT03-SRYCD.
                04  KT03-VALUE  PIC X(9).
              03  KT03-NYUGAIKBN.
                04  KT03-VALUE  PIC S9(1).
              03  KT03-FSANTEIYMD.
                04  KT03-VALUE  PIC X(10).
              03  KT03-NAME-G.
                04  KT03-NAME-STYLE  PIC X(20).
                04  KT03-NAME  PIC X(30).
              03  KT03-BIRTHDAY.
                04  KT03-VALUE  PIC X(9).
              03  KT03-SELNUM.
                04  KT03-VALUE  PIC S9(3).
              03  KT03-MSANTEID.
                04  KT03-VALUE  PIC S9(2).
              03  KT03-MSANTEICNT.
                04  KT03-VALUE  PIC S9(3).
              03  KT03-NENREI.
                04  KT03-VALUE  PIC X(6).
              03  KT03-SRYKA.
                04  KT03-VALUE  PIC S9(2).
              03  KT03-HKNCOMBI.
                04  KT03-VALUE  PIC S9(4).
              03  KT03-SRYKAMEI.
                04  KT03-VALUE  PIC X(7).
              03  KT03-HKNCOMBIMEI.
                04  KT03-VALUE  PIC X(15).
              03  KT03-SCROLLEDWINDOW1.
                04  KT03-HPOS   PIC S9(9)   BINARY.
                04  KT03-VPOS   PIC S9(9)   BINARY.
                04  KT03-SANTEILST.
                  05  KT03-COUNT  PIC S9(9)   BINARY.
                  05  KT03-ROW    PIC S9(9)   BINARY.
                  05  KT03-ITEM       OCCURS  100 TIMES.
                    06  KT03-TRENNUM PIC X(4).
                    06  KT03-TSRYCD PIC X(9).
                    06  KT03-TNYUGAIKBN PIC X(2).
                    06  KT03-TSRYKA PIC X(10).
                    06  KT03-THKNCOMBI PIC X(20).
                    06  KT03-TMEISYO PIC X(60).
                    06  KT03-TFSANTEIYMD PIC X(10).
                    06  KT03-TMSANTEID PIC X(2).
                    06  KT03-TMSANTEICNT PIC X(4).
                    06  KT03-TMSANTEITEN PIC X(10).
                  05  KT03-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  KT03-B02.
                04  KT03-B02-STATE  PIC S9(9)   BINARY.
              03  KT03-B03.
                04  KT03-B03-STATE  PIC S9(9)   BINARY.
              03  KT03-B04.
                04  KT03-B04-STATE  PIC S9(9)   BINARY.
              03  KT03-B12.
                04  KT03-B12-STATE  PIC S9(9)   BINARY.
