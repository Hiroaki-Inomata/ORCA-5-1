        02  Z08.
     *****  02  Z08-FIXED1.
              03  Z08-SRYCDH.
                04  Z08-VALUE   PIC X(9).
              03  Z08-YUKOSTYMDH.
                04  Z08-VALUE   PIC X(9).
              03  Z08-YUKOEDYMDH.
                04  Z08-VALUE   PIC X(9).
              03  Z08-NAMEH.
                04  Z08-VALUE   PIC X(100).
              03  Z08-NAME.
                04  Z08-VALUE   PIC X(100).
              03  Z08-KANANAME.
                04  Z08-VALUE   PIC X(100).
              03  Z08-YUKOSTYMD.
                04  Z08-VALUE   PIC X(10).
              03  Z08-YUKOEDYMD.
                04  Z08-VALUE   PIC X(10).
              03  Z08-FORMALNAME.
                04  Z08-VALUE   PIC X(200).
              03  Z08-IDOKANREN.
                04  Z08-VALUE   PIC X(9).
              03  Z08-IDOKANRENNAME.
                04  Z08-VALUE   PIC X(100).
              03  Z08-IDOKIKAN.
                04  Z08-IDOKIKAN-STYLE   PIC X(20).
                04  Z08-IDOKIKAN-VALUE   PIC X(30).
              03  Z08-COPYCD.
                04  Z08-VALUE   PIC X(9).
              03  Z08-TEN.
                04  Z08-VALUE   PIC S9(8)V9(2).
              03  Z08-TANICD-COMB.
                04  Z08-TANICD.
                  05  Z08-VALUE   PIC X(30).
                04  Z08-TANICD-COUNT   PIC S9(9)   BINARY.
                04  Z08-TANICD-ITEM
                        PIC X(30)   OCCURS  200 TIMES.
              03  Z08-MSGLBEL.
                04  Z08-VALUE   PIC X(100).
              03  Z08-MIDASHI.
                04  Z08-MIDASHI-STYLE  PIC X(20).
                04  Z08-MIDASHI-DATA   PIC X(80).
              03  Z08-SCROLLEDWINDOW1.
                04  Z08-SHONAMELIST.
                  05  Z08-SHONAMELIST-COUNT   PIC S9(9)   BINARY.
                  05  Z08-SHONAMELIST-ROW PIC S9(9)   BINARY.
                  05  Z08-SHONAMELIST-SELECT
                          PIC X       OCCURS  200 TIMES.
                  05  Z08-ITEM        OCCURS  200 TIMES.
                    06  Z08-RENNUM  PIC X(3).
                    06  Z08-SNAME   PIC X(200).
                    06  Z08-KIKAKU  PIC X(120).
              03  Z08-SELNUM.
                04  Z08-TEXTDATA    PIC X(3).
              03  Z08-MNEXT.
                04  Z08-MNEXT-STYLE   PIC X(20).
                04  Z08-MNEXT-DATA    PIC X(30).
              03  Z08-B03.
                04  Z08-B03-STATE   PIC S9(9)   BINARY.
