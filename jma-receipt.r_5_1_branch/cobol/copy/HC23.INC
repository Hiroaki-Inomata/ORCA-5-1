          01  HC23.
              03  HC23-HAKKOUYY         PIC X(2).
              03  HC23-HAKKOUMM         PIC X(2).
              03  HC23-PTNUM            PIC X(20).
              03  HC23-NAME             PIC X(26).
              03  HC23-TANJYOG          PIC X(2)    OCCURS  4.
              03  HC23-TANJYOYY         PIC X(2).
              03  HC23-SEX              PIC X(2)    OCCURS  2.
              03  HC23-NENREI           PIC X(3).
              03  HC23-SYOBYO           PIC X(50)   OCCURS  3.
              03  HC23-JYUSYOUBI-G.
                  05  HC23-JYUSYOUBIYY  PIC X(2).
                  05  HC23-JYUSYOUBIMM  PIC X(2).
                  05  HC23-JYUSYOUBIDD  PIC X(2).
              03  HC23-SYOSHINBI-G.
                  05  HC23-SYOSHINBIYY  PIC X(2).
                  05  HC23-SYOSHINBIMM  PIC X(2).
                  05  HC23-SYOSHINBIDD  PIC X(2).
              03  HC23-STR-G.
                  05  HC23-STRYY        PIC X(2).
                  05  HC23-STRMM        PIC X(2).
                  05  HC23-STRDD        PIC X(2).
              03  HC23-END-G.
                  05  HC23-ENDYY        PIC X(2).
                  05  HC23-ENDMM        PIC X(2).
                  05  HC23-ENDDD        PIC X(2).
              03  HC23-JITSUNISSU       PIC X(3).
              03  HC23-TENKI            PIC X(2)    OCCURS  5.
              03  HC23-SYOSHIN          PIC X(2)    OCCURS  4.
              03  HC23-KHNTEN           PIC X(7)    OCCURS  32.
              03  HC23-KAISU            PIC X(3)    OCCURS  32.
              03  HC23-KOUTEN           PIC X(7)    OCCURS  33.
              03  HC23-KOUTENG          PIC X(7).
              03  HC23-YKZTENSU         PIC X(7)    OCCURS  33.
              03  HC23-YKZTENSUG        PIC X(7).
              03  HC23-SAISINKAISU      PIC X(2).
              03  HC23-SHIDOKAISU       PIC X(2).
              03  HC23-KINGAKU          PIC X(7)    OCCURS  6.
              03  HC23-TEKIYO1          PIC X(24)   OCCURS  6.
              03  HC23-SHINDANSYOSUU    PIC X(3).
              03  HC23-MEISAISYOSUU     PIC X(3).
              03  HC23-SHINDANSYOMONEY  PIC X(7).
              03  HC23-MEISAISYOMONEY   PIC X(7).
              03  HC23-SRYKBN           PIC X(2)    OCCURS  26.
              03  HC23-TEKIYO2          PIC X(52)   OCCURS  26.
              03  HC23-SEIKYU           PIC X(2)    OCCURS  2.
              03  HC23-SEIKYUGAKU       PIC X(9)    OCCURS  5.
              03  HC23-TSUIN-TBL.
                  05  HC23-TSUIN-G                  OCCURS  2.
                      07  HC23-TSUIN    PIC X(2)    OCCURS  31.
                  05  HC23-TSUINMM      PIC X(2)    OCCURS  2.
                  05  HC23-TSUINSUU     PIC X(2)    OCCURS  2.
              03  HC23-SEIKYUGAKUTOTAL  PIC X(9).
              03  HC23-SEIKYU-G.
                  05  HC23-SEIKYUYY     PIC X(2).
                  05  HC23-SEIKYUMM     PIC X(2).
                  05  HC23-SEIKYUDD     PIC X(2).
              03  HC23-HOSPJYUSYO       PIC X(40).
              03  HC23-HOSPNAME         PIC X(30).
              03  HC23-ISYA             PIC X(40).
              03  HC23-TEL              PIC X(15).
              03  HC23-BYOSYOSU         PIC X(4).
              03  HC23-KANANAME         PIC X(30).
