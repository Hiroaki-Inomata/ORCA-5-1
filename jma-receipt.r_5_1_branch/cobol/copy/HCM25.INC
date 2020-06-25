        01  HCM25.
            02  HCM25-HAKOUYM.
              03  HCM25-HAKKOUYY  PIC X(2).
              03  HCM25-HAKKOUMM  PIC X(2).
            02  HCM25-PTNUM PIC X(20).
            02  HCM25-RENNUM    PIC X(7).
            02  HCM25-KIGONUM   PIC X(50).
            02  HCM25-HKNJANAME PIC X(60).
            02  HCM25-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM25-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM25-KANANAME  PIC X(50).
            02  HCM25-NAME  PIC X(50).
            02  HCM25-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM25-TANJYOYY  PIC X(2).
            02  HCM25-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM25-NENREI    PIC X(3).
            02  HCM25-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM25-JYUSYOUBI-G.
              03  HCM25-JYUSYOUBIYY   PIC X(2).
              03  HCM25-JYUSYOUBIMM   PIC X(2).
              03  HCM25-JYUSYOUBIDD   PIC X(2).
            02  HCM25-SYOSHINBI-G.
              03  HCM25-SYOSHINBIYY   PIC X(2).
              03  HCM25-SYOSHINBIMM   PIC X(2).
              03  HCM25-SYOSHINBIDD   PIC X(2).
            02  HCM25-STR-G.
              03  HCM25-STRYY PIC X(2).
              03  HCM25-STRMM PIC X(2).
              03  HCM25-STRDD PIC X(2).
            02  HCM25-END-G.
              03  HCM25-ENDYY PIC X(2).
              03  HCM25-ENDMM PIC X(2).
              03  HCM25-ENDDD PIC X(2).
            02  HCM25-JITSUNISSU    PIC X(3).
            02  HCM25-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM25-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM25-KHNTEN
                    PIC X(7)    OCCURS  37 TIMES.
            02  HCM25-KAISU
                    PIC X(3)    OCCURS  37 TIMES.
            02  HCM25-TENSU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM25-KINGAKU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM25-TANKA PIC X(4).
            02  HCM25-TENSUG    PIC X(7).
            02  HCM25-KINGAKUG  PIC X(7).
            02  HCM25-SRYKBN
                    PIC X(2)    OCCURS  29 TIMES.
            02  HCM25-TEKIYO
                    PIC X(52)   OCCURS  29 TIMES.
            02  HCM25-SHINDANSYOSUU PIC X(3).
            02  HCM25-FTNWARIAI PIC X(3).
            02  HCM25-MEISAISYOSUU  PIC X(3).
            02  HCM25-SEIKYU
                    PIC X(10)   OCCURS  8 TIMES.
            02  HCM25-SEIKYUG   PIC X(10).
            02  HCM25-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCM25-TSUIN-G           OCCURS  3 TIMES.
              03  HCM25-TSUINMM   PIC X(2).
              03  HCM25-TSUINSUU  PIC X(2).
              03  HCM25-TSUIN
                      PIC X(2)    OCCURS  31 TIMES.
            02  HCM25-SEIKYUYMD.
              03  HCM25-SEIKYUYY  PIC X(2).
              03  HCM25-SEIKYUMM  PIC X(2).
              03  HCM25-SEIKYUDD  PIC X(2).
            02  HCM25-HOSPJYUSYO    PIC X(100).
            02  HCM25-HOSPNAME  PIC X(80).
            02  HCM25-ISYA  PIC X(40).
            02  HCM25-TEL   PIC X(15).
            02  HCM25-BYOSYOSU  PIC X(4).
            02  HCM25-HOKENNAME PIC X(60).
            02  HCM25-PAGE  PIC X(5).
