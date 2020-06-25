        01  HCM55.
            02  HCM55-HAKOUYM.
              03  HCM55-HAKKOUYY  PIC X(2).
              03  HCM55-HAKKOUMM  PIC X(2).
            02  HCM55-PTNUM PIC X(20).
            02  HCM55-RENNUM    PIC X(7).
            02  HCM55-KIGONUM   PIC X(50).
            02  HCM55-HKNJANAME PIC X(60).
            02  HCM55-SINRYOKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM55-SYOBYOKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCM55-KANANAME  PIC X(50).
            02  HCM55-NAME  PIC X(50).
            02  HCM55-TANJYOG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM55-TANJYOYY  PIC X(2).
            02  HCM55-SEX
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM55-NENREI    PIC X(3).
            02  HCM55-SYOBYO
                    PIC X(50)   OCCURS  3 TIMES.
            02  HCM55-JYUSYOUBI-G.
              03  HCM55-JYUSYOUBIYY   PIC X(2).
              03  HCM55-JYUSYOUBIMM   PIC X(2).
              03  HCM55-JYUSYOUBIDD   PIC X(2).
            02  HCM55-SYOSHINBI-G.
              03  HCM55-SYOSHINBIYY   PIC X(2).
              03  HCM55-SYOSHINBIMM   PIC X(2).
              03  HCM55-SYOSHINBIDD   PIC X(2).
            02  HCM55-STR-G.
              03  HCM55-STRYY PIC X(2).
              03  HCM55-STRMM PIC X(2).
              03  HCM55-STRDD PIC X(2).
            02  HCM55-END-G.
              03  HCM55-ENDYY PIC X(2).
              03  HCM55-ENDMM PIC X(2).
              03  HCM55-ENDDD PIC X(2).
            02  HCM55-JITSUNISSU    PIC X(3).
            02  HCM55-TENKIKBN
                    PIC X(2)    OCCURS  5 TIMES.
            02  HCM55-SYOSHINKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM55-KHNTEN
                    PIC X(7)    OCCURS  37 TIMES.
            02  HCM55-KAISU
                    PIC X(3)    OCCURS  37 TIMES.
            02  HCM55-TENSU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM55-KINGAKU
                    PIC X(7)    OCCURS  38 TIMES.
            02  HCM55-TANKA PIC X(4).
            02  HCM55-TENSUG    PIC X(7).
            02  HCM55-KINGAKUG  PIC X(7).
            02  HCM55-SRYKBN
                    PIC X(2)    OCCURS  30 TIMES.
            02  HCM55-TEKIYO
                    PIC X(52)   OCCURS  30 TIMES.
            02  HCM55-SHINDANSYOSUU PIC X(3).
            02  HCM55-FTNWARIAI PIC X(3).
            02  HCM55-MEISAISYOSUU  PIC X(3).
            02  HCM55-SEIKYU
                    PIC X(10)   OCCURS  8 TIMES.
            02  HCM55-SEIKYUG   PIC X(10).
            02  HCM55-SEIKYUGAKUTOTAL   PIC X(10).
            02  HCM55-TSUIN-G           OCCURS  3 TIMES.
              03  HCM55-TSUINMM   PIC X(2).
              03  HCM55-TSUINSUU  PIC X(2).
              03  HCM55-TSUIN
                      PIC X(2)    OCCURS  31 TIMES.
            02  HCM55-SEIKYUYMD.
              03  HCM55-SEIKYUYY  PIC X(2).
              03  HCM55-SEIKYUMM  PIC X(2).
              03  HCM55-SEIKYUDD  PIC X(2).
            02  HCM55-HOSPJYUSYO    PIC X(100).
            02  HCM55-HOSPNAME  PIC X(80).
            02  HCM55-ISYA  PIC X(40).
            02  HCM55-TEL   PIC X(10).
            02  HCM55-BYOSYOSU  PIC X(4).
            02  HCM55-HOKENNAME PIC X(60).
            02  HCM55-PAGE  PIC X(5).
