        01  SKT3010.
            02  SKT3010-PRTYM   PIC X(16).
            02  SKT3010-HKNJANUM    PIC X(12).
            02  SKT3010-HKNJANAME   PIC X(26).
            02  SKT3010-ZAIMARU PIC X(2).
            02  SKT3010-ZAIMOJI PIC X(2).
            02  SKT3010-SEIYMD  PIC X(22).
            02  SKT3010-HOSPCD  PIC X(7).
            02  SKT3010-ADRS    PIC X(80).
            02  SKT3010-HOSPNAME    PIC X(80).
            02  SKT3010-KAISETUNAME PIC X(40).
            02  SKT3010-TEL PIC X(15).
            02  SKT3010-NO  PIC X(4).
            02  SKT3010-DATA1           OCCURS  2 TIMES.
              03  SKT3010-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT3010-KENSU   PIC X(5).
                04  SKT3010-NISSU   PIC X(5).
                04  SKT3010-TENSU   PIC X(9).
                04  SKT3010-ITBFTN  PIC X(9).
              03  SKT3010-SKENSU  PIC X(5).
              03  SKT3010-SNISSU  PIC X(5).
              03  SKT3010-SKINGK  PIC X(9).
              03  SKT3010-SFTNGK  PIC X(9).
            02  SKT3010-DATA2           OCCURS  12 TIMES.
              03  SKT3010-KOHNAME PIC X(8).
              03  SKT3010-HBTNUM  PIC X(2).
              03  SKT3010-NYUGAIKBN       OCCURS  2 TIMES.
                04  SKT3010-K-KENSU PIC X(5).
                04  SKT3010-K-NISSU PIC X(5).
                04  SKT3010-K-TENSU PIC X(9).
                04  SKT3010-K-ITBFTN    PIC X(9).
              03  SKT3010-K-SKENSU    PIC X(5).
              03  SKT3010-K-SNISSU    PIC X(5).
              03  SKT3010-K-SKINGK    PIC X(9).
              03  SKT3010-K-SFTNGK    PIC X(9).
            02  SKT3010-TYOKI-TBL       OCCURS  2 TIMES.
              03  SKT3010-TYOKI   PIC X(7).
            02  SKT3010-NYUIN   PIC X(2).
            02  SKT3010-GAIRAI  PIC X(2).
