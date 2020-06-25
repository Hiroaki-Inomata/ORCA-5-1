        01  SKT4300.
            02  SKT4300-SRYYM   PIC X(16).
            02  SKT4300-POST.
              03  SKT4300-POST-OCC
                      PIC X(2)    OCCURS  7 TIMES.
            02  SKT4300-SRYKANAME
                    PIC X(16)   OCCURS  2 TIMES.
            02  SKT4300-NO  PIC X(3).
            02  SKT4300-ADRS    PIC X(100).
            02  SKT4300-HOSPNAME    PIC X(100).
            02  SKT4300-TEL PIC X(15).
            02  SKT4300-KAISETU PIC X(30).
            02  SKT4300-HOSPCDN PIC X(20).
            02  SKT4300-SEIKYU          OCCURS  8 TIMES.
              03  SKT4300-HKNJANAME   PIC X(20).
              03  SKT4300-KYURATE PIC X(6).
              03  SKT4300-HKN-TBL         OCCURS  7 TIMES.
                04  SKT4300-NYUGAIKBN       OCCURS  2 TIMES.
                  05  SKT4300-KENSU   PIC X(4).
                  05  SKT4300-TENSU   PIC X(7).
                04  SKT4300-SKINGK  PIC X(7).
                04  SKT4300-SFTNGK  PIC X(7).
              03  SKT4300-SSKENSU PIC X(4).
              03  SKT4300-SSKINGK PIC X(7).
              03  SKT4300-SSFTNGK PIC X(7).
            02  SKT4300-GOKEI-AREA.
              03  SKT4300-G-HKN-TBL       OCCURS  7 TIMES.
                04  SKT4300-G-NYUGAIKBN     OCCURS  2 TIMES.
                  05  SKT4300-G-KENSU PIC X(4).
                  05  SKT4300-G-TENSU PIC X(7).
                04  SKT4300-G-SKINGK    PIC X(7).
                04  SKT4300-G-SFTNGK    PIC X(7).
              03  SKT4300-G-SSKENSU   PIC X(4).
              03  SKT4300-G-SSKINGK   PIC X(7).
              03  SKT4300-G-SSFTNGK   PIC X(7).
            02  SKT4300-KOHI-TBL        OCCURS  6 TIMES.
              03  SKT4300-HBTNUM  PIC X(2).
              03  SKT4300-KNYUGAIKBN      OCCURS  2 TIMES.
                04  SKT4300-K-KENSU PIC X(4).
                04  SKT4300-K-TENSU PIC X(7).
              03  SKT4300-K-SKENSU    PIC X(4).
              03  SKT4300-K-SKINGK    PIC X(7).
              03  SKT4300-K-SFTNGK    PIC X(7).
