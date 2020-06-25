        01  HCM07V02.
            02  HCM07V02-SYORIKBN   PIC X(14).
            02  HCM07V02-SAKYMD PIC X(10).
            02  HCM07V02-SRYKA  PIC X(40).
            02  HCM07V02-SYORIYMD   PIC X(24).
            02  HCM07V02-KIKAN  PIC X(100).
            02  HCM07V02-PAGE   PIC X(3).
            02  HCM07V02-NAIYO-TBL      OCCURS  30 TIMES.
              03  HCM07V02-PTNUM  PIC X(20).
              03  HCM07V02-NAME   PIC X(36).
              03  HCM07V02-SEX    PIC X(2).
              03  HCM07V02-BIRTHDAY   PIC X(22).
              03  HCM07V02-DENPNUM    PIC X(7).
              03  HCM07V02-SRYMD  PIC X(12).
              03  HCM07V02-SEIKYU PIC X(11).
              03  HCM07V02-RYOSYU PIC X(11).
              03  HCM07V02-MISYU  PIC X(11).
              03  HCM07V02-JYOTAI PIC X(10).
            02  HCM07V02-TTL-TBL        OCCURS  3 TIMES.
              03  HCM07V02-TTLMSG PIC X(10).
              03  HCM07V02-SEIKYUTTL  PIC X(11).
              03  HCM07V02-RYOSYUTTL  PIC X(11).
              03  HCM07V02-MISYUTTL   PIC X(11).
            02  HCM07V02-SEIKYUTTLMSG   PIC X(10).
            02  HCM07V02-RYOSYUTTLMSG   PIC X(10).
            02  HCM07V02-MISYUTTLMSG    PIC X(10).
            02  HCM07V02-CREYMD PIC X(30).
