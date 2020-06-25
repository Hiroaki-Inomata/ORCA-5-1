        01  HCM08V02.
            02  HCM08V02-SYORIKBN   PIC X(14).
            02  HCM08V02-SYORIYMD   PIC X(24).
            02  HCM08V02-KIKAN  PIC X(100).
            02  HCM08V02-PAGE   PIC X(3).
            02  HCM08V02-NAIYO-TBL      OCCURS  30 TIMES.
              03  HCM08V02-BTUNAME    PIC X(10).
              03  HCM08V02-BRMNUM PIC X(6).
              03  HCM08V02-SRYKA  PIC X(8).
              03  HCM08V02-PTNUM  PIC X(10).
              03  HCM08V02-PTNUM2 PIC X(20).
              03  HCM08V02-NAME   PIC X(30).
              03  HCM08V02-SEX    PIC X(2).
              03  HCM08V02-BIRTHDAY   PIC X(22).
              03  HCM08V02-DENPNUM    PIC X(7).
              03  HCM08V02-SRYMD  PIC X(5).
              03  HCM08V02-SEIKYU PIC X(12).
              03  HCM08V02-RYOSYU PIC X(12).
              03  HCM08V02-MISYU  PIC X(12).
              03  HCM08V02-JYOTAI PIC X(16).
            02  HCM08V02-TTL-TBL        OCCURS  3 TIMES.
              03  HCM08V02-TTLMSG PIC X(10).
              03  HCM08V02-SEIKYUTTL  PIC X(12).
              03  HCM08V02-RYOSYUTTL  PIC X(12).
              03  HCM08V02-MISYUTTL   PIC X(12).
            02  HCM08V02-SEIKYUTTLMSG   PIC X(10).
            02  HCM08V02-RYOSYUTTLMSG   PIC X(10).
            02  HCM08V02-MISYUTTLMSG    PIC X(10).
            02  HCM08V02-CREYMD PIC X(30).
