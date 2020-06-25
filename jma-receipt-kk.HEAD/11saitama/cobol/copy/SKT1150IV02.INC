        01  HCM2IV02.
            02  HCM2IV02-PRTYM  PIC X(16).
            02  HCM2IV02-SEIYMD PIC X(22).
            02  HCM2IV02-HOSPCD PIC X(20).
            02  HCM2IV02-ADRS   PIC X(100).
            02  HCM2IV02-HOSPNAME   PIC X(100).
            02  HCM2IV02-KAISETUNAME    PIC X(60).
            02  HCM2IV02-IHO-TBL1       OCCURS  11 TIMES.
              03  HCM2IV02-KENSU1 PIC X(6).
              03  HCM2IV02-NISSU1 PIC X(6).
              03  HCM2IV02-TENSU1 PIC X(13).
              03  HCM2IV02-ITBFTN1    PIC X(13).
              03  HCM2IV02-SYOKENSU1  PIC X(6).
              03  HCM2IV02-SYONISSU1  PIC X(6).
              03  HCM2IV02-SYOKINGAK1 PIC X(13).
              03  HCM2IV02-SYOFTNGAK1 PIC X(13).
            02  HCM2IV02-IHO-TBL2       OCCURS  9 TIMES.
              03  HCM2IV02-KENSU2 PIC X(6).
              03  HCM2IV02-NISSU2 PIC X(6).
              03  HCM2IV02-TENSU2 PIC X(13).
              03  HCM2IV02-ITBFTN2    PIC X(13).
              03  HCM2IV02-SYOKENSU2  PIC X(6).
              03  HCM2IV02-SYONISSU2  PIC X(6).
              03  HCM2IV02-SYOKINGAK2 PIC X(13).
              03  HCM2IV02-SYOFTNGAK2 PIC X(13).
            02  HCM2IV02-IHO-TBL3       OCCURS  12 TIMES.
              03  HCM2IV02-KENSU3 PIC X(6).
              03  HCM2IV02-NISSU3 PIC X(6).
              03  HCM2IV02-TENSU3 PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) start
              03  HCM2IV02-ITBFTN3    PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) end
              03  HCM2IV02-SYOKENSU3  PIC X(6).
              03  HCM2IV02-SYONISSU3  PIC X(6).
              03  HCM2IV02-SYOKINGAK3 PIC X(13).
              03  HCM2IV02-SYOFTNGAK3 PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) start
      *      02  HCM2IV02-ITBFTN3    PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) end
            02  HCM2IV02-IHO-TBL4       OCCURS  9 TIMES.
              03  HCM2IV02-KENSU4 PIC X(6).
              03  HCM2IV02-NISSU4 PIC X(6).
              03  HCM2IV02-TENSU4 PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) start
              03  HCM2IV02-ITBFTN4    PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) end
              03  HCM2IV02-SYOKENSU4  PIC X(6).
              03  HCM2IV02-SYONISSU4  PIC X(6).
              03  HCM2IV02-SYOKINGAK4 PIC X(13).
              03  HCM2IV02-SYOFTNGAK4 PIC X(13).
            02  HCM2IV02-IHO-TBL5       OCCURS  9 TIMES.
              03  HCM2IV02-KENSU5 PIC X(6).
              03  HCM2IV02-NISSU5 PIC X(6).
              03  HCM2IV02-TENSU5 PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) start
              03  HCM2IV02-ITBFTN5    PIC X(13).
      *=== 一部負担金の記載対応 by Yoshikawa(2012/11/14) end
              03  HCM2IV02-SYOKENSU5  PIC X(6).
              03  HCM2IV02-SYONISSU5  PIC X(6).
              03  HCM2IV02-SYOKINGAK5 PIC X(13).
              03  HCM2IV02-SYOFTNGAK5 PIC X(13).
            02  HCM2IV02-KENSUG PIC X(6).
            02  HCM2IV02-SYOKENSUG  PIC X(6).
            02  HCM2IV02-TITLE  PIC X(50).
            02  HCM2IV02-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM2IV02-TITLE1 PIC X(10).
      *=== H20年10月改正対応 by Yoshikawa(2008/10/24) start
            02  HCM2IV02-SEIDONAME
                    PIC X(8)    OCCURS  5 TIMES.
      *=== H20年10月改正対応 by Yoshikawa(2008/10/24) start
      *=== 災害対応 by Yoshikawa(2011/04/01) start
            02  HCM2IV02-GENMEN PIC X(12).
      *=== 災害対応 by Yoshikawa(2011/04/01) end
