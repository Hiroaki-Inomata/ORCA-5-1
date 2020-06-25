      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
          02  Q02A.
            03  Q02A-FIXED17.
              04  Q02A-B05-STATE                PIC S9(9)   BINARY.
              04  Q02A-B05S-STATE               PIC S9(9)   BINARY.
              04  Q02A-B06-STATE                PIC S9(9)   BINARY.
              04  Q02A-B06S-STATE               PIC S9(9)   BINARY.
              04  Q02A-B07-STATE                PIC S9(9)   BINARY.
              04  Q02A-B09-STATE                PIC S9(9)   BINARY.
              04  Q02A-B09S-STATE               PIC S9(9)   BINARY.
              04  Q02A-B10-STATE                PIC S9(9)   BINARY.
              04  Q02A-B11-STATE                PIC S9(9)   BINARY.
              04  Q02A-B12-STATE                PIC S9(9)   BINARY.
              04  Q02A-B12S-STATE               PIC S9(9)   BINARY.
              04  Q02A-HYODAI                   PIC X(100).
              04  Q02A-JYOKEN1                  PIC X(100).
              04  Q02A-JYOKEN2                  PIC X(100).
              04  Q02A-SCR-KEKKALST.
                05  Q02A-KEKKALST.
                  06  Q02A-KEKKALST-COUNT       PIC S9(9)   BINARY.
                  06  Q02A-KEKKALST-ROW         PIC S9(9)   BINARY.
                  06  Q02A-KEKKALST-ROWATTR     PIC S9(9)   BINARY.
                  06  Q02A-KEKKALST-ITEM    OCCURS  200.
                    07  Q02A-KEKKALST-NO        PIC X(5).
                    07  Q02A-KEKKALST-PTNUM     PIC X(20).
                    07  Q02A-KEKKALST-NAME      PIC X(50).
                    07  Q02A-KEKKALST-SEX       PIC X(2).
                    07  Q02A-KEKKALST-BIRTH     PIC X(10).
                    07  Q02A-KEKKALST-AGE       PIC X(08).
                    07  Q02A-KEKKALST-NYUGAI    PIC X(02).
                    07  Q02A-KEKKALST-HKN       PIC X(10).
                    07  Q02A-KEKKALST-KOH1      PIC X(10).
                    07  Q02A-KEKKALST-KOH2      PIC X(10).
                    07  Q02A-KEKKALST-TOTALTEN  PIC X(13).
                  06  Q02A-KEKKALST-SELECT      PIC X
                                           OCCURS  200 TIMES.
              04  Q02A-TOTAL                    PIC X(22).
              04  Q02A-TOTAL-STYLE              PIC X(20).
              04  Q02A-SELNUM                   PIC 9(05).
              04  Q02A-DURATION   PIC S9(09) BINARY.
