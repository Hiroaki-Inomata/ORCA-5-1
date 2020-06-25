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
          02  Q02.
            03  Q02-FIXED17.
              04  Q02-B05-STATE                PIC S9(9)   BINARY.
              04  Q02-B05S-STATE               PIC S9(9)   BINARY.
              04  Q02-B06-STATE                PIC S9(9)   BINARY.
              04  Q02-B06S-STATE               PIC S9(9)   BINARY.
              04  Q02-B07-STATE                PIC S9(9)   BINARY.
              04  Q02-B09-STATE                PIC S9(9)   BINARY.
              04  Q02-B09S-STATE               PIC S9(9)   BINARY.
              04  Q02-B10-STATE                PIC S9(9)   BINARY.
              04  Q02-B11-STATE                PIC S9(9)   BINARY.
              04  Q02-B12-STATE                PIC S9(9)   BINARY.
              04  Q02-B12S-STATE               PIC S9(9)   BINARY.
              04  Q02-HYODAI                   PIC X(100).
              04  Q02-JYOKEN1                  PIC X(100).
              04  Q02-JYOKEN2                  PIC X(100).
              04  Q02-SCR-KEKKALST.
                05  Q02-KEKKALST.
                  06  Q02-KEKKALST-COUNT       PIC S9(9)   BINARY.
                  06  Q02-KEKKALST-ROW         PIC S9(9)   BINARY.
                  06  Q02-KEKKALST-ROWATTR     PIC S9(9)   BINARY.
                  06  Q02-KEKKALST-ITEM    OCCURS  200.
                    07  Q02-KEKKALST-NO        PIC X(5).
                    07  Q02-KEKKALST-PTNUM     PIC X(20).
                    07  Q02-KEKKALST-NAME      PIC X(50).
                    07  Q02-KEKKALST-SEX       PIC X(2).
                    07  Q02-KEKKALST-BIRTH     PIC X(10).
                    07  Q02-KEKKALST-AGE       PIC X(08).
                    07  Q02-KEKKALST-HKN       PIC X(06).
                    07  Q02-KEKKALST-LASTYMD   PIC X(10).
                    07  Q02-KEKKALST-TEL       PIC X(15).
                    07  Q02-KEKKALST-POST      PIC X(08).
                    07  Q02-KEKKALST-SRYACTNAIYO
                                               PIC X(200).
                    07  Q02-KEKKALST-PATIENT-ID1 PIC X(20).
                  06  Q02-KEKKALST-SELECT      PIC X
                                           OCCURS  200 TIMES.
              04  Q02-TOTAL                    PIC X(22).
              04  Q02-TOTAL-STYLE              PIC X(20).
              04  Q02-SELNUM                   PIC 9(05).
              04  Q02-DURATION   PIC S9(09) BINARY.
