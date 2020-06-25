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
          02  Q02B.
            03  Q02B-FIXED17.
              04  Q02B-B05-STATE                PIC S9(9)   BINARY.
              04  Q02B-B05S-STATE               PIC S9(9)   BINARY.
              04  Q02B-B06-STATE                PIC S9(9)   BINARY.
              04  Q02B-B06S-STATE               PIC S9(9)   BINARY.
              04  Q02B-B07-STATE                PIC S9(9)   BINARY.
              04  Q02B-B09-STATE                PIC S9(9)   BINARY.
              04  Q02B-B09S-STATE               PIC S9(9)   BINARY.
              04  Q02B-B10-STATE                PIC S9(9)   BINARY.
              04  Q02B-B11-STATE                PIC S9(9)   BINARY.
              04  Q02B-B12-STATE                PIC S9(9)   BINARY.
              04  Q02B-B12S-STATE               PIC S9(9)   BINARY.
              04  Q02B-HYODAI                   PIC X(100).
              04  Q02B-JYOKEN1                  PIC X(100).
              04  Q02B-JYOKEN2                  PIC X(100).
              04  Q02B-SCR-KEKKALST.
                05  Q02B-KEKKALST.
                  06  Q02B-KEKKALST-COUNT       PIC S9(9)   BINARY.
                  06  Q02B-KEKKALST-ROW         PIC S9(9)   BINARY.
                  06  Q02B-KEKKALST-ROWATTR     PIC S9(9)   BINARY.
                  06  Q02B-KEKKALST-ITEM    OCCURS  200.
                    07  Q02B-KEKKALST-NO        PIC X(5).
                    07  Q02B-KEKKALST-PTNUM     PIC X(20).
                    07  Q02B-KEKKALST-NAME      PIC X(50).
                    07  Q02B-KEKKALST-SEX       PIC X(2).
                    07  Q02B-KEKKALST-AGE       PIC X(08).
                    07  Q02B-KEKKALST-SRYNAME   PIC X(80).
                    07  Q02B-KEKKALST-SRYSURYO  PIC X(11).
                    07  Q02B-KEKKALST-ZAIKAISU  PIC X(05).
                    07  Q02B-KEKKALST-SURYOTOTAL PIC X(13).
                    07  Q02B-KEKKALST-SRYYM     PIC X(10).
                    07  Q02B-KEKKALST-MEMO      PIC X(200).
                  06  Q02B-KEKKALST-SELECT      PIC X
                                           OCCURS  200 TIMES.
              04  Q02B-TOTAL                    PIC X(22).
              04  Q02B-TOTAL-STYLE              PIC X(20).
              04  Q02B-SELNUM                   PIC 9(05).
              04  Q02B-DURATION   PIC S9(09) BINARY.
