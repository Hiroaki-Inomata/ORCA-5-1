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
        02  S09.
      *     02  S09-FIXED4.
              03  S09-HKNJANUM         PIC X(8).
              03  S09-HKNJANAME        PIC X(40).
              03  S09-CMB-SYAKOK.
                04  S09-SYAKOK         PIC X(10).
                04  S09-SYAKOK-COUNT   PIC S9(9)   BINARY.
                04  S09-SYAKOK-ITEM    PIC X(10)   OCCURS  5 TIMES.
              03  S09-CMB-HONKZK.
                04  S09-HONKZK         PIC X(10).
                04  S09-HONKZK-COUNT   PIC S9(9)   BINARY.
                04  S09-HONKZK-ITEM    PIC X(10)   OCCURS  5 TIMES.
              03  S09-KIGO             PIC X(80).
              03  S09-NUM              PIC X(80).
