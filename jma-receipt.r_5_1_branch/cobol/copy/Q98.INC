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
         02  Q98.
            03  Q98-FIXED1.
              04  Q98-POST                 PIC X(7).
              04  Q98-ADRS                 PIC X(100).
              04  Q98-ADRSCHK-TBL.
                05  Q98-ADRSCHK-OCC        OCCURS  60.
                  06  Q98-TADRSCHK-STYLE   PIC X(20).
                  06  Q98-TADRS            PIC X(20).
              04  Q98-SELNUM               PIC 9(3).
