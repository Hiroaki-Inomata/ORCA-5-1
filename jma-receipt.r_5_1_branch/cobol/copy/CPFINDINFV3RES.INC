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
        01  XML-FINDINFV3RES.
            02  FINDINFRES-CONTEXT                 PIC S9(9)   BINARY.
            02  FINDINFRES-OBJECT                  PIC X(36).
            02  FINDINFRES-MODE                    PIC S9(9)   BINARY.
            02  FINDINFRES-RECORDNAME              PIC X(128).
            02  FINDINFRES-FINDINFV2RES.
              03  FINDINFRES-INFORMATION-DATE      PIC X(10).
              03  FINDINFRES-INFORMATION-TIME      PIC X(8).
              03  FINDINFRES-API-RESULT            PIC X(5).
              03  FINDINFRES-API-RESULT-MSG        PIC X(100).
              03  FINDINFRES-REQNUM                PIC X(2).
              03  FINDINFRES-KIHON-AREA.
                04 FINDINFRES-CONDITION1LST        OCCURS  100.
                  05  FINDINFRES-CONDITION1L       PIC X(2).
                  05  FINDINFRES-CONDITION1MEIL    PIC X(20).
                04 FINDINFRES-CONDITION2LST        OCCURS  100.
                  05  FINDINFRES-CONDITION2L       PIC X(2).
                  05  FINDINFRES-CONDITION2MEIL    PIC X(20).
                04 FINDINFRES-CONDITION3LST        OCCURS  100.
                  05  FINDINFRES-CONDITION3L       PIC X(2).
                  05  FINDINFRES-CONDITION3MEIL    PIC X(20).
                04  FINDINFRES-GENMENLST           OCCURS  99.
                  05  FINDINFRES-GENMENL           PIC X(2).
                  05  FINDINFRES-GENMENMEIL        PIC X(20).
                04  FINDINFRES-TOKKIINFOLST        OCCURS  50.
                  05  FINDINFRES-TOKKIINFOL        PIC X(2).
                  05  FINDINFRES-TOKKIINFOMEIL     PIC X(12).
              03  FINDINFRES-HOKEN-AREA.
                04  FINDINFRES-HKNNUMLST           OCCURS  50.
                  05  FINDINFRES-HKNNUML           PIC X(3).
                  05  FINDINFRES-HKNNUMMEIL        PIC X(16).
              03  FINDINFRES-KOHI-AREA.
                04  FINDINFRES-KOHNUMLST           OCCURS  150.
                  05  FINDINFRES-KOHNUML           PIC X(3).
                  05  FINDINFRES-KOHNUMMEIL        PIC X(16).
                  05  FINDINFRES-KOHNUM-TYPEL      PIC X(1).
