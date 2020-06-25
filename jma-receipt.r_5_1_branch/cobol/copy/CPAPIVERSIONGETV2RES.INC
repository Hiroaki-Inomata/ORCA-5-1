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
      ******************************************************************
      *    APIバージョン情報取得レスポンス
      *    COPY (CPAPIVERSIONGETV2RES.INC)
      ******************************************************************
        01  XML-AVGRES.
            03  AVGRES-CONTEXT                 PIC S9(9)   BINARY.
            03  AVGRES-OBJECT                  PIC X(36).
            03  AVGRES-MODE                    PIC S9(9)   BINARY.
            03  AVGRES-RECORDNAME              PIC X(128).
            03  AVGRES-AREA.
                05  AVGRES-INFORMATION-DATE    PIC X(10).
                05  AVGRES-INFORMATION-TIME    PIC X(8).
                05  AVGRES-RESULT              PIC X(4).
                05  AVGRES-RESULT-MSG          PIC X(100).
                05  AVGRES-OCC                 OCCURS 200.
                    07  AVGRES-URL             PIC X(64).
                    07  AVGRES-NAME            PIC X(64).
                    07  AVGRES-VERSION         PIC X(16).
                    07  AVGRES-HAORI-NAME      PIC X(64).
