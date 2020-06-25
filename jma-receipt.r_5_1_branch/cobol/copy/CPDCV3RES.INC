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
      *    HAORIレスポンスデータチェック情報
      *    COPY (DCV3RES.INC)
      ******************************************************************
        01 DCRES-AREA.
           03  DCRES-CONTEXT                   PIC S9(9)   BINARY.
           03  DCRES-OBJECT                    PIC X(36).
           03  DCRES-MODE                      PIC S9(9)   BINARY.
           03  DCRES-RECORDNAME                PIC X(128).
           03  DCRES-DATA-AREA.
               05  DCRES-REQNUM                PIC X(2).
               05  DCRES-RESNUM                PIC X(2).
               05  DCRES-KARTE-UID             PIC X(36).
               05  DCRES-ORCA-UID              PIC X(36).
               05  DCRES-INFORMATION-DATE      PIC X(10).
               05  DCRES-INFORMATION-TIME      PIC X(8).
               05  DCRES-API-RESULT            PIC X(5).
               05  DCRES-API-RESULT-MSG        PIC X(100).
               05  DCRES-HOKEN-AREA.
                   07  DCRES-HOKEN-OCC         OCCURS  6.
                       09  DCRES-HOKENID       PIC X(50).
                       09  DCRES-HOKENNAME     PIC X(50).
               05  DCRES-ITEM-AREA.
                   07  DCRES-ITEM-OCC          OCCURS  21.
                       09  DCRES-ITEMID        PIC X(50).
                       09  DCRES-ITEMNAME      PIC X(50).
               05  DCRES-INGAIPROCKBN          PIC X(1).
               05  DCRES-INGAIPROCNAME         PIC X(20).
               05  DCRES-PRTSRTKBN             PIC X(1).
               05  DCRES-PRTSRTNAME            PIC X(20).
               05  DCRES-ERR-PTINF-AREA.
                   07  DCRES-ERR-PTINF-OCC     OCCURS  100.
                       09  DCRES-ERR-PTNUM         PIC X(20).
                       09  DCRES-ERR-PTNUM-ERRCD   PIC X(5).
                       09  DCRES-ERR-PTNUM-ERRMSG  PIC X(100).
               05  DCRES-DATA-ID-AREA.
                   07  DCRES-DATA-ID           PIC X(36).
                   07  DCRES-PRINT-ID          PIC X(36).
               05  DCRES-RECORD-COUNT          PIC X(5).
               05  DCRES-PAGE-COUNT            PIC X(5).
