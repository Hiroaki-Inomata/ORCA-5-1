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
      ******************************************************************
      ******************************************************************
      *    データチェックエラー情報(大容量API返却)                      
      ******************************************************************
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付     内容
      ******************************************************************
       01  DC-REC.
           03  DC-OCC                  OCCURS 1000.
               05  DC-RENBAN           PIC X(05).
               05  DC-PTNUM            PIC X(20).
               05  DC-NAME             PIC X(100).
               05  DC-KANANAME         PIC X(100).
               05  DC-BIRTHDAY         PIC X(10).
               05  DC-SEX              PIC X(1).
               05  DC-HOKEN-NAME       PIC X(20).
               05  DC-ERRCD            PIC X(06).
               05  DC-ERRMSG           PIC X(220).
