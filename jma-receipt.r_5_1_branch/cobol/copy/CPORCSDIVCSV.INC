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
      ************************************************************
      *    CSVレコード分割            COPY (CPORCSDIVCSV.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSDIVCSV-AREA.
      *    CSVレコード
           03  ORCSDIVCSV-REC              PIC X(3000).
           03  ORCSDIVCSV-REC-R        REDEFINES   ORCSDIVCSV-REC.
               05  ORCSDIVCSV-CHAR         PIC X(01)   OCCURS  3000.
      *    要素数
           03  ORCSDIVCSV-ELE-C            PIC 9(03).
      *    デリミタ(1バイト)
           03  ORCSDIVCSV-DELI             PIC X(01).
      *    ダブルコーテーション有無(0:なし,1:あり)
           03  ORCSDIVCSV-DQ               PIC 9(01).
      *    要素情報
           03  ORCSDIVCSV-TABLE.
               05  ORCSDIVCSV-ELE          OCCURS 100.
                   07  ORCSDIVCSV-D        PIC X(300).
      *    結果コード（0:成功、1:引数不正）
           03  ORCSDIVCSV-RC               PIC 9(01).
