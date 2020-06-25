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
      *----------------------------------------------------------*
      *    入院カルテ出力パラメタ　     COPY (ORCHCN01 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHCN01AREA.
      *    処理区分 (1:頭書きのみ印字/ 2:すべて印字） ... 必須
           03  ORCHCN01-SYORIKBN           PIC X(01).
      *    帳票区分 (1:カルテ 2:続紙 3:両方)<- 処理区分=１のときのみ設定
           03  ORCHCN01-PRTKBN             PIC X(01).
      *    主治医 ... 任意
           03  ORCHCN01-DR-TBL.
               05  ORCHCN01-DR-OCC         OCCURS  3.
                   07  ORCHCN01-DR         PIC X(05).
      *    入院日 ... 任意
           03  ORCHCN01-NYUINYMD           PIC X(08).
      *    退院日 ... 任意
           03  ORCHCN01-TAIINYMD           PIC X(08).
