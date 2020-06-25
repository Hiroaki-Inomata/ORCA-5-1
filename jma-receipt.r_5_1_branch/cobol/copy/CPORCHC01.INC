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
      *    外来カルテ出力パラメタ　     COPY (ORCHC01 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  06/02/06  一括印刷追加
      ************************************************************
       01  ORCHC01AREA.
      *    処理区分 (2:頭書きのみ印字/ 1:すべて印字） ... 必須
      *             (3:バッチ一括印刷）
           03  ORCHC01-SYORIKBN           PIC X(01).
      *
      *    一括用病名編集区分
           03  ORCHC01-BYOMEIFLG       PIC X(01).
      *    一括パラメタ
           03  ORCHC01-PRTAREA         PIC X(25000).
      *出力領域
      *    エラーコード
           03  ORCHC01-RC              PIC 9(02).
      *    一括出力枚数
           03  ORCHC01-CNT             PIC 9(04).
