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
      *    システム管理レコード定義取得サブルーチンパラメタ
      *                                   COPY (CPORCSSKDEF.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSSKDEFAREA.
      *    管理コード
           03  ORCSSKDEF-KANRICD       PIC X(04).
      *    レコード名称
           03  ORCSSKDEF-NAME          PIC X(40).
      *    区分コード
           03  ORCSSKDEF-KBNCD         PIC X(08).
      *    項目数
           03  ORCSSKDEF-ITEM-MAX      PIC 9(03).
      *    項目テーブル
           03  ORCSSKDEF-ITEM-TBL.
               05  ORCSSKDEF-ITEM-OCC          OCCURS  500.
      *            項目番号
                   07  ORCSSKDEF-ITEM-NO       PIC 9(03).
      *            項目名
                   07  ORCSSKDEF-ITEM          PIC X(30).
      *            項目名（漢字）
                   07  ORCSSKDEF-ITEM-JP       PIC X(80).
      *            項目タイプ（9:数字、
      *                        A:英字、
      *                        X:英数字（半角）、
      *                        N:英数字（全角）、
      *                        M:英数字（混在））
                   07  ORCSSKDEF-TYPE          PIC X(01).
      *            項目開始位置
                   07  ORCSSKDEF-START         PIC 9(03).
      *            項目長（最大サイズ）
                   07  ORCSSKDEF-LENGTH        PIC 9(03).
      *            項目精度
                   07  ORCSSKDEF-PRECISION     PIC 9(01).
      *
