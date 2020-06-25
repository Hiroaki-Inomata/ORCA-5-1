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
      *    患者番号変換パラメタ                         *
      *                         COPY  (CPORCSPTID.INC)  *
      *
      *     SPTID-RC    : ZERO （正常終了）
      *                  1     (値エラー）
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSPTIDAREA.
      *入力値
           03  SPTID-HOSPNUM            PIC 9(2).
      *    患者番号構成情報検索区分
           03  SPTID-SYS1009-KBN        PIC 9(1).
      *
           03  SPTID-PTNUM              PIC X(20).
           03  SPTID-PTID               PIC 9(10).
      *---- TAKE MOD ST 
           03  SPTID-DBSUBNAME          PIC X(20).
      *---- TAKE MOD ED 
      *
      *復帰コード
           03  SPTID-RC                 PIC S9(9)   BINARY.
      *
