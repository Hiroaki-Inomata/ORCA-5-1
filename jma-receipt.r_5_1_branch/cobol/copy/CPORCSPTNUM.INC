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
      *                         COPY  (CPORCSPTNUM.INC)  *
      *
      *     SPTNUM-RC    : ZERO （正常終了）
      *                    99    (値エラー）
      *                    10    (データなし）
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.00    NACL-多々納  09/01/29  本院分院対応
      ************************************************************
       01  ORCSPTNUMAREA.
      *入力値
           03  SPTNUM-PTNUM              PIC X(20).
           03  SPTNUM-GRPHOSPNUM         PIC 9(02).
      *出力値
           03  SPTNUM-PTID               PIC 9(10).
      *
      *入力種別（１：半角、２：全角）
           03  SPTNUM-SYUBETU            PIC 9(01).
      *復帰コード
           03  SPTNUM-RC                 PIC S9(9)   BINARY.
      *
