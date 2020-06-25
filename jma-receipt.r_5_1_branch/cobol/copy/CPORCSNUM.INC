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
      *    数値変換サブパラメタ                         *
      *                         COPY  (CPORCSNUM.INC)    *
      *
      *     復帰コード   : ZERO （正常終了）
      *                    1     (入力値エラー）
      *                    3     (数値１３桁以上等のエラー）
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNUMAREA.
      *入力値
           03  SNUM-INX              PIC X(26).
      *数字出力値
           03  SNUM-OUTNUM           PIC S9(12)V9(06).
      *出力値
           03  SNUM-OUTEDT           PIC X(15).
      *桁数
           03  SNUM-KETSUU           PIC 9(02).
      *整数部桁数
           03  SNUM-SEISUU           PIC 9(02).
      *小数部桁数
           03  SNUM-SYOSUU           PIC 9(02).
      *−値
           03  SNUM-MINKBN           PIC X(01).
      *小数部有無
           03  SNUM-SYOKBN           PIC X(01). 
      *
      *復帰コード
           03  SNUM-RC               PIC S9(9)   BINARY.
      *
