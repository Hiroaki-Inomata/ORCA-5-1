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
      *    保険組合せ名称編集パラメタ　　COPY (CPORCSHKNMEI.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSHKNMEIAREA.
      *
      *    保険組合せ番号
           03  ORCSHKNMEI-HKNCOMBINUM          PIC X(04).
      *    患者ＩＤ
           03  ORCSHKNMEI-PTID                 PIC 9(10).
      *    診療年月日
           03  ORCSHKNMEI-SRYYMD               PIC X(08).
      *
           03  ORCSHKNMEI-OUT-AREA.
      *        保険組合せ編集名称
               05  ORCSHKNMEI-HKNCOMBIMEI      PIC  X(100).
      *        適用期間
               05  ORCSHKNMEI-TEKSTYMD         PIC  X(08).
               05  ORCSHKNMEI-TEKEDYMD         PIC  X(08).
      *        保険種別
               05  ORCSHKNMEI-HKNNUM           PIC  X(03).
      *        保険区分
               05  ORCSHKNMEI-HKNKBN           PIC  9(01).
      *        労災・保険区分
               05  ORCSHKNMEI-RSI-HKNKBN       PIC  X(01).
      *エラーコード
           03  ORCSHKNMEI-ERRCD                PIC 9(01).
      *
