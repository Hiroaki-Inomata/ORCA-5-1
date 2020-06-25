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
      *    患者保険確認年月日履歴更新パラメタ　COPY (CPORCSPTHKNRRK.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSPTHKNRRKAREA.
      *入力
      *患者ＩＤ
           03  ORCSHKNRRK-PTID             PIC  9(10).
      *保険ＩＤ
           03  ORCSHKNRRK-HKNID            PIC  9(10).
      *今回確認年月日
           03  ORCSHKNRRK-KAKUNINYMD       PIC  X(10).
      *エラーコード
           03  ORCSHKNRRK-RC               PIC S9(9)   BINARY.
      *
      *
