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
      *    診療報酬請求書     COPY (CPRMF001.INC)                *
      *
      *                        区分：ORCBM001
      *                              2:公費と医保・老人の併用
      *                              3:公費と公費の併用
      *                              4:公費単独
      *                              ORCBM004
      *                              1のみ
      *                        療養の給付情報
      *                               1:入院
      *                               2:入院外 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF01-REC.
           03  MF01-KEY.
      *区分
               05  MF01-KBN             PIC  X(01).
      *公費情報
               05  MF01-KOHINF          OCCURS  4.
      *公費−保険番号
                   07  MF01-KOHNUM      PIC  X(03).
      *療養の給付情報
           03  MF01-KYUFUINF            OCCURS  2.
      *件数
               05  MF01-KENSU           PIC  9(06).
      *日数
               05  MF01-NISSU           PIC  9(06).
      *点数
               05  MF01-TOTALTEN        PIC  9(10).
      *負担金額
               05  MF01-FTNMONEY        PIC  9(10).
      *薬剤一部負担金
               05  MF01-YKZFTN          PIC  9(10).
      *食事療養費情報
           03  MF01-SHOKUJIINF.
      *食事件数
               05  MF01-SHOKUJIKENSU    PIC  9(06).
      *食事日数
               05  MF01-SHOKUJINISSU    PIC  9(06).
      *食事療養費
               05  MF01-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF01-SHOKUJIFTN      PIC  9(09).
      *
