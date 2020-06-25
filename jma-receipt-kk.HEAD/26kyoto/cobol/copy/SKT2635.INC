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
      ********************************************************************
      *    後期高齢総括表（京都府）     COPY (SKT2635.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/11    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者     日付        内容
      *  01.00.01    吉川       2008/04/29  特別療養費は指定の場所に印字
      *  01.00.02    門間       2018/12/04  修正履歴コメント削除
      ********************************************************************
       01  SKT2635.
      *    診療年月
           03  SKT2635-SRYYM              PIC  X(16).
      *    保険者名
           03  SKT2635-HKNJANAME          PIC  X(100).
      *    郵便番号
           03  SKT2635-POST               PIC  X(8).
      *    住所
           03  SKT2635-ADRS               PIC  X(100).
      *    医療機関名
           03  SKT2635-HOSPNAME           PIC  X(100).
      *    開設者名
           03  SKT2635-KAISETUNAME        PIC  X(40).
      *    電話番号
           03  SKT2635-TEL                PIC  X(15).
      *    医療機関コード
           03  SKT2635-HOSPCD             PIC  X(14).
      *    特別療養費件数
           03  SKT2635-T-KENSU            PIC  X(06).
      *    特別療養費点数
           03  SKT2635-T-TENSU            PIC  X(09).
      *    請求テーブル
           03  SKT2635-SEIKYU-TBL    OCCURS  3.
      *    入外テーブル
               05  SKT2635-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2635-KENSU      PIC  X(06).
      *    点数
                   07  SKT2635-TENSU      PIC  X(09).
      *    一部負担金
                   07  SKT2635-ITBFTN     PIC  X(09).
      *    他
                   07  SKT2635-TAKOH      PIC  X(02).
      *    長期
                   07  SKT2635-TYOKI      PIC  X(02).
      *    原爆
                   07  SKT2635-GENBAKU    PIC  X(02).
      *    実日数
                   07  SKT2635-NISSU      PIC  X(03).
      *    食事療養：件数
                   07  SKT2635-S-KENSU    PIC  X(06).
      *    食事療養：金額
                   07  SKT2635-S-KINGK    PIC  X(09).
      *    食事療養：標準負担額
                   07  SKT2635-S-FTNGK    PIC  X(09).
