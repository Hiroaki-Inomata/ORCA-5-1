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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2011/02/09  特別療養費国保対応
      *  01.00.02    萩野    2018/10/10  修正履歴コメント削除
      ********************************************************************
       01  SKT0720.
      *    診療年月
      *     03  SKT0720-SRYYM             PIC  X(16).
      *    保険者番号
           03  SKT0720-HKNJANUM          PIC  X(12).
      *    医療機関コード
           03  SKT0720-HOSPCD            PIC  X(14).
      *    在宅
           03  SKT0720-ZAI1              PIC  X(02).
           03  SKT0720-ZAI2              PIC  X(02).
      *    ページ番号
           03  SKT0720-NO                PIC  X(04).
      *    特別療養費
           03  SKT0720-TOKUBETU          PIC  X(10).
      *    公費テーブル
           03  SKT0720-KOH-TBL     OCCURS  14.
      *    公費名
               05  SKT0720-KOHNAME       PIC  X(10).
      *    入外テーブル
               05  SKT0720-NYUGAI-TBL     OCCURS  2.
      *    件数
                   07  SKT0720-KENSU     PIC  X(05).
      *    実日数
                   07  SKT0720-NISSU     PIC  X(05).
      *    点数
                   07  SKT0720-TENSU     PIC  X(10).
      *    一部負担金
                   07  SKT0720-ITBFTN    PIC  X(09).
      *    食事療養：件数
               05  SKT0720-S-KENSU       PIC  X(05).
      *    食事療養：日数
               05  SKT0720-S-NISSU       PIC  X(05).
      *    食事療養：金額
               05  SKT0720-S-KINGAKU     PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0720-S-FTNGAKU     PIC  X(09).
