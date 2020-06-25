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
      *  01.00.01    吉川    2011/06/15  特別療養費国保対応
      *  01.00.02    吉川    2012/04/19  帳票様式の変更
      *  01.00.03    井上    2018/10/11  修正履歴コメント削除
      ********************************************************************
       01  SKT0320.
      *    ページ番号
           03  SKT0320-NO                PIC  X(04).
      *    特別療養費
           03  SKT0320-TOKUBETU          PIC  X(10).
      *    公費テーブル
           03  SKT0320-KOH-TBL     OCCURS  9.
      *    入外テーブル
               05  SKT0320-NYUGAI-TBL     OCCURS  2.
      *    件数
                   07  SKT0320-KENSU     PIC  X(05).
      *    実日数
                   07  SKT0320-NISSU     PIC  X(05).
      *    点数
                   07  SKT0320-TENSU     PIC  X(09).
      *    一部負担金
                   07  SKT0320-ITBFTN    PIC  X(09).
      *    食事療養：件数
               05  SKT0320-S-KENSU       PIC  X(05).
      *    食事療養：日数
               05  SKT0320-S-NISSU       PIC  X(05).
      *    食事療養：金額
               05  SKT0320-S-KINGAKU     PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0320-S-FTNGAKU     PIC  X(09).
      *    公費テーブル
           03  SKT0320-KOHNUM-TBL     OCCURS  3.
      *    公費名
               05  SKT0320-KOHNAME       PIC  X(10).
      *    公費番号
               05  SKT0320-KOHNUM        PIC  X(02).
