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
      *  01.00.03    井上    2018/10/10  修正履歴コメント削除
      ********************************************************************
       01  SKT0310.
      *    ページ番号
           03  SKT0310-NO                   PIC  X(04).
      *    診療年月
           03  SKT0310-SRYYM                PIC  X(16).
      *    保険者名
           03  SKT0310-HKNJANAME            PIC  X(26).
      *    保険者番号
           03  SKT0310-HKNJANUM             PIC  X(12).
      *    請求年月日
           03  SKT0310-SEIYMD               PIC  X(22).
      *    医療機関コード
           03  SKT0310-HOSPCDN              PIC  X(40).
      *    医療機関住所
           03  SKT0310-ADRS                 PIC  X(100).
      *    医療機関名
           03  SKT0310-HOSPNAME             PIC  X(100).
      *    電話番号
           03  SKT0310-TEL                  PIC  X(15).
      *    開設者名
           03  SKT0310-KAISETUNAME          PIC  X(40).
      *    割合（一般保険者）
           03  SKT0310-KYURATE              PIC  X(02).
      *
           03  SKT0310-KUMIAI1              PIC  X(02).
           03  SKT0310-KUMIAI2              PIC  X(04).
           03  SKT0310-KUMIAI3              PIC  X(02).
      *    特別療養費
           03  SKT0310-TOKUBETU             PIC  X(10).
      *    請求テーブル
           03  SKT0310-SEIKYU-TBL     OCCURS  8.
      *    入外テーブル
               05  SKT0310-NYUGAI-TBL     OCCURS  2.
      *    件数
                   07  SKT0310-KENSU        PIC  X(05).
      *    実日数
                   07  SKT0310-NISSU        PIC  X(05).
      *    点数
                   07  SKT0310-TENSU        PIC  X(09).
      *    一部負担金
                   07  SKT0310-ITBFTN       PIC  X(09).
      *    長期
                   07  SKT0310-TYOKI        PIC  X(02).
      *    食事療養：件数
               05  SKT0310-S-KENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT0310-S-NISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT0310-S-KINGAKU        PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0310-S-FTNGAKU        PIC  X(09).
      *    在総診テーブル
           03  SKT0310-ZAI-TBL     OCCURS  4.
      *    件数
               05  SKT0310-ZKENSU           PIC  X(05).
      *    実日数
               05  SKT0310-ZNISSU           PIC  X(05).
      *    点数
               05  SKT0310-ZTENSU           PIC  X(09).
      *    一部負担金
               05  SKT0310-ZITBFTN          PIC  X(09).
      *    長期
               05  SKT0310-ZTYOKI           PIC  X(02).
