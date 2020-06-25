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
      *  01.00.01    吉川    2011/01/12  特別療養費国保対応
      *  01.00.02    萩野    2019/02/07  修正履歴コメント削除
      ********************************************************************
       01  SKT4230.
      *    医療機関コード
           03  SKT4230-HOSPCD              PIC  X(07).
      *    在総診・在医総
           03  SKT4230-ZAI1                PIC  X(02).
           03  SKT4230-ZAI2                PIC  X(02).
      *    長期
           03  SKT4230-TYOKI1              PIC  X(02).
           03  SKT4230-TYOKI2              PIC  X(02).
      *    ページ番号
           03  SKT4230-NO                  PIC  X(04).
      *    特別療養費
           03  SKT4230-TOKUBETU            PIC  X(10).
      *    請求テーブル(原爆・被曝)
           03  SKT4230-GENBK-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT4230-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4230-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4230-NISSU       PIC  X(05).
      *    点数
                   07  SKT4230-TENSU       PIC  X(09).
      *    薬剤一部負担金
                   07  SKT4230-YKZFTN      PIC  X(09).
      *    一部負担金
                   07  SKT4230-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT4230-SKENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT4230-SNISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT4230-SKINGK          PIC  X(09).
      *    食事療養：標準負担額
               05  SKT4230-SFTNGK          PIC  X(09).
      *    請求テーブル(その他公費)
           03  SKT4230-KOH-TBL  OCCURS  8.
      *    法別
               05  SKT4230-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT4230-KOHNAME         PIC  X(08).
      *    法制
               05  SKT4230-HOUSEI          PIC  X(02).
      *    入外テーブル
               05  SKT4230-K-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4230-K-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4230-K-NISSU       PIC  X(05).
      *    点数
                   07  SKT4230-K-TENSU       PIC  X(09).
      *    薬剤一部負担金
                   07  SKT4230-K-YKZFTN      PIC  X(09).
      *    一部負担金
                   07  SKT4230-K-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT4230-K-SKENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT4230-K-SNISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT4230-K-SKINGK          PIC  X(09).
      *    食事療養：標準負担額
               05  SKT4230-K-SFTNGK          PIC  X(09).
