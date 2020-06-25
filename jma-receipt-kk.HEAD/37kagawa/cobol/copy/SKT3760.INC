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
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/09/24  主科対応
      *  01.00.02    吉川         11/01/11  特別療養費国保対応
      *  01.00.03    萩野         19/01/11  修正履歴コメント削除
      ************************************************************
       01  SKT3760.
      *    診療年月
           03  SKT3760-SRYYM               PIC  X(16).
      *    保険者番号
           03  SKT3760-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT3760-HOSPCD              PIC  X(14).
      *    在総身・在医総
           03  SKT3760-ZAI1                PIC  X(02).
           03  SKT3760-ZAI2                PIC  X(02).
      *    ページ番号
           03  SKT3760-NO                  PIC  X(04).
      *    診療科
           03  SKT3760-RECEKA              PIC  X(20).
      *    特別療養費
           03  SKT3760-TOKUBETU            PIC  X(10).
      *    老人請求テーブル
           03       SKT3760-RSEIKYU-TBL OCCURS 2.
               05   SKT3760-RNYUGAI-TBL OCCURS 2.
      *    件数
                   07   SKT3760-KENSU   PIC X(05).
      *    診療実日数
                   07   SKT3760-NISSU   PIC X(05).
      *    点数
                   07   SKT3760-TENSU   PIC X(09).
      *    一部負担金
                   07   SKT3760-ITBFTN  PIC X(09).
      *    食事：件数
               05   SKT3760-S-KENSU      PIC X(05).
      *    食事：日数
               05   SKT3760-S-NISSU      PIC X(05).
      *    食事：金額
               05   SKT3760-S-KINGK      PIC X(09).
      *    食事：標準負担額
               05   SKT3760-S-FTNGK      PIC X(09).
      *    公費請求テーブル
           03       SKT3760-KSEIKYU-TBL OCCURS 6.
      *    公費名
               05   SKT3760-HBTNUM      PIC X(04).
               05   SKT3760-KOHNAME     PIC X(10).
               05   SKT3760-KNYUGAI-TBL OCCURS 2.
      *    件数
                   07   SKT3760-KKENSU   PIC X(05).
      *    診療実日数
                   07   SKT3760-KNISSU   PIC X(05).
      *    点数
                   07   SKT3760-KTENSU   PIC X(09).
      *    一部負担金
                   07   SKT3760-KITBFTN  PIC X(09).
      *    食事：件数
               05   SKT3760-S-KKENSU      PIC X(05).
      *    食事：日数
               05   SKT3760-S-KNISSU      PIC X(05).
      *    食事：金額
               05   SKT3760-S-KKINGK      PIC X(09).
      *    食事：標準負担額
               05   SKT3760-S-KFTNGK      PIC X(09).
