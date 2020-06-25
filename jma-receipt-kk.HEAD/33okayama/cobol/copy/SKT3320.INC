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
      *  01.00.01    吉川      2010/12/09   特別療養費国保対応
      *  01.00.02    井上      2019/01/10   修正履歴コメント削除
      ********************************************************************
       01  SKT3320.
      *    保険者番号
           03       SKT3320-HKNJANUM              PIC  X(12).
      *    医療機関コード
           03       SKT3320-HOSPCD                PIC  X(14).
      *    特別療養費
           03       SKT3320-TOKUBETU              PIC  X(10).
      *    ページ番号
           03       SKT3320-NO                    PIC  X(04).
      *    請求テーブル
           03       SKT3320-HKN-TBL.
               05  SKT3320-ROUHO-TBL  OCCURS  2.      
                   07  SKT3320-NYUGAI-TBL  OCCURS  2.
                       09  SKT3320-KENSU                PIC  X(05).
                       09  SKT3320-NISSU                PIC  X(05).
                       09  SKT3320-TENSU                PIC  X(09).
                       09  SKT3320-ITBFTN               PIC  X(09).
                   07  SKT3320-SKENSU                   PIC  X(05).
                   07  SKT3320-SNISSU                   PIC  X(05).
                   07  SKT3320-SKINGK                   PIC  X(09).
                   07  SKT3320-SFTNGK                   PIC  X(09).
               05  SKT3320-ZAITAKU-TBL OCCURS 2.
                   07  SKT3320-ZKENSU                   PIC  X(03).
                   07  SKT3320-ZNISSU                   PIC  X(03).
                   07  SKT3320-ZTENSU                   PIC  X(09).
                   07  SKT3320-ZITBFTN                  PIC  X(09).
               05  SKT3320-KOH-TBL  OCCURS 8.
                   07  SKT3320-HBTNUM                  PIC  X(03).
                   07  SKT3320-TANSEIDONAME            PIC  X(10).
                   07  SKT3320-NYUGAI-TBL  OCCURS  2.
                       09  SKT3320-K-KENSU             PIC  X(05).
                       09  SKT3320-K-NISSU             PIC  X(05).
                       09  SKT3320-K-TENSU             PIC  X(09).
                       09  SKT3320-K-ITBFTN            PIC  X(09).
                   07  SKT3320-K-SKENSU                PIC  X(05).
                   07  SKT3320-K-SNISSU                PIC  X(05).
                   07  SKT3320-K-SKINGK                PIC  X(09).
                   07  SKT3320-K-SFTNGK                PIC  X(09).
