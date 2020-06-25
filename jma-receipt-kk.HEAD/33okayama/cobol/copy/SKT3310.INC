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
       01  SKT3310.
      *    診療年月
           03       SKT3310-SRYYM                 PIC  X(16).
      *    医療機関住所
           03       SKT3310-ADRS                  PIC  X(100).
      *    医療機関名
           03       SKT3310-HOSPNAME              PIC  X(100).
      *    電話番号
           03       SKT3310-TEL                   PIC  X(15).
      *    開設者名
           03       SKT3310-KAISETUNAME           PIC  X(40).
      *    保険者名
           03       SKT3310-HKNJANAME             PIC  X(26).
      *    請求年月日
           03       SKT3310-SEIYMD                PIC  X(22).
      *    保険者番号
           03       SKT3310-HKNJANUM              PIC  X(12).
      *    医療機関コード
           03       SKT3310-HOSPCD                PIC  X(14).
      *    特別療養費
           03       SKT3310-TOKUBETU              PIC  X(10).
      *    ページ番号
           03       SKT3310-NO                    PIC  X(04).
      *    請求テーブル
           03       SKT3310-HKN-TBL.
               05  SKT3310-PAN-TBL  OCCURS  9.      
                   07  SKT3310-NYUGAI-TBL  OCCURS  2.
                       09  SKT3310-KENSU              PIC  X(05).
                       09  SKT3310-NISSU              PIC  X(05).
                       09  SKT3310-TENSU              PIC  X(09).
                       09  SKT3310-ITBFTN             PIC  X(09).
                   07  SKT3310-SKENSU                 PIC  X(05).
                   07  SKT3310-SNISSU                 PIC  X(05).
                   07  SKT3310-SKINGK                  PIC  X(09).
                   07  SKT3310-SFTNGK                  PIC  X(09).
           03       SKT3310-ZAITAKU-TBL   OCCURS 4.
               05  SKT3310-ZKENSU                     PIC  X(05).
               05  SKT3310-ZNISSU                     PIC  X(05).
               05  SKT3310-ZTENSU                     PIC  X(09).
               05  SKT3310-ZITBFTN                    PIC  X(09).
