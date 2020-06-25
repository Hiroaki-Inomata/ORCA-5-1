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
       01  SKT3750.
      *    診療年月
           03  SKT3750-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT3750-SEIYMD              PIC  X(22).
      *    住所
           03  SKT3750-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT3750-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT3750-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT3750-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT3750-HKNJANAME           PIC  X(24).
      *    保険者番号
           03  SKT3750-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT3750-HOSPCD              PIC  X(14).
      *    在総診・在医総
           03  SKT3750-ZAI1                PIC  X(02).
           03  SKT3750-ZAI2                PIC  X(02).
      *    ページ番号
           03  SKT3750-NO                  PIC  X(04).
      *    診療科
           03  SKT3750-RECEKA              PIC  X(20).
      *    特別療養費
           03  SKT3750-TOKUBETU            PIC  X(10).
      *    件数
           03       SKT3750-KENSU-TBL OCCURS 9.
               05   SKT3750-KENSU     OCCURS 2 PIC X(05).
      *    診療実日数
           03       SKT3750-NISSU-TBL OCCURS 9.
               05   SKT3750-NISSU     OCCURS 2 PIC X(05).
      *    点数
           03       SKT3750-TENSU-TBL OCCURS 9.
               05   SKT3750-TENSU     OCCURS 2 PIC X(09).
      *    一部負担金
           03       SKT3750-ITBFTN-TBL OCCURS 9.
               05   SKT3750-ITBFTN     OCCURS 2 PIC X(09).
      *    件数
           03       SKT3750-S-KENSU-TBL OCCURS 9.
               05   SKT3750-S-KENSU      PIC X(05).
      *    診療実日数
           03       SKT3750-S-NISSU-TBL OCCURS 9.
               05   SKT3750-S-NISSU      PIC X(05).
      *    金額
           03       SKT3750-S-KINGK-TBL OCCURS 9.
               05   SKT3750-S-KINGK      PIC X(09).
      *    一部負担金
           03       SKT3750-S-FTNGK-TBL OCCURS 9.
               05   SKT3750-S-FTNGK     PIC X(09).
