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
      *  01.00.02    萩野      2019/01/11   修正履歴コメント削除
      ********************************************************************
       01  SKT3510.
      *診療年月
           03  SKT3510-SRYYM               PIC  X(16).
      *請求年月日
           03  SKT3510-SEIYMD              PIC  X(22).
      *医療機関コード
           03  SKT3510-HOSPCD              PIC  X(20).
      *保険者番号
           03  SKT3510-HKNJANUM            PIC  X(12).
      *保険者名
           03  SKT3510-HKNJANAME           PIC  X(24).
      *医療機関所在地
           03  SKT3510-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT3510-HOSPNAME            PIC  X(100).
      *電話番号
           03  SKT3510-TEL                 PIC  X(20).
      *開設者氏名
           03  SKT3510-KAISETUNAME         PIC  X(40).
      *診療科CD
           03  SKT3510-SRYKACD             PIC  X(02).
      *診療科名
           03  SKT3510-SRYKANAME           PIC  X(20).
      *在
           03  SKT3510-ZAI1                PIC  X(02).
           03  SKT3510-ZAI2                PIC  X(02).
      *特別療養費
           03  SKT3510-TOKUBETU            PIC  X(10).
      *ページ番号
           03  SKT3510-NO                  PIC  X(04).
      *請求一覧
           03  SKT3510-SEIKYU  OCCURS 9.
               05  SKT3510-NYUGAI-TBL  OCCURS  2.
      *療養の給付：件数
                   07  SKT3510-KENSU       PIC  X(05).
      *療養の給付：診療実日数
                   07  SKT3510-NISSU       PIC  X(05).
      *療養の給付：点数
                   07  SKT3510-TENSU       PIC  X(09).
      *療養の給付：一部負担金
                   07  SKT3510-ITBFTN      PIC  X(09).
      *食事療養：件数
               05  SKT3510-SKENSU         PIC  X(05).
      *食事療養：日数
               05  SKT3510-SNISSU         PIC  X(05).
      *食事療養：金額
               05  SKT3510-SKINGK         PIC  X(09).
      *食事療養：標準負担額
               05  SKT3510-SFTNGK         PIC  X(09).
