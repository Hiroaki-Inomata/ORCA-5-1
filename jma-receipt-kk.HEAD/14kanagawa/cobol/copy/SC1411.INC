      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       01  SC11.
      *印刷日付
           03  SC11-PRTYM             PIC  X(16).
      *点数表区分
           03  SC11-TENHYOKBN1        PIC  X(02).
           03  SC11-TENHYOKBN2        PIC  X(02).
           03  SC11-TENHYOKBN3        PIC  X(02).
           03  SC11-TENHYOKBN4        PIC  X(02).
           03  SC11-TENHYOKBN5        PIC  X(02).
      *医療機関コード
           03  SC11-HOSPCD            PIC  X(14).
      *医療機関所在地
           03  SC11-ADRS              PIC  X(100).
      *医療機関名称
           03  SC11-HOSPNAME          PIC  X(100).
      *電話番号
           03  SC11-TEL               PIC  X(15).
      *管理者名
           03  SC11-KAISETUNAME       PIC  X(80).
      *請求日付
           03  SC11-SEIYMD            PIC  X(22).
      *
           03  SC11-MEISAI-TBL OCCURS 8.
      *負担者番号
               05  SC11-FTNJANUM      PIC  X(08).
      *療養の給付：件数
               05  SC11-KENSU         PIC  X(03).
      *療養の給付：点数
               05  SC11-TENSU         PIC  X(09).
      *薬剤一部負担金：件数
               05  SC11-YKZKENSU      PIC  X(03).
      *薬剤一部負担金：金額
               05  SC11-YKZFTN        PIC  X(09).
      *一部負担金：件数
               05  SC11-ITBKENSU      PIC  X(03).
      *一部負担金：金額
               05  SC11-ITBFTN        PIC  X(09).
      *食事療養：件数
               05  SC11-SKJKENSU      PIC  X(03).
      *食事療養：標準負担額
               05  SC11-SKJFTN        PIC  X(09).
      *合計
           03  SC11-GOKEI-TBL.
      *
      *合計−療養の給付：件数
               05  SC11-GKENSU        PIC  X(03).
      *合計−療養の給付：点数
               05  SC11-GTENSU        PIC  X(10).
      *合計−薬剤一部負担金：件数
               05  SC11-GYKZKENSU     PIC  X(03).
      *合計−薬剤一部負担金：金額
               05  SC11-GYKZFTN       PIC  X(10).
      *合計−一部負担金：件数
               05  SC11-GITBKENSU     PIC  X(03).
      *合計−一部負担金：金額
               05  SC11-GITBFTN       PIC  X(10).
      *合計−食事療養：件数
               05  SC11-GSKJKENSU     PIC  X(03).
      *合計−食事療養：標準負担額
               05  SC11-GSKJFTN       PIC  X(10).
      *再掲欄
           03  SC11-SAIKEI-TBL   OCCURS  16.
               05  SC11-SAIKEI        PIC  X(03).
