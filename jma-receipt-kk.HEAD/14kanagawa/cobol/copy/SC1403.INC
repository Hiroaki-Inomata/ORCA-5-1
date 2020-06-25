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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      *  01.00.01    門間     15/06/08    件数欄の桁数変更
      ************************************************************
       01  SC03.
      *印刷日付
           03  SC03-PRTYM             PIC  X(16).
      *点数表区分
           03  SC03-TENHYOKBN1        PIC  X(02).
           03  SC03-TENHYOKBN2        PIC  X(02).
           03  SC03-TENHYOKBN3        PIC  X(02).
           03  SC03-TENHYOKBN4        PIC  X(02).
      *医療機関コード
           03  SC03-HOSPCD            PIC  X(14).
      *医療機関所在地
           03  SC03-ADRS              PIC  X(100).
      *医療機関名称
           03  SC03-HOSPNAME          PIC  X(100).
      *電話番号
           03  SC03-TEL               PIC  X(15).
      *管理者名
           03  SC03-KAISETUNAME       PIC  X(80).
      *請求日付
           03  SC03-SEIYMD            PIC  X(22).
      *請求テーブル
           03  SC03-SEIKYU-TBL   OCCURS  8.
      *負担者番号
               05  SC03-FTNJANUM      PIC  X(08).
      *件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-KENSU         PIC  X(03).
               05  SC03-KENSU         PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
      *点数
               05  SC03-TENSU         PIC  X(09).
      *薬剤一部負担金：件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-YKZKENSU      PIC  X(03).
               05  SC03-YKZKENSU      PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
      *薬剤一部負担金：金額
               05  SC03-YKZFTN        PIC  X(09).
      *食事療養：件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-SKJKENSU      PIC  X(03).
               05  SC03-SKJKENSU      PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
      *食事療養：金額
               05  SC03-SKJFTN        PIC  X(09).
      *合計
           03  SC03-GOKEI-TBL.
      *合計−件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-GKENSU        PIC  X(03).
               05  SC03-GKENSU        PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
      *合計−点数
               05  SC03-GTENSU        PIC  X(10).
      *合計−薬剤一部負担金：件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-GYKZKENSU     PIC  X(03).
               05  SC03-GYKZKENSU     PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
      *合計−薬剤一部負担金：金額
               05  SC03-GYKZFTN       PIC  X(10).
      *合計−食事療養：件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-GSKJKENSU     PIC  X(03).
               05  SC03-GSKJKENSU     PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
      *合計−食事療養：標準負担額
               05  SC03-GSKJFTN       PIC  X(10).
      *再掲欄
           03  SC03-SAIKEI-TBL   OCCURS  5.
      *再掲欄−件数
      *=== 件数欄の桁数変更 by Monma(2015/06/08) start
      *         05  SC03-SAIKEI        PIC  X(03).
               05  SC03-SAIKEI        PIC  X(05).
      *=== 件数欄の桁数変更 by Monma(2015/06/08) end
