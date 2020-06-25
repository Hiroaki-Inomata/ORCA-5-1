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
      *  01.00.01    吉川    2010/02/03  特別療養費国保対応
      *  01.00.02    井上    2018/10/12  修正履歴コメント削除
      ********************************************************************
        01  SKT0911.
      *保険者番号
            02  SKT0911-HKNJANUM    PIC X(12).
      *県番号
            02  SKT0911-KENNUM  PIC X(10).
      *医療機関コード
            02  SKT0911-HOSPCD  PIC X(20).
      *ページ数
            02  SKT0911-NO  PIC X(10).
      *特別療養費
            02  SKT0911-TOKUBETU        PIC X(10).
      *公費請求一覧
            02  SKT0911-KOH-TBL         OCCURS  4 TIMES.
      *療養の給付：入院・外来
              03  SKT0911-KOH-G           OCCURS  2 TIMES.
      *療養の給付：件数
                04  SKT0911-KOH-KENSU   PIC X(6).
      *療養の給付：日数
                04  SKT0911-KOH-NISSU   PIC X(6).
      *療養の給付：点数
                04  SKT0911-KOH-TENSU   PIC X(13).
      *療養の給付：一部負担金
                04  SKT0911-KOH-ITBFTN  PIC X(13).
      *****
      *食事療養：
              03  SKT0911-KOH-SYOG.
      *食事療養：件数
                04  SKT0911-KOH-SYOKENSU    PIC X(6).
      *食事療養：日数
                04  SKT0911-KOH-SYONISSU    PIC X(6).
      *食事療養：金額
                04  SKT0911-KOH-SYOKINGAK   PIC X(13).
      *食事療養：標準負担金
                04  SKT0911-KOH-SYOFTNGAK   PIC X(13).
      *公費番号
              03  SKT0911-KOH-NUM PIC X(3).
      *公費名称
              03  SKT0911-KOH-NAME    PIC X(20).
