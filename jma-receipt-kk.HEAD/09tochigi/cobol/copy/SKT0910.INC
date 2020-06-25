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
        01  SKT0910.
      *診療年月
            02  SKT0910-PRTYM   PIC X(16).
      *保険者番号
            02  SKT0910-HKNJANUM    PIC X(12).
      *請求年月日
            02  SKT0910-SEIYMD  PIC X(22).
      *県番号
            02  SKT0910-KENNUM  PIC X(4).
      *医療機関コード
            02  SKT0910-HOSPCD  PIC X(20).
      *医療機関所在地
            02  SKT0910-ADRS    PIC X(100).
      *医療機関名称
            02  SKT0910-HOSPNAME    PIC X(100).
      *開設者氏名
            02  SKT0910-KAISETUNAME PIC X(30).
      *ページ番号
            02  SKT0910-NO  PIC X(10).
      *療養の給付
            02  SKT0910-HKN-TBL.
      *一覧
              03  SKT0910-HKN-TBLG        OCCURS  9 TIMES.
      *入院・外来テーブル
                04  SKT0910-HKN-G           OCCURS  2 TIMES.
      *療養の給付：件数
                  05  SKT0910-KENSU   PIC X(6).
      *療養の給付：日数
                  05  SKT0910-NISSU   PIC X(6).
      *療養の給付：点数
                  05  SKT0910-TENSU   PIC X(13).
      *療養の給付：一部負担金
                  05  SKT0910-ITBFTN  PIC X(13).
      *食事療養
            02  SKT0910-SYO-TBL.
      *一覧
              03  SKT0910-SYO-TBLG        OCCURS  9 TIMES.
      *食事療養：件数
                04  SKT0910-SYOKENSU    PIC X(6).
      *食事療養：日数
                04  SKT0910-SYONISSU    PIC X(6).
      *食事療養：金額
                04  SKT0910-SYOKINGAK   PIC X(13).
      *食事療養：標準負担金
                04  SKT0910-SYOFTNGAK   PIC X(13).
            02  SKT0910-TITLE1  PIC X(10).
            02  SKT0910-HYOBETU
                    PIC X(2)    OCCURS  2 TIMES.
      *特別療養費
            02  SKT0910-TOKUBETU        PIC X(10).
