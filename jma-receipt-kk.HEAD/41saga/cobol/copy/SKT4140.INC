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
      *  01.00.02    萩野    2017/04/14  点数の桁数増加
      *  01.00.03    萩野    2019/02/07  修正履歴コメント削除
      ********************************************************************
        01  SKT4140.
      *     医療機関コード
            03  SKT4140-HOSPCD                       PIC X(07).
      *     ページ番号
            03  SKT4140-NO                           PIC X(04).
      *     法別番号
            03  SKT4140-HBTNUM                       PIC X(02).
      *     公費名
            03  SKT4140-KOHNAME                      PIC X(10).
      *     公費請求テーブル
            03  SKT4140-KOUHI-TBL                  OCCURS  8 TIMES.
      *     入外テーブル
                05  SKT4140-KNYUGAI-TBL            OCCURS  2 TIMES.
      *     件数
                    07  SKT4140-KKENSU                 PIC X(05).
      *     実日数
                    07  SKT4140-KNISSU                 PIC X(05).
      *     点数
                    07  SKT4140-KTENSU                 PIC X(10).
      *     薬剤一部負担金
                    07  SKT4140-KYKZFTN                PIC X(07).
      *     一部負担金
                    07  SKT4140-KITBFTN                PIC X(09).
      *     控除額
                    07  SKT4140-KKOUJO                 PIC X(09).
      *     食事療養：件数
                05  SKT4140-S-KKENSU                   PIC X(05).
      *     食事療養：日数
                05  SKT4140-S-KNISSU                   PIC X(05).
      *     食事療養：金額
                05  SKT4140-S-KKINGK                   PIC X(09).
      *     食事療養：標準負担額
                05  SKT4140-S-KFTNGK                   PIC X(09).
      *     特別療養費
            03  SKT4140-TOKUBETU                     PIC  X(10).
