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
        01  SKT4150.
      *ページ番号
            03  SKT4150-NO                           PIC X(04).
      *診療年月
            03  SKT4150-SRYYM                        PIC X(16).
      *請求年月日
            03  SKT4150-SEIYMD                       PIC X(22).
      *医療機関住所
            03  SKT4150-ADRS                         PIC X(100).
      *医療機関名
            03  SKT4150-HOSPNAME                     PIC X(100).
      *開設者名
            03  SKT4150-KAISETUNAME                  PIC X(40).
      *電話番号
            03  SKT4150-TEL                          PIC X(15).
      *保険者番号
            03  SKT4150-HKNJANUM                     PIC X(06).
      *医療機関コード
            03  SKT4150-HOSPCD                       PIC X(07).
      *保険者名
            03  SKT4150-HOKENJANAME                  PIC X(20).
      *特別療養費
            03  SKT4150-TOKUBETU                     PIC X(10).
      *給付割合
            03  SKT4150-KYURATE-TBL               OCCURS  4 TIMES.
                05  SKT4150-KYURATE                  PIC X(02).
      *請求テーブル
            03  SKT4150-SEIKYU-TBL                OCCURS  6 TIMES.
      *件数
                05  SKT4150-KENSU                    PIC X(05).
      *日数
                05  SKT4150-NISSU                    PIC X(05).
      *点数
                05  SKT4150-TENSU                    PIC X(10).
      *一部負担金
                05  SKT4150-ITBFTN                   PIC X(09).
      *
      *公費請求テーブル
            03  SKT4150-KOUHI-TBL                 OCCURS  4 TIMES.
      *法別番号
                05  SKT4150-HBTNUM                   PIC X(02).
      *公費名
                05  SKT4150-KOHNAME                  PIC X(10).
      *件数
                05  SKT4150-KKENSU                   PIC X(05).
      *日数
                05  SKT4150-KNISSU                   PIC X(05).
      *点数
                05  SKT4150-KTENSU                   PIC X(10).
      *一部負担金
                05  SKT4150-KITBFTN                  PIC X(09).
      *患者負担金
                05  SKT4150-KKANFTN                  PIC X(09).
      *老人公費ー法別番号
            03  SKT4150-R-HBTNUM                     PIC X(02).
      *老人公費ー公費名
            03  SKT4150-R-KOHNAME                    PIC X(08).
      *
      *老人公費請求テーブル
            03  SKT4150-ROJINKOUHI-TBL            OCCURS  4 TIMES.
      *件数
                05  SKT4150-R-KKENSU                 PIC X(05).
      *日数
                05  SKT4150-R-KNISSU                 PIC X(05).
      *点数
                05  SKT4150-R-KTENSU                 PIC X(10).
      *一部負担
                05  SKT4150-R-KITBFTN                PIC X(09).
      *控除額
                05  SKT4150-R-KKOUJO                 PIC X(09).
