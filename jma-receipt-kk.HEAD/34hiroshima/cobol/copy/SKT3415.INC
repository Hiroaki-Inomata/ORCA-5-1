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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    田中      2008/04/26   新規作成(後期高齢者対応)
      *  01.00.02    吉川      2008/05/03   保険者番号追加
      *  01.00.03    井上      2019/01/10   修正履歴コメント削除
      ************************************************************
        01  SKT3415.
      *    診療年月
            03  SKT3415-SRYYM          PIC X(16).
            03  SKT3415-HKNJANUM       PIC X(16).
      *    広域連合名
            03  SKT3415-HKNJANAME      PIC X(10).
      *    在
            03  SKT3415-ZAI1           PIC X(2).
      *    在○
            03  SKT3415-ZAI2           PIC X(2).
      *    請求年月日
            03  SKT3415-SEIYMD         PIC X(22).
      *    医療機関コード
            03  SKT3415-HOSPCD         PIC X(40).
      *    保険医療機関の所在地
            03  SKT3415-ADRS           PIC X(100).
      *    保険医療機関の名称
            03  SKT3415-HOSPNAME       PIC X(100).
      *    開設者氏名
            03  SKT3415-KAISETUNAME    PIC X(40).
      *    NO
            03  SKT3415-NO             PIC X(4).
      *    特別療養費
            03  SKT3415-TOKUBETU        PIC  X(10).
      *    請求テーブル
            03  SKT3415-SEIKYU-TBL     OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT3415-NYUGAI-TBL   OCCURS  2 TIMES.
      *    件数
                07  SKT3415-KENSU      PIC X(5).
      *    診療実日数
                07  SKT3415-NISSU      PIC X(5).
      *    点数
                07  SKT3415-TENSU      PIC X(9).
      *    一部負担金
                07  SKT3415-ITBFTN     PIC X(9).
      *    食事療養：件数
              05  SKT3415-S-KENSU      PIC X(5).
      *    食事療養：日数
              05  SKT3415-S-NISSU      PIC X(5).
      *    食事療養：金額
              05  SKT3415-S-KINGK      PIC X(9).
      *    食事療養：標準負担額
              05  SKT3415-S-FTNGK      PIC X(9).
      *
      *    原爆テーブル
            03  SKT3415-GSEIKYU-TBL.
      *    入外テーブル
              05  SKT3415-GNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT3415-GKENSU     PIC X(5).
      *    診療実日数
                07  SKT3415-GNISSU     PIC X(5).
      *    点数
                07  SKT3415-GTENSU     PIC X(9).
      *    一部負担金
                07  SKT3415-GITBFTN    PIC X(9).
      *    食事療養：件数
              05  SKT3415-S-GKENSU     PIC X(5).
      *    食事療養：日数
              05  SKT3415-S-GNISSU     PIC X(5).
      *    食事療養：金額
              05  SKT3415-S-GKINGK     PIC X(9).
      *    食事療養：標準負担額
              05  SKT3415-S-GFTNGK     PIC X(9).
      *
      *    福祉医療テーブル
            03  SKT3415-FSEIKYU-TBL    OCCURS  3 TIMES.
      *    入外テーブル
              05  SKT3415-FNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT3415-FKENSU     PIC X(5).
      *    診療実日数
                07  SKT3415-FNISSU     PIC X(5).
      *    点数
                07  SKT3415-FTENSU     PIC X(9).
      *    一部負担金
                07  SKT3415-FITBFTN    PIC X(9).
      *
      *    公費請求テーブル
            03  SKT3415-KSEIKYU-TBL    OCCURS  3 TIMES.
      *    入外テーブル
              05  SKT3415-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT3415-KKENSU     PIC X(5).
      *    診療実日数
                07  SKT3415-KNISSU     PIC X(5).
      *    点数
                07  SKT3415-KTENSU     PIC X(9).
      *    一部負担金
                07  SKT3415-KITBFTN    PIC X(9).
      *    公費法別番号
              05  SKT3415-HBTNUM       PIC X(2).
      *    公費名称
              05  SKT3415-KOHNAME      PIC X(10).
      *    食事療養：件数
              05  SKT3415-S-KKENSU     PIC X(5).
      *    食事療養：日数
              05  SKT3415-S-KNISSU     PIC X(5).
      *    食事療養：標準負担額
              05  SKT3415-S-KKINGK     PIC X(9).
      *    食事療養：標準負担額
              05  SKT3415-S-KFTNGK     PIC X(9).
      *
      *    長期高額療養費請求テーブル
            03  SKT3415-TYKSEIKYU-TBL.
      *    管掌別(一般・退職)テーブル
              05  SKT3415-KANSYO-TBL   OCCURS  2 TIMES.
      *    件数
                07  SKT3415-TKENSU     PIC X(5).
      *    金額
                07  SKT3415-S-TKINGK   PIC X(9).
