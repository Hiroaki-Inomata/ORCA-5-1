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
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者   日付         内容
      *  01.00.01       内田   07/01/23    帳票様式の変更に伴う修正
      *  01.00.02       長屋   08/04/26    帳票様式に伴う修正
      *  01.00.03       吉川   11/01/11    特別療養費国保対応
      *  01.00.04       門間   18/10/09    不要コメントを削除
      *****************************************************************
        01  SKT0820.
      *医療機関コード
            03  SKT0820-HOSPCDN                      PIC X(20).
      *保険者番号
            03  SKT0820-HKNJANUM                     PIC X(12).
      *在総・在医総
            03  SKT0820-ZAI1                         PIC X(02).
            03  SKT0820-ZAI2                         PIC X(02).
      *ページ番号
            03  SKT0820-NO                           PIC X(04).
      *    特別療養費
           03  SKT0820-TOKUBETU                      PIC  X(10).
      *請求一覧
            03  SKT0820-SEIKYU                 OCCURS  2 TIMES.
      *入院・外来テーブル
              05  SKT0820-NYUGAIKBN            OCCURS  2 TIMES.
      *療養の給付：件数
                07  SKT0820-KENSU                    PIC X(05).
      *療養の給付：日数
                07  SKT0820-NISSU                    PIC X(05).
      *療養の給付：点数
                07  SKT0820-TENSU                    PIC X(09).
      *療養の給付：一部負担金
                07  SKT0820-ITBFTN                   PIC X(09).
      *食事療養：件数
              05  SKT0820-S-KENSU                    PIC X(05).
      *食事療養：日数
              05  SKT0820-S-NISSU                    PIC X(05).
      *食事療養：金額
              05  SKT0820-S-KINGK                    PIC X(09).
      *食事療養：標準負担金
              05  SKT0820-S-FTNGK                    PIC X(09).
      *公費請求一覧
            03  SKT0820-KSEIKYU                OCCURS  5 TIMES.
      *公費判別番号
              05  SKT0820-HBTNUM                     PIC X(02).
      *公費名
              05  SKT0820-KOHNAME                    PIC X(10).
      *入院・外来テーブル
              05  SKT0820-KNYUGAIKBN           OCCURS  2 TIMES.
      *療養の給付：件数
                07  SKT0820-KKENSU                   PIC X(05).
      *療養の給付：日数
                07  SKT0820-KNISSU                   PIC X(05).
      *療養の給付：点数
                07  SKT0820-KTENSU                   PIC X(09).
      *療養の給付：一部負担金
                07  SKT0820-KITBFTN                  PIC X(09).
      *食事療養：件数
              05  SKT0820-S-KKENSU                   PIC X(05).
      *食事療養：日数
              05  SKT0820-S-KNISSU                   PIC X(05).
      *食事療養：金額
              05  SKT0820-S-KKINGK                   PIC X(09).
      *食事療養：標準負担金
              05  SKT0820-S-KFTNGK                   PIC X(09).
      *備考欄エリア
            03  SKT0820-BIKO-AREA              OCCURS  4 TIMES.
      *入院・外来テーブル
              05  SKT0820-BNYUGAIKBN           OCCURS  2 TIMES.
      *療養の給付：件数
                07  SKT0820-BKENSU                   PIC X(05).
      *療養の給付：日数
                07  SKT0820-BNISSU                   PIC X(05).
      *療養の給付：点数
                07  SKT0820-BTENSU                   PIC X(09).
      *療養の給付：一部負担金
                07  SKT0820-BITBFTN                  PIC X(09).
      *入外区分
                07  SKT0820-BKBN                     PIC X(02).
      *ハイフン
                07  SKT0820-HAIFUN1                  PIC X(01).
                07  SKT0820-HAIFUN2                  PIC X(01).
      *一部負担文字
                07  SKT0820-FTNMOJI                  PIC X(02).
                07  SKT0820-FTNMARU                  PIC X(02).
      *公費法別番号
              05  SKT0820-B-MARU1                    PIC X(02).
              05  SKT0820-B-HBTNUM                   PIC X(02).
      *食事療養：件数
              05  SKT0820-S-BKENSU                   PIC X(05).
      *食事療養：日数
              05  SKT0820-S-BNISSU                   PIC X(05).
      *食事療養：金額
              05  SKT0820-S-BKINGK                   PIC X(09).
      *食事療養：標準負担金
              05  SKT0820-S-BFTNGK                   PIC X(09).
      *食事文字
              05  SKT0820-S-SMOJI                    PIC X(02).
              05  SKT0820-S-SMARU                    PIC X(02).
      *食事用ハイフン
              05  SKT0820-S-HAIFUN1                  PIC X(01).
              05  SKT0820-S-HAIFUN2                  PIC X(01).
              05  SKT0820-S-HAIFUN3                  PIC X(01).
