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
      *  01.00.01    吉川      2012/10/09   電話番号追加
      *  01.00.02    吉川      2014/05/12   年齢区分追加
      *  01.00.03    吉川      2016/06/06   備考３、備考４追加
      *  01.00.04    門間      2017/04/10   月遅れの場合の備考欄の記載修正
      *  01.00.05    萩野      2018/10/11   修正履歴コメント削除
      ********************************************************************
       01  SC09.
      *表示年月
              03  SC09-PRTYM            PIC  X(16).
      *請求年月日
              03  SC09-SEIYMD           PIC  X(22).
      *医療機関郵便番号
              03  SC09-POST             PIC  X(10).
      *医療機関住所
              03  SC09-ADRS             PIC  X(100).
      *医療機関名称
              03  SC09-HOSPNAME         PIC  X(100).
      *開設者名
              03  SC09-KAISETU          PIC  X(80).
      *医療機関コード
              03  SC09-HOSPCDN          PIC  X(20).
      *保険区分
              03  SC09-HKNKBN-TBL OCCURS 4.
                  05  SC09-HKNKBN       PIC  X(02).
      *患者テーブル
              03  SC09-DATA-TBL        OCCURS  10.
      *受給者番号
                  05  SC09-JKYSNUM      PIC  X(24).
      *氏名
                  05  SC09-NAME         PIC  X(20).
      *入院・外来区分
                  05  SC09-NYUGAIKBN1   PIC  X(02).
                  05  SC09-NYUGAIKBN2   PIC  X(02).
      *総点数
                  05  SC09-TOTALTEN     PIC  X(18).
      *結予・精神、公費負担点数
                  05  SC09-KOHFTNTEN    PIC  X(18).
      *一部負担額
                  05  SC09-ITBFTNGAKU   PIC  X(18).
      *合計請求
              03  SC09-GOKEI-TBL.
      *入院 件数
                  05  SC09-N-GKENSU     PIC  X(08).
      *入院 総点数
                  05  SC09-N-GTOTALTEN  PIC  X(18).
      *入院 結予、精神公費負担点数
                  05  SC09-N-GFTNTEN    PIC  X(18).
      *入院 一部負担額
                  05  SC09-N-GFTNGAKU   PIC  X(18).
      *外来 件数
                  05  SC09-G-GKENSU     PIC  X(08).
      *外来 総点数
                  05  SC09-G-GTOTALTEN  PIC  X(18).
      *外来 結予、精神公費負担点数
                  05  SC09-G-GFTNTEN    PIC  X(18).
      *外来 一部負担額
                  05  SC09-G-GFTNGAKU   PIC  X(18).
      *備考欄
              03  SC09-BIKO-TBL OCCURS 10.
                  05  SC09-BIKO         PIC  X(16).
              03  SC09-BIKO-TBL2 OCCURS 10.
                  05  SC09-BIKO2        PIC  X(16).
      *電話番号
              03  SC09-TEL              PIC  X(15).
      *年齢区分
              03  SC09-AGEKBN-TBL OCCURS 2.
                  05  SC09-AGEKBN       PIC  X(02).
      *備考欄３、４
              03  SC09-BIKO-TBL3 OCCURS 10.
                  05  SC09-BIKO3        PIC  X(16).
              03  SC09-BIKO-TBL4 OCCURS 10.
                  05  SC09-BIKO4        PIC  X(16).
      *備考欄５
              03  SC09-BIKO-TBL5 OCCURS 10.
                  05  SC09-BIKO5        PIC  X(22).
