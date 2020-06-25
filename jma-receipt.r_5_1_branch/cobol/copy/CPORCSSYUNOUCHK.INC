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
      *    一括再計算該当者チェック用パラメタ   COPY (CPORCSSYUNOUCHK.INC)
      *                    使用プログラム            (ORCSSYUNOUCHK)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  LNK-ORCSSYUNOUCHK-REC.
      *
      *医療機関ＩＤ
           03  LNK-ORCSSYUNOUCHK-HOSPNUM        PIC 9(2).
      *入外区分
           03  LNK-ORCSSYUNOUCHK-NYUGAIKBN      PIC X(01).
      *診療年月
           03  LNK-ORCSSYUNOUCHK-SRYYM          PIC X(06).
      *患者請求額の変更がない方の一括再計算
      *　　０：該当者チェック時に自動で一括再計算を行わない（デフォルト）
      *　　１：該当者チェック時に自動で一括再計算を行う
           03  LNK-ORCSSYUNOUCHK-OPTION1        PIC X(01).
      *
      *返却値
      *
           03  LNK-ORCSSYUNOUCHK-OUT-AREA.
             05  LNK-ORCSSYUNOUCHK-OUT-OCC      OCCURS 10000.
      *患者ＩＤ
               07  LNK-ORCSSYUNOUCHK-PTID       PIC 9(10).
      *状態フラグ
      *　　０：【患者請求額の変更がある方】
      *　　７：【患者請求額の変更がない方】
      *　　８：【患者請求額の変更がない方】　※該当者チェック時に自動で一括再計算済
      *　　９：【該当者チェックでエラーとなった方】
               07  LNK-ORCSSYUNOUCHK-JTIFLG     PIC 9(01).
      *該当区分
      *　　１：第三者行為現物給付（通常診療分の優先計算該当）
      *　　２：７０歳未満現物給付（公費併用分の優先計算該当）
      *　　３：高齢者現物給付（公費併用分の優先計算該当）
      *　　４：収納更新未処理（会計照会【収納更新】未処理の収納あり）
      *　　５：該当者チェック抽出分（データ項目値が変更となる収納あり）
      *　　６：診療行為で訂正登録が必要な収納あり。訂正登録後に再チェック。
      *　　７：第三者行為現物給付（第三者行為分の優先計算該当）
      *　　８：マル長計算
               07  LNK-ORCSSYUNOUCHK-GAITOKBN   PIC 9(02).
      *
      *復帰コード
      *
           03  LNK-ORCSSYUNOUCHK-RC             PIC 9(02).
      *
