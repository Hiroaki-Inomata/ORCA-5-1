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
      ************************************************************
      *    入院調剤料一括更新パラメタ　　COPY (CPORCSCHOZAI.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCHOZAIAREA.
      *
      *入力
           03  ORCSCHO-IN-AREA.
      *処理区分（１：通常、２：退院（剤削除））
               05  ORCSCHO-KBN                 PIC  X(01).
      *患者ＩＤ
               05  ORCSCHO-PTID                PIC  9(10).
      *診療年月
               05  ORCSCHO-SRYYM               PIC  X(06).
      *エラーコード（１：診療会計更新エラー
      *              ２：患者マスタなし
      *              ３：患者マスタ更新エラー
      *              ４：診療行為更新エラー
      *              ５：診療会計追加エラー
           03  ORCSCHO-ERRCD           PIC 9(01).
      *保険組み合わせ期間外作成あり
           03  ORCSCHO-HKNERR          PIC 9(01).
      *
