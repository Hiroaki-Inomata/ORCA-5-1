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
      *    病棟選定入院自己負担取得  COPY (CPORCSBTUSENTEI.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSBTUSENTEIAREA.
      *  医療機関コード
           03  SBTUSENTEI-HOSPNUM          PIC 9(2).
      *  患者ＩＤ
           03  SBTUSENTEI-PTID             PIC  9(10).
      *    診療年月
           03  SBTUSENTEI-SRYYM            PIC  X(06).
      *
      *  出力領域
      *    通算日数
           03  SBTUSENTEI-OUT-AREA.
               05  SBTUSENTEI-DAY-AREA.
                   07  SBTUSENTEI-DAY-OCC  OCCURS  31.
      *                病棟番号
                       09  SBTUSENTEI-DAY-BTUNUM
                                           PIC  X(02).
      *                選定入院設定区分
      *                1:定率設定 / 2:定額設定
                       09  SBTUSENTEI-DAY-FTNKBN
                                           PIC  X(01).
      *                消費税区分
      *                0:なし / 1:あり
                       09  SBTUSENTEI-DAY-TAXKBN
                                           PIC  X(01).
      *                点数単価（定率設定時有効）
                       09  SBTUSENTEI-DAY-TANKA
                                           PIC  9(02).
      *                １日毎の金額（定額設定時有効）
                       09  SBTUSENTEI-DAY-KINGAKU
                                           PIC  9(04).
      *  リターンコード
           03  SBTUSENTEI-RC               PIC 9(02).
