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
      *    短期滞在３包括チェックパラメタ　COPY (CPORCSTANKI3HKT.INC)
      *                                使用プログラム (ORCSTANKI3HKT)
      *    設定項目
      *      医療機関番号
      *      患者ＩＤ
      *      算定年月日
      *      診療行為コード
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  17/04/XX  検体検査管理加算包括対応
      ************************************************************
       01  ORCSTANKI3HKT-AREA.
      *
      *医療機関番号
           03  LNK-TANKI3HKT-HOSPNUM           PIC  9(2).
      *患者ＩＤ
           03  LNK-TANKI3HKT-PTID              PIC  9(10).
      *算定年月日
           03  LNK-TANKI3HKT-SANTEIYMD         PIC  X(08).
      *診療行為コード
           03  LNK-TANKI3HKT-SRYCD             PIC  X(09).
      *H29.4
      *１：短期滞在３包括により算定不可対象とする
           03  LNK-TANKI3HKT-HKTKBN            PIC  X(01).
      *返却項目
           03  LNK-TANKI3HKT-HENKYAKU-AREA.
      *包括区分（空白：算定可　１：短期滞在３包括により算定不可）
             05  LNK-TANKI3HKT-KBN             PIC  X(01).
      *H29.4
      *包括区分２（１：短期滞在３包括により算定可の検査判断料）
             05  LNK-TANKI3HKT-KBN2            PIC  X(01).
      *
      *復帰コード（１：患者検索エラー　２：算定年月日エラー）
           03  LNK-TANKI3HKT-RC                PIC S9(9) BINARY.
      *
