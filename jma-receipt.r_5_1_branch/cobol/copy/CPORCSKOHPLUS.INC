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
      *    公費付加情報取得用パラメタ    COPY (CPORCSKOHPLUS.INC)
      *                                  使用プログラム (ORCSKOHPLUS)
      *    設定項目
      *      医療機関ＩＤ
      *      県番号
      *      患者ＩＤ
      *      保険組合せ
      *      診療年月日
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSKOHPLUS-AREA.
      *
      *医療機関ＩＤ
           03  LNK-SKOHPLUS-HOSPNUM                 PIC 9(2).
      *県番号
           03  LNK-SKOHPLUS-PREFNUM                 PIC  X(02).
      *患者ＩＤ
           03  LNK-SKOHPLUS-PTID                    PIC  9(10).
      *保険組合せ
           03  LNK-SKOHPLUS-HKNCOMBI                PIC  9(04).
      *診療年月日
           03  LNK-SKOHPLUS-SRYYMD                  PIC  X(08).
      *返却項目
           03  LNK-SKOHPLUS-HENKYAKU-AREA.
      *負担区分
               05  LNK-SKOHPLUS-FTNKBN              PIC  X(01).
      *入院−食事療養区分
               05  LNK-SKOHPLUS-NYUSKJRYYKBN        PIC  X(01).
      *レセプト請求区分
               05  LNK-SKOHPLUS-RECESKYKBN          PIC  X(01).
      *
      *復帰コード
           03  LNK-SKOHPLUS-RC                      PIC S9(9) BINARY.
      *
