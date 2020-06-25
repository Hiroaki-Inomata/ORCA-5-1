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
      *    入院時生活療養費負担額取得パラメタ　　COPY (CPORCSNYULIFE.INC)
      *                                使用プログラム (ORCSNYULIFE)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      *
      *    ※返却項目の食事に係る金額は１食分の値を返却
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNYULIFE-AREA.
      *
      *医療機関ＩＤ
           03  LNK-NYULIFE-HOSPNUM                    PIC 9(2).
      *患者ＩＤ
           03  LNK-NYULIFE-PTID                       PIC  9(10).
      *算定年月
           03  LNK-NYULIFE-SANTEIYM                   PIC  X(06).
      *返却項目
           03  LNK-NYULIFE-HENKYAKU-AREA.
      *入院時生活療養費返却テーブル
               05  LNK-NYULIFE-HENKYAKU-TABLE         OCCURS  31.
      *入院時生活療養費対象区分
      *  （空白：対象外　１：対象）
                   07  LNK-NYULIFE-KBN                PIC  X(01).
      *入院時食事療養費区分
      *  （１：食事療養費１　２：食事療養費２）
                   07  LNK-NYULIFE-SKJKBN             PIC  X(01).
      *入院時生活療養費負担軽減区分１（所得の状況より軽減）
      *  （空白：軽減無し　１：低所得２　２：低所得２−９０日越
      *    ３：低所得１　４：低所得１−老齢福祉年金受給者）
                   07  LNK-NYULIFE-FTNKBN1            PIC  X(01).
      *入院時生活療養費負担軽減区分２（病状の程度等より軽減）
      *  （空白：軽減無し　１：軽減有り）
                   07  LNK-NYULIFE-FTNKBN2            PIC  X(01).
      *入院時生活療養費−食事−診療コード
                   07  LNK-NYULIFE-SKJ-SRYCD          PIC  X(09).
      *入院時生活療養費−食事−金額
                   07  LNK-NYULIFE-SKJ-MONEY          PIC  9(07).
      *入院時生活療養費−環境療養−診療コード
                   07  LNK-NYULIFE-KAN-SRYCD          PIC  X(09).
      *入院時生活療養費−環境療養−金額
                   07  LNK-NYULIFE-KAN-MONEY          PIC  9(07).
      *入院時生活療養費−食事負担−診療コード
                   07  LNK-NYULIFE-SKJFTN-SRYCD       PIC  X(09).
      *入院時生活療養費−食事負担−金額
                   07  LNK-NYULIFE-SKJFTN-MONEY       PIC  9(07).
      *入院時生活療養費−環境療養負担−診療コード
                   07  LNK-NYULIFE-KANFTN-SRYCD       PIC  X(09).
      *入院時生活療養費−環境療養負担−金額
                   07  LNK-NYULIFE-KANFTN-MONEY       PIC  9(07).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院会計なし　４：入院時生活療養対象外）
           03  LNK-NYULIFE-RC                     PIC S9(9) BINARY.
      *
