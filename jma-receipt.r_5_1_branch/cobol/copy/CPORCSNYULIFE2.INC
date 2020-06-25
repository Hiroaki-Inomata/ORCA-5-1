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
      *    入院時生活療養費負担額取得パラメタ　　COPY (CPORCSNYULIFE2.INC)
      *                                使用プログラム (ORCSNYULIFE2)
      *    平成２８年４月改定用
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
       01  ORCSNYULIFE2-AREA.
      *
      *医療機関ＩＤ
           03  LNK-NYULIFE2-HOSPNUM                    PIC 9(2).
      *患者ＩＤ
           03  LNK-NYULIFE2-PTID                       PIC  9(10).
      *算定年月
           03  LNK-NYULIFE2-SANTEIYM                   PIC  X(06).
      *返却項目
           03  LNK-NYULIFE2-HENKYAKU-AREA.
      *入院時生活療養費返却テーブル
               05  LNK-NYULIFE2-HENKYAKU-TABLE         OCCURS  31.
      *入院時生活療養費対象区分
      *  （空白：対象外　１：対象）
                 06  LNK-NYULIFE2-KBN                  PIC  X(01).
      *入院時食事療養費区分
      *  （１：食事療養費１　２：食事療養費２）
                 06  LNK-NYULIFE2-SKJKBN               PIC  X(01).
      *入院時生活療養費負担軽減区分１（所得の状況より軽減）
      *  （空白：軽減無し　１：低所得２　２：低所得２−９０日越
      *    ３：低所得１　４：低所得１−老齢福祉年金受給者）
                 06  LNK-NYULIFE2-FTNKBN1              PIC  X(01).
      *入院時生活療養費負担軽減区分２（病状の程度等より軽減）
      *  （空白：軽減無し　１：軽減有り）
                 06  LNK-NYULIFE2-FTNKBN2              PIC  X(01).
      *食事時間（添字１：朝食　添字２：昼食　添字３：夕食）
                 06  LNK-NYULIFE2-HENKYAKU-TIME        OCCURS  3.
      *入院時生活療養費−食事−診療コード
                   07  LNK-NYULIFE2-SKJ-SRYCD          PIC  X(09).
      *入院時生活療養費−食事−金額
                   07  LNK-NYULIFE2-SKJ-MONEY          PIC  9(07).
      *入院時生活療養費−食事負担−診療コード
                   07  LNK-NYULIFE2-SKJFTN-SRYCD       PIC  X(09).
      *入院時生活療養費−食事負担−金額
                   07  LNK-NYULIFE2-SKJFTN-MONEY       PIC  9(07).
      *入院時生活療養費−環境療養−診療コード
                 06  LNK-NYULIFE2-KAN-SRYCD            PIC  X(09).
      *入院時生活療養費−環境療養−金額
                 06  LNK-NYULIFE2-KAN-MONEY            PIC  9(07).
      *入院時生活療養費−環境療養負担−診療コード
                 06  LNK-NYULIFE2-KANFTN-SRYCD         PIC  X(09).
      *入院時生活療養費−環境療養負担−金額
                 06  LNK-NYULIFE2-KANFTN-MONEY         PIC  9(07).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院会計なし　４：入院時生活療養対象外）
           03  LNK-NYULIFE2-RC                     PIC S9(9) BINARY.
      *
