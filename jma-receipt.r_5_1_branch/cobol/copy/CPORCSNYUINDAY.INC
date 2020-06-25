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
      *    入院料未算定日取得パラメタ　COPY (CPORCSNYUINDAY.INC)
      *                                使用プログラム (ORORCSNYUINDAY)
      *    設定項目
      *      医療機関番号
      *      患者ＩＤ
      *      算定年月
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNYUINDAY-AREA.
      *
      *医療機関番号
           03  LNK-NYUINDAY-HOSPNUM           PIC 9(2).
      *患者ＩＤ
           03  LNK-NYUINDAY-PTID              PIC  9(10).
      *算定年月
           03  LNK-NYUINDAY-SANTEIYM          PIC  X(06).
      *返却項目
           03  LNK-NYUINDAY-HENKYAKU-AREA.
      *入院区分
             05  LNK-NYUINDAY-TBL.
      *  （空白：対象外　１：入院中で入院料未算定日）
               07  LNK-NYUINDAY-KBN           PIC  X(01)  OCCURS  31.
      *
      *短期滞在手術基本料２（又は３）入院区分
             05  LNK-NYUINDAY-TANKI2-TBL.
      *  （空白：対象外
      *    　１：短期滞在手術基本料２（又は３）（生活療養含む）の入院対象日
               07  LNK-NYUINDAY-TANKI-KBN    PIC  X(01)  OCCURS  31.
      *
      *  （空白：対象外
      *    　１：短期滞在手術基本料２（又は３）で入院料未算定日
      *      ２：短期滞在手術基本料２（又は３）−生活療養で入院料未算定日）
               07  LNK-NYUINDAY-TANKI2-KBN    PIC  X(01)  OCCURS  31.
      *
      *平成２６年改定で追加になった短期滞在手術基本料３入院区分
      *  （空白：対象外
      *    　１：短期滞在手術基本料３で入院料未算定日
      *      ２：短期滞在手術基本料３−生活療養で入院料未算定日）
               07  LNK-NYUINDAY-TANKI3-KBN    PIC  X(01)  OCCURS  31.
      *
      *救急搬送診療料算定区分
             05  LNK-NYUINDAY-KYUKYU-TBL.
      *  （空白：未算定 　１：算定）
               07  LNK-NYUINDAY-KYUKYU-KBN    PIC  X(01)  OCCURS  31.
      *
      *同日再入院判定用
             05  LNK-NYUINDAY-DOUJITSU-TBL.
      *        同日再入院区分（空白：同日再入院以外 　１：同日再入院）
               07  LNK-NYUINDAY-DOUJITSU-KBN  PIC  X(01)  OCCURS  31.
      *        退院再入院の保険変動区分
      *        （１：医保から医保　２：医保から医保以外　
      *          ３：医保以外から医保　４：医保以外から医保以外）
               07  LNK-NYUINDAY-DOUJITSU-HKNHENKBN PIC X(01) OCCURS 31.
      *
      *        同日再入院（入院分）の保険組合せ
               07  LNK-NYUINDAY-DOUJITSU-HKNCOMBI PIC 9(04) OCCURS  31.
      *        同日再入院（入院分）の保険種類
      *        （１：社保　２：国保　３：後期高齢　４：労災　５：自賠
      *          ６：公害　７：自費　８：治験）
               07  LNK-NYUINDAY-DOUJITSU-HKNSYU   PIC X(01) OCCURS  31.
      *        同日再入院（入院分）の食事開始時間区分
      *       （０：食なし／１：朝から／２：昼から／３：夕から）
               07  LNK-NYUINDAY-DOUJITSU-SKJKBN PIC X(01) OCCURS  31.
      *
      *        同日再入院（退院分）の保険組合せ
               07  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI PIC 9(04) OCCURS 31.
      *        同日再入院（退院分）の保険種類
      *        （１：社保　２：国保　３：後期高齢　４：労災　５：自賠
      *          ６：公害　７：自費　８：治験）
               07  LNK-NYUINDAY-DOUJITSU-T-HKNSYU  PIC X(01) OCCURS  31.
      *        同日再入院（退院分）の食事終了時間区分
      *       （０：食なし／１：朝まで／２：昼まで／３：夕まで）
               07  LNK-NYUINDAY-DOUJITSU-T-SKJKBN PIC X(01) OCCURS  31.
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ３：入院会計なし　４：入院区分設定なし）
           03  LNK-NYUINDAY-RC                PIC S9(9) BINARY.
      *
