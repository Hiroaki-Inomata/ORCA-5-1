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
      *         チェック例外マスタ        COPY (CPCHKREIGAI.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CHKREIGAI-REC.
      *チェック番号
           03  CHKREIGAI-KEY.
               05  CHKREIGAI-HOSPNUM       PIC 9(02).
      *患者ＩＤ
               05  CHKREIGAI-PTID          PIC 9(10).
      *連番
               05  CHKREIGAI-RENNUM        PIC 9(08).
      *確認項目番号
      *    01:保険者番号
      *    02:被保険者記号・番号
      *    03:公費負担者番号・受給者番号
      *    04:保険適応年齢
      *    05:患者病名
      *    06:診療開始日
      *    07:疑い病名・急性病名
      *    08:薬剤と病名
      *    09:診療行為と病名
      *    10:薬剤と併用禁忌
      *    11:診療行為の併用算定
      *    12:実日数
      *    13:患者氏名
      *    14:保険組合せ有効期間
      *    15:投与禁忌薬剤と病名
      *    16:診療行為どうしの算定漏れ
      *    17:病名と薬剤
      *    18:病名と診療行為
      *    19:薬剤の月上限回数・月投与量
      *    20:診療行為算定回数
      *    21:選択式コメント
      *    99:その他
      *    A1:未登録薬剤（薬剤と病名）
           03  CHKREIGAI-CHKNUM            PIC X(02).
      *入外区分
           03  CHKREIGAI-NYUGAIKBN         PIC X(01).
      *チェック区分
           03  CHKREIGAI-CHKKBN            PIC X(01).
      *診療コード
           03  CHKREIGAI-SRYCD             PIC X(09).
      *病名
           03  CHKREIGAI-BYOMEI            PIC X(160).
      *条件区分
           03  CHKREIGAI-COMPKBN           PIC X(1).
      *適用コード
           03  CHKREIGAI-TEKCD             PIC X(9).
      *適応病名
           03  CHKREIGAI-TEKBYOMEI         PIC X(160).
      *適応病名条件区分
           03  CHKREIGAI-TEKCOMPKBN        PIC X(1).
      *単位コード
           03  CHKREIGAI-TANICD            PIC 9(3).
      *単位名称
           03  CHKREIGAI-TANINAME          PIC X(12).
      *エラーコード
           03  CHKREIGAI-ERRCD             PIC X(06).
      *エラーメッセージ
           03  CHKREIGAI-ERRMSG            PIC X(200).
      *開始有効年月日
           03  CHKREIGAI-STYUKYMD          PIC X(08).
      *終了有効年月日
           03  CHKREIGAI-EDYUKYMD          PIC X(08).
      *端末ＩＤ
           03  CHKREIGAI-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  CHKREIGAI-OPID              PIC X(16).
      *作成年月日
           03  CHKREIGAI-CREYMD            PIC X(08).
      *更新年月日
           03  CHKREIGAI-UPYMD             PIC X(08).
      *更新時間
           03  CHKREIGAI-UPHMS             PIC X(06).
           03  CHKREIGAI-SELLIMIT          PIC 9(08).
           03  CHKREIGAI-SELOFFSET         PIC 9(08).
