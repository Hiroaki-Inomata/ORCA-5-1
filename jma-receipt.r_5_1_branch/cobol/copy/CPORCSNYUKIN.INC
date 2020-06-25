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
      *    まとめ入金編集パラメタ                                 *
      *                         COPY  (CPORCSNYUKIN.INC)          *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNYUKINAREA.
      *入力値
      *    医療機関ＩＤ（必須）
           03  LNK-SCNYUKIN-HOSPNUM          PIC 9(2).
      *    患者ＩＤ（必須）
           03  LNK-SCNYUKIN-PTID             PIC 9(10).
      *    入外区分
      *      １：入院
      *      ２：外来
           03  LNK-SCNYUKIN-NYUGAIKBN        PIC X(01).
      *    処理区分（必須）
      *      １：請求確認画面より処理
      *      ２：未収まとめ入金画面より処理
           03  LNK-SCNYUKIN-SYORIKBN         PIC X(01).
      *    更新区分（必須）
      *      １：未収仮処理（収納実更新なし）
      *      ２：未収更新
           03  LNK-SCNYUKIN-KOUSHINKBN       PIC X(01).
      *    返金区分
      *      １：返金額からの充当をしない
           03  LNK-SCNYUKIN-HENKINKBN        PIC X(01).
      *    入金管理区分
           03  LNK-SCNYUKIN-NYKNKNRKBN       PIC X(01).
      *    入金額
           03  LNK-SCNYUKIN-NYUKIN-MONEY     PIC 9(08).
      *    返金額
           03  LNK-SCNYUKIN-HENKIN-MONEY     PIC 9(08).
      *    入金日
           03  LNK-SCNYUKIN-NYUKIN-YMD       PIC X(08).
      *    入金方法
           03  LNK-SCNYUKIN-NYUKIN-HOHO      PIC X(02).
      *出力値
           03  LNK-SCNYUKIN-OUT-AREA.
      *
      *        まとめ入金区分
               05  LNK-SCNYUKIN-MATOME-NYUHENKIN-KBN.
      *            空白：入金なし1:入金あり
                   07  LNK-SCNYUKIN-MATOME-NYUKIN-KBN  PIC X(01).
      *            空白：返金なし1:返金あり
                   07  LNK-SCNYUKIN-MATOME-HENKIN-KBN  PIC X(01).
      *        入金振り分け情報
               05  LNK-SCNYUKIN-FURIWAKE-INF   OCCURS  100.
      *            伝票番号
                   07  LNK-SCNYUKIN-DENPNUM    PIC 9(07).
      *            請求額
                   07  LNK-SCNYUKIN-SKYMONEY   PIC 9(07).
      *            入金振り分け額
                   07  LNK-SCNYUKIN-FURIWAKEMONEY  PIC 9(07)
                                                   OCCURS 2.
      *        返金振り分け残額
               05  LNK-SCNYUKIN-HENKINZAN      PIC 9(07).
      *        入金振り分け残額
               05  LNK-SCNYUKIN-FURIWAKEZAN    PIC 9(07).
      *復帰コード
      *    00:正常
      *    10:パラメータ不正
      *    11:入金振り分け対象データなし
      *    12:入金振り分け対象データ１００件以上の為、処理不可
      *    13:収納更新エラー
      *    14:収納明細インサートエラー
           03  LNK-SCNYUKIN-RC              PIC 9(02).
      *
      *
