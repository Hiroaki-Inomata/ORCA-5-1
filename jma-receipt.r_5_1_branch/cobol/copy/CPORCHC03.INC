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
      *----------------------------------------------------------*
      *    請求書兼領収書出力パラメタ      COPY (CPORCHC03.INC)
      *                                          (ORCHC03 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  02/03/14  今回請求額を追加
      *  01.00.02    NACL-多々納  03/02/28  まとめ伝票番号を追加
      *  01.00.03    NACL-多々納  04/01/20  発行フラグ他追加
      *  01.00.04    NACL-多々納  04/07/15  複数科対応
      *  03.03.00    NACL-多々納  06/09/27  返金対応
      *  03.05.01    NACL-多々納  07/08/XX  訂正時合計対応
      *  04.00.00    NACL-太田    07/10/18  用紙サイズ追加
      ************************************************************
      *
       01  ORCHC03AREA.
      *
      *    伝票番号（必須）
           03  ORCHC03-DENPNUM          PIC 9(07).
      *
      *    今回請求額
           03  ORCHC03-SEIKYU           PIC S9(07).
      *    今回入金額
           03  ORCHC03-NYUKIN           PIC S9(07).
      *    今回請求−消費税（再掲）
           03  ORCHC03-SEIKYU-TAX-SAI   PIC S9(07).
      *
      *****複数科まとめ伝票番号
      *    03  ORCHC03-FUKU-DENPNUM     PIC 9(07).
      *    まとめ請求額
      *    03  ORCHC03-KON-SEIKYU       PIC S9(07).
      *    まとめ調整金
      *****03  ORCHC03-KON-CHOSEI       PIC S9(07).
      *
      *    発行フラグ（０：新規、１：再発行、２：訂正、３：入金
           03  ORCHC03-HAKKOFLG         PIC  9(01).
      *    前回未収額
           03  ORCHC03-ZENMISYU         PIC S9(07).
      *    前回返金額
           03  ORCHC03-HENKIN           PIC S9(07).
      *
      *    発行方法（1:科・保険別、2:保険別、3:科別、4:全体)
      *             (0:伝票番号分のみ)
           03  ORCHC03-HAKHOUFLG        PIC 9(01).
      *    訂正前の内容
           03  ORCHC03-OLD-AREA.
               05  ORCHC03-OLD-OCC      OCCURS   15.
      *            伝票番号
                   07  ORCHC03-OLD-DENPNUM       PIC  9(07).
      *            診療科
                   07  ORCHC03-OLD-SRYKA         PIC  X(02).
      *            保険組合せ
                   07  ORCHC03-OLD-HKNCOMBI      PIC  X(04).
      *            訂正前請求金額
                   07  ORCHC03-OLD-SKYMONEY      PIC S9(07).
      *            訂正前消費税（再掲）
                   07  ORCHC03-OLD-SKYMONEY-SAI  PIC S9(07).
      *            今回入金額
                   07  ORCHC03-OLD-NYUKIN-TOTAL  PIC S9(07).
      *
      *            今回返金額
                   07  ORCHC03-KON-HENKIN        PIC S9(07).
      *    今回の前回までの返金額
           03  ORCHC03-ZEN-HENKIN         PIC S9(07).
      *    前回未収額
           03  ORCHC03-ZENMISYU-2         PIC S9(07).
      *    前回過入金
           03  ORCHC03-KANYUKIN           PIC S9(07).
      *
      *    訂正合計表示
           03  ORCHC03-TEIKIN-KBN         PIC 9(01).
      *
      *    伝票番号枝番（収納からの再発行）
           03  ORCHC03-DENPEDANUM      PIC  9(02).
      *    パラメタテーブル使用フラグ
           03  ORCHC03-PARAFLG                 PIC 9(01).
      *    パラメタテーブルキー
           03  ORCHC03-PARA-KEY.
      *        業務ＩＤ
               05  ORCHC03-PARA-GYOUMUID       PIC X(03).
      *        端末ＩＤ
               05  ORCHC03-PARA-TERMID         PIC X(64).
      *        ファイル名
               05  ORCHC03-PARA-FILEMEI        PIC X(20).
      *    印刷ＤＢ出力区分(1:印刷ＤＢに出力）
           03  ORCHC03-PRTKBN                  PIC X(01).
      *    一括パラメタ
           03  ORCHC03-PRTAREA         PIC X(25000).
           03  ORCHC03-SIZE            PIC X(02).
      *
      *    依頼元業務
      *    0:入退院登録業務/診療行為業務
      *    1:収納業務（再発行）
      *    2:収納業務（月別請求書）　外来のみ
      *    3:収納業務（請求確認画面）
      *    4:収納業務（一括入金画面）
      *    5:月次統計業務（月別請求書）　外来のみ
      *    6:定期請求業務　入院のみ
      *    7:定期請求業務(請求確認画面)　入院のみ
      *    8:退院時仮計算　入院のみ
           03  ORCHC03-MOTOID           PIC 9(02).
      *    まとめ入返金区分
      *    (ORCSNYUKINの返却値をそのまま設定)
           03  ORCHC03-MATOME-NYUHENKIN-KBN.
               05  ORCHC03-MATOME-NYUKIN-KBN   PIC X(01).
               05  ORCHC03-MATOME-HENKIN-KBN   PIC X(01).
