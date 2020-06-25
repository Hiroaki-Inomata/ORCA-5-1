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
      *    請求書兼領収書出力パラメタ      COPY (CPORCHCN03.INC)
      *                                          (ORCHCN03 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  02/03/14  今回請求額を追加
      *  02.07.00    NACL-太田    06/02/09  印刷区分を追加
      *  04.00.00    NACL-太田    07/10/18  用紙サイズを追加
      ************************************************************
      *
       01  ORCHCN03AREA.
           03  ORCHCN03-HEAD-HOSPNUM       PIC 9(2).
           03  ORCHCN03-STAFFCD            PIC X(05).
           03  ORCHCN03-SYSYMD             PIC X(08).
           03  ORCHCN03-TERMID             PIC X(64).
      *    請求書印刷方法フラグ
      *    0:収納データの発行方法に従う
      *    1:個別に発行
           03  ORCHCN03-SKYPRTFLG          PIC 9(01).
           03  ORCHCN03-CNT                PIC 9(10).
           03  ORCHCN03-OCC                OCCURS  3000.
      *        基準日
               05  ORCHCN03-KJNYMD         PIC  X(08).
      *        医療機関ＩＤ
               05  ORCHCN03-HOSPNUM        PIC 9(2).
      *        患者ＩＤ
               05  ORCHCN03-PTID           PIC  9(10).
      *        患者番号
               05  ORCHCN03-PTNUM          PIC  X(20).
      *        入外区分
               05  ORCHCN03-NYUGAIKBN      PIC X(01).
      *        伝票番号
               05  ORCHCN03-DENPNUM        PIC 9(07).
      *        今回請求−消費税（再掲）
               05  ORCHCN03-SEIKYU-TAX-SAI PIC S9(07).
      *        今回請求額
               05  ORCHCN03-SEIKYU         PIC S9(07).
      *        今回入金額
               05  ORCHCN03-NYUKIN         PIC S9(07).
      *        発行フラグ（０：新規、１：再発行、２：訂正
      *                    ３：収納請求確認画面から）
               05  ORCHCN03-HAKKOFLG       PIC 9(01).
      *        伝票番号枝番
               05  ORCHCN03-DENPEDANUM     PIC 9(02).
      *        今回請求未収額
               05  ORCHCN03-KONMISYU       PIC S9(07).
      *        診療年月日
               05  ORCHCN03-SRYYMD         PIC X(08).
      *        保険組合せ
               05  ORCHCN03-HKNCOMBI       PIC X(04).
      *
           03  ORCHCN03-PRTKBN             PIC X(1).
           03  ORCHCN03-PRTAREA            PIC X(25000).
           03  ORCHCN03-OUTCNT             PIC 9(10).
           03  ORCHCN03-SIZE               PIC X(02).
      *
      *    依頼元業務
      *    0:入退院登録業務
      *    1:収納業務（再発行）
      *    2:収納業務（月別請求書）　外来のみ
      *    3:収納業務（請求確認画面）
      *    4:収納業務（一括入金画面）
      *    5:月次統計業務（月別請求書）　外来のみ
      *    6:定期請求業務　入院のみ
      *    7:定期請求業務(請求確認画面)　入院のみ
      *    8:退院時仮計算　入院のみ
           03  ORCHCN03-MOTOID           PIC 9(02).
      *    概算書発行フラグ
           03  ORCHCN03-GAISAN-FLG       PIC 9(01).
