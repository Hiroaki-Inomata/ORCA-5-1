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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ剤診療内容編集サブパラメタ（一体化）
      *                                         (CPORAPI021SUB3V3.INC)
      ******************************************************************
       01  ORAPI021SUB3V3AREA.
      *    処理区分
           03  ORAPISUB3-SYORIKBN      PIC X(01).
      *
      *    テーブル件数
           03  ORAPISUB3-ZAI-MAX       PIC 9(04).
      *
      *SPA返却
           03  ORAPISUB3-HENSYU-AREA.
               05  ORAPISUB3-HENSYU-OCC        OCCURS   400.
      *        診療会計内容
                   07  ORAPISUB3-ZAINUM        PIC 9(08).
                   07  ORAPISUB3-SRYKA         PIC X(02).
                   07  ORAPISUB3-SRYKA-NAME    PIC X(20).
                   07  ORAPISUB3-HKNCOMBI      PIC X(04).
      *
      *            保険組合せ状態（1:削除、2:期間外）
                   07  ORAPISUB3-COMBINATION-MODE   PIC X(01).
      *
                   07  ORAPISUB3-SRYKBN        PIC X(02).
                   07  ORAPISUB3-SRYSYUKBN     PIC X(03).
      *            包括診療分
                   07  ORAPISUB3-JIHOKBN       PIC X(01).
      *            剤点数
                   07  ORAPISUB3-ZAITEN        PIC 9(08).
      *            マイナス区分
                   07  ORAPISUB3-PLUSKBN       PIC 9(01).
                   07  ORAPISUB3-ZAIKBN        PIC 9(03).
      *            自費金額
                   07  ORAPISUB3-JIHIMONEYTOTAL    PIC  9(07).
      *            剤回数
                   07  ORAPISUB3-NAI-DAY-AREA.
                       09  ORAPISUB3-NAI-DAY-TBL   OCCURS   10.
                           11   ORAPISUB3-NAI-DAY  PIC  9(03)
                                                       OCCURS   31.
      *            入院調剤料自動算定剤分
                   07  ORAPISUB3-CHOUZAIAUTO   PIC X(01).
      *        API返却
      *            剤点数
                   07  ORAPISUB3-MDC-POINT         PIC X(08).
      *            剤金額
                   07  ORAPISUB3-MDC-MONEY         PIC X(08).
      *            剤区分（１：包括剤）
                   07  ORAPISUB3-MDC-CODE          PIC X(01).
      *            包括検査の検査数
                   07  ORAPISUB3-HKTKNSA-CNT       PIC X(02).
      *            診療種別名称
                   07  ORAPISUB3-MDC-CLASS-NAME    PIC X(40).
      *            保険組合せ名称
                   07  ORAPISUB3-MAIN-INS-NAME     PIC X(20).
                   07  ORAPISUB3-PUBLIC-INS-INFO.
      *            公費の種類名称
                       09  ORAPISUB3-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *
      *            診療内容
                   07  ORAPISUB3-SRYACT-AREA.
                       09  ORAPISUB3-SRYACT-TBL   OCCURS   50.
                           11   ORAPISUB3-SRYCD        PIC X(09).
                           11   ORAPISUB3-SRYSURYO     PIC 9(05)V9(05).
                           11   ORAPISUB3-SRYKAISU     PIC 9(03).
                           11   ORAPISUB3-MEISKYFLG    PIC X(01).
                           11   ORAPISUB3-AUTOKBN      PIC X(01).
                           11   ORAPISUB3-INPUTNUM     PIC 9(03).
      *                    自費金額
                           11   ORAPISUB3-JIHIMONEY    PIC 9(07).
      *                    換算数量入力
                           11   ORAPISUB3-KANSURYO     PIC 9(05)V9(05).
      *
      *        API返却
      *                    診療名称
                           11  ORAPISUB3-PRSCRPT-NAME      PIC X(140).
      *                    数量
                           11  ORAPISUB3-PRSCRPT-NUMBER    PIC X(11).
      *                    単位コード
                           11  ORAPISUB3-PRSCRPT-TANICD    PIC X(03).
      *                    単位名称
                           11  ORAPISUB3-PRSCRPT-TANICD-NAME PIC X(24).
      *                    コメント埋め込み値
                           11  ORAPISUB3-PRSCRPT-ATAI-G.
                               13  ORAPISUB3-PRSCRPT-ATAI  PIC X(10)
                                                         OCCURS   5.
      *                    自費金額
                           11  ORAPISUB3-PRSCRPT-MONEY     PIC X(07).
      *                    フィルム分画数
                           11  ORAPISUB3-PRSCRPT-FILMNUM   PIC X(03).
      *                    継続指示区分
                           11  ORAPISUB3-PRSCRPT-CONTKBN   PIC X(01).
      *                    自動区分（返却用）
                           11  ORAPISUB3-PRSCRPT-AUTOKBN   PIC X(01).
      *
      *    エラー区分
           03  ORAPISUB3-ERRCD         PIC X(04).
           03  ORAPISUB3-ERRMSG        PIC X(100).
      *
