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
      *     カルテ３号様式出力パラメタ    COPY (CPORCHC33.INC)
      *                                          (ORCHC33 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  04/12/1X  印刷区分等追加
      ************************************************************
      *
       01  ORCHC33AREA.
      *
      *    処理区分（１：オンライン、２：一括）
           03  ORCHC33-KBN             PIC X(01).
      *
      *    患者番号
           03  ORCHC33-PTNUM           PIC X(20).
      *    患者ＩＤ
           03  ORCHC33-PTID            PIC 9(10).
      *    患者氏名
           03  ORCHC33-NAME            PIC X(50).
      *    患者性別
           03  ORCHC33-SEX             PIC X(02).
      *    患者生年月日
           03  ORCHC33-BIRTHDAY        PIC X(08).
      *    診療年月
           03  ORCHC33-SRYYM           PIC X(06).
      *    診療年月（終了）
           03  ORCHC33-SRYYM-END       PIC X(06).
      *    入外区分
           03  ORCHC33-NYUGAIKBN       PIC X(01).
      *    診療科
           03  ORCHC33-SRYKA           PIC X(02).
      *    保険組合せ
           03  ORCHC33-HKNCOMBI        PIC X(04).
      *
      *    印刷区分（1:頭書、0:明細)
           03  ORCHC33-PRINT-FLG       PIC 9(01).
      *    印刷開始日
           03  ORCHC33-STRDD           PIC 9(02).
      *    最終印刷頁
           03  ORCHC33-ENDPAGE         PIC 9(03).
      *    最終印刷行
           03  ORCHC33-ENDLINE         PIC 9(02).
      *    印刷形態
           03  ORCHC33-KARUTE3KBN      PIC X(01).
      *
      *    一括パラメタ
           03  ORCHC33-PRTAREA         PIC X(25000).
      *
      *出力領域
      *    ページ枚数
           03  ORCHC33-PAGE            PIC 9(04).
      *
      *    エラーコード
           03  ORCHC33-RC              PIC 9(02).
      *
