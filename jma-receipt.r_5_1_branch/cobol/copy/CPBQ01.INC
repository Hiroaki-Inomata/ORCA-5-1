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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.02    MCC-太田     02.07.09  基準日追加
      ************************************************************
       01  BQ01.
      *    処理リクエスト
           03  BQ01-REQUEST-G.
      *        PRINT 1: true
               05  BQ01-REQUEST-PRINT  PIC X(1).
      *        CSV   1: true
               05  BQ01-REQUEST-CSV    PIC X(1).
      *    ＣＳＶファイル名
           03  BQ01-CSVFILENAME        PIC X(100).
      *    表題
           03  BQ01-HYODAI             PIC X(100).
      *    条件１
           03  BQ01-JYOKEN1            PIC X(100).
      *    条件２
           03  BQ01-JYOKEN2            PIC X(100).
      *    開始ページ数
           03  BQ01-STPAGE             PIC 9(09).
      *    終了ページ数
           03  BQ01-EDPAGE             PIC 9(09).
      *    開始件数
           03  BQ01-STKNS              PIC 9(09).
      *    終了件数
           03  BQ01-EDKNS              PIC 9(09).
      *----(01.00.02) LINE ADD START ----------------------------------
      *    基準日
           03  BQ01-KJYNYMD            PIC 9(08).
      *----(01.00.02) LINE ADD END   ----------------------------------
      *    診療年月
           03  BQ01-SRYYM              PIC X(06).
      *    入外区分
           03  BQ01-NYUGAIKBN          PIC X(01).
      *    連携区分
           03  BQ01-LNKKBN             PIC X(01).
      *    TERMID(APIの場合はORCA-UID)
           03  BQ01-TERMID             PIC X(64).
           03  BQ01-SYSYMD             PIC X(08).
           03  BQ01-SYSTIME            PIC X(08).
           03  BQ01-STAFFCD            PIC X(05).
           03  BQ01-SHELLID            PIC X(07).
           03  BQ01-FILESVKBN          PIC X(01).
           03  BQ01-FILENAME           PIC X(100).
           03  BQ01-DIRNAME            PIC X(100).
           03  BQ01-KARTE-UID          PIC X(36).
           03  BQ01-OUTKBN             PIC X(01).
           03  BQ01-CSV-ROWS           PIC 9(10).
      *
      *    SQL
           03  BQ01-SQL                PIC X(15000).
      *    SQL ORDER
           03  BQ01-SQL-ORDER          PIC X(500).
      *    SPA-Q01KYOUTU
           03  BQ01-Q01KYOUTU          PIC X(20000).
