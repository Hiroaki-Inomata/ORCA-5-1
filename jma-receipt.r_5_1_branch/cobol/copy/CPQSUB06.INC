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
       01  QSUB06-LNK.
           03  QSUB06-SHELLNAME                PIC X(32).
      *        0: FAULE, 1: TRUE
           03  QSUB06-REQUEST-G.
               05  QSUB06-REQUEST-PRINT        PIC X(01).
               05  QSUB06-REQUEST-CSV          PIC X(01).
           03  QSUB06-SELHMS                   PIC X(06).
           03  QSUB06-SHELLID                  PIC X(08).
           03  QSUB06-JOBID                    PIC 9(07).
           03  QSUB06-KARTE-UID                PIC X(36).
           03  QSUB06-ORCA-UID                 PIC X(36).
      *    1: クライアント保存
           03  QSUB06-FILESVKBN                PIC X(01).
      *    1: "EUC-JP", 2: "S-JIS", 3: "UTF-8"
           03  QSUB06-ENCODEKBN                PIC X(01).
           03  QSUB06-DIRNAME                  PIC X(100).
           03  QSUB06-FILENAME                 PIC X(100).
      *    出力結果
      *    1:  基本, 2: 点数, 3:診療行為
           03  QSUB06-OUTKBN                   PIC X(01).
           03  QSUB06-DATA-ID                  PIC X(37).
           03  QSUB06-PRINT-ID                 PIC X(37).
           03  QSUB06-TBL-KEY                  PIC X(08).
      *    ＣＳＶファイル名
           03  QSUB06-CSVFILENAME              PIC X(100).
      *    ＰＤＦファイル名
           03  QSUB06-PDFFILENAME              PIC X(100).
      *    表題
           03  QSUB06-HYODAI                   PIC X(100).
      *    条件１
           03  QSUB06-JYOKEN1                  PIC X(100).
      *    条件２
           03  QSUB06-JYOKEN2                  PIC X(100).
      *    開始ページ数
           03  QSUB06-STPAGE                   PIC 9(09).
      *    終了ページ数
           03  QSUB06-EDPAGE                   PIC 9(09).
      *    開始件数
           03  QSUB06-STKNS                    PIC 9(09).
      *    終了件数
           03  QSUB06-EDKNS                    PIC 9(09).
      *    基準日
           03  QSUB06-KJYNYMD                  PIC 9(08).
      *    診療年月
           03  QSUB06-SRYYM                    PIC X(06).
      *    入外区分
           03  QSUB06-NYUGAIKBN                PIC X(01).
      *    連携区分
           03  QSUB06-LNKKBN                   PIC X(01).
      *    SQL(ORDER)
           03  QSUB06-SQL                      PIC X(500).
