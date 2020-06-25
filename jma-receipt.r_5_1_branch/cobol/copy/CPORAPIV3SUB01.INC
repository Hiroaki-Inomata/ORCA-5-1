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
      *****************************************************************
      ******************************************************************
      *    ＡＰＩＶ３　処理終了サブパラメタ
      *                                         (CPORAPIV3SUB01.INC)
      ******************************************************************
       01  ORAPIV3SUB01AREA.
      *    9:強制終了
           03  APIV3SUB01-SYORI             PIC X(01).
      *    TERMID（API+オルカＵＩＤ）
           03  APIV3SUB01-TERMID            PIC X(64).
      *    カルテＵＩＤ
           03  APIV3SUB01-KARTE-UID         PIC X(36).
      *    患者番号
           03  APIV3SUB01-PTNUM             PIC X(20).
      *    API業務ID
           03  APIV3SUB01-API-GYOUMUID      PIC X(08).
           03  APIV3SUB01-API-FILEMEI       PIC X(20).
      *    業務ID(排他制御）
           03  APIV3SUB01-PARA-GYOUMUID     PIC X(04).
      *    業務ID（予備）
           03  APIV3SUB01-PARA-GYOUMUID2    PIC X(04).
      *
      *返却
           03  APIV3SUB01-ERRCD             PIC X(02).
           03  APIV3SUB01-ERRMSG            PIC X(200).
      *
