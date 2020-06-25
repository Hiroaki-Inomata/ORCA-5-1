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
      *         ジョブログ管理    COPY   (CPJOBLOG.INC)          *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *  01  TBL-JOBLOG.
            02  JOBLOG-HOSPNUM         PIC 9(2).
            02  JOBLOG-LOGYMD          PIC X(8).
            02  JOBLOG-LOGTIME         PIC X(6).
            02  JOBLOG-PROGNAME        PIC X(20).
            02  JOBLOG-KBNID           PIC X(1).
            02  JOBLOG-OBJECTYYM       PIC X(6).
            02  JOBLOG-RESULT          PIC X(1).
            02  JOBLOG-ERRCD           PIC X(4).
            02  JOBLOG-MSG             PIC X(200).
