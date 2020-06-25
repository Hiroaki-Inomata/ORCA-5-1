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
      *    データチェック用クエリ  COPY (CPQDTCHK.INC)           *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  QDTCHK-REC.
           03  QDTCHK-CD                   PIC X(170).
           03  QDTCHK-BYOMEI               PIC X(160).
           03  QDTCHK-COMPKBN              PIC X(1).
           03  QDTCHK-UTAGAIKBN            PIC X(1).
           03  QDTCHK-SRYKA                PIC X(2).
           03  QDTCHK-SRYYMD               PIC X(08).
           03  QDTCHK-PTBYOMEI             PIC X(160).
           03  QDTCHK-BYOMEICD-G.
               05  QDTCHK-BYOMEICD         PIC X(07)
                                           OCCURS  21.
           03  QDTCHK-UTAGAIFLG            PIC X(01).
           03  QDTCHK-NYUGAIKBN            PIC X(01).
           03  QDTCHK-TENKIKBN             PIC X(01).
           03  QDTCHK-TENKIYMD             PIC X(08).
           03  QDTCHK-HKNCOMBI             PIC 9(04).
           03  QDTCHK-REZEFLG              PIC X(01).
           03  QDTCHK-REZEMM               PIC 9(02).
           03  QDTCHK-ID                   PIC S9(9)   BINARY.
           03  QDTCHK-GRPID                PIC X(01).
           03  QDTCHK-PTHOSOKU             PIC X(40).
           03  QDTCHK-HOSOKU               PIC X(40).
           03  QDTCHK-HOSOKU-COMPKBN       PIC X(01).
           03  QDTCHK-KOHID                PIC 9(10).
      *
           03  QDTCHK-SELHOSPNUM           PIC 9(02).
           03  QDTCHK-SELPTID              PIC 9(10).
           03  QDTCHK-SELSRYKA             PIC X(02).
           03  QDTCHK-SELCHKKBN            PIC X(01).
           03  QDTCHK-SELSRYCD             PIC X(09).
           03  QDTCHK-SELYUKOSTYMD         PIC X(08).
           03  QDTCHK-SELYUKOEDYMD         PIC X(08).
           03  QDTCHK-SELCDKBN             PIC X(01).
