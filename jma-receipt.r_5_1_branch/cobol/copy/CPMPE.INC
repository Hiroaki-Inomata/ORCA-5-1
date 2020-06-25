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
      ************************************************************
      *    monpushevent                  COPY (CPMPE.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
      * 01  MPE-REC.
            03  MPE-UUID                   PIC X(37).
            03  MPE-ID                     PIC S9(9)   BINARY.
            03  MPE-EVENT                  PIC X(64).
            03  MPE-USER                   PIC X(32).
            03  MPE-PUSHED-AT.
              04  MPE-PUSHED-AT-DATE       PIC X(10).
              04  MPE-PUSHED-AT-FL1        PIC X(01).
              04  MPE-PUSHED-AT-TIME       PIC X(08).
            03  MPE-DATA                   PIC X(4096).
      * 以下virtual項目
            03  MPE-PUSHED-FROM            PIC X(32).
            03  MPE-PUSHED-TO              PIC X(32).
