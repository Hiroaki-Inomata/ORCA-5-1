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
      *************************************************************
      *    チェックデジット　
      *************************************************************
      *************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *************************************************************
       01  LNK-CHKDGT-AREA.
           03  LNK-IN.
               05  LNK-I-SYORIKBN      PIC 9(01).
               05  LNK-I-CODE          PIC X(20).
           03  LNK-OUT.
               05  LNK-O-CODE          PIC X(20).
               05  LNK-O-CHKDGT        PIC 9(01).
               05  LNK-O-ERRCD         PIC 9(02).
