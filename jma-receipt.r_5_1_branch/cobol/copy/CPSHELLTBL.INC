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
        01  SHELLTBL.
            02  SHELLTBL-NAME   PIC X(200).
            02  SHELLTBL-ID     PIC X(64).
            02  SHELLTBL-COMMENT PIC X(200).
            02  SHELLTBL-GROUPNAME     PIC X(64).
            02  SHELLTBL-GROUPNAME-R REDEFINES SHELLTBL-GROUPNAME.
                03  SHELLTBL-HOSPNUM   PIC  9(02).
                03  SHELLTBL-GROUPNAME-FILLER
                                       PIC  X(62).
            02  SHELLTBL-EXTRA   PIC X(512).
            02  SHELLTBL-EXTRA-R REDEFINES SHELLTBL-EXTRA.
                03  SHELLTBL-JOBID          PIC  9(07).
                03  SHELLTBL-SHELLID        PIC  X(08).
                03  SHELLTBL-RUN-OPID       PIC  X(16).
                03  SHELLTBL-LOG-FILEID     PIC  X(08).
                03  SHELLTBL-TERMID         PIC  X(64).
                03  SHELLTBL-EXTRA-VALUE    PIC  X(409).
            02  SHELLTBL-REPOS-NAME         PIC  X(256).
            02  SHELLTBL-PGID   PIC S9(09) BINARY.
            02  SHELLTBL-STARTTIME  PIC X(32).
            02  SHELLTBL-ARG1   PIC X(150).
            02  SHELLTBL-ARG2   PIC X(150).
            02  SHELLTBL-ARG3   PIC X(150).
            02  SHELLTBL-ARG4   PIC X(150).
            02  SHELLTBL-ARG5   PIC X(150).
            02  SHELLTBL-ARG6   PIC X(150).
            02  SHELLTBL-ARG7   PIC X(150).
            02  SHELLTBL-ARG8   PIC X(150).
            02  SHELLTBL-ARG9   PIC X(150).
            02  SHELLTBL-ARG10  PIC X(150).
            02  SHELLTBL-ARG11  PIC X(150).
            02  SHELLTBL-ARG12  PIC X(150).
            02  SHELLTBL-ARG13  PIC X(150).
            02  SHELLTBL-ARG14  PIC X(150).
            02  SHELLTBL-ARG15  PIC X(150).
            02  SHELLTBL-ARG16  PIC X(150).
            02  SHELLTBL-ARG17  PIC X(150).
            02  SHELLTBL-ARG18  PIC X(150).
            02  SHELLTBL-ARG19  PIC X(150).
            02  SHELLTBL-ARG20  PIC X(150).
            02  SHELLTBL-ARG21  PIC X(100).
            02  SHELLTBL-ARG22  PIC X(100).
            02  SHELLTBL-ARG23  PIC X(100).
            02  SHELLTBL-ARG24  PIC X(100).
            02  SHELLTBL-ARG25  PIC X(100).
            02  SHELLTBL-ARG26  PIC X(100).
            02  SHELLTBL-ARG27  PIC X(100).
            02  SHELLTBL-ARG28  PIC X(100).
            02  SHELLTBL-ARG29  PIC X(100).
            02  SHELLTBL-ARG30  PIC X(100).
            02  SHELLTBL-ARG31  PIC X(100).
            02  SHELLTBL-ARG32  PIC X(100).
            02  SHELLTBL-ARG33  PIC X(100).
            02  SHELLTBL-ARG34  PIC X(100).
            02  SHELLTBL-ARG35  PIC X(100).
            02  SHELLTBL-ARG36  PIC X(100).
            02  SHELLTBL-ARG37  PIC X(100).
            02  SHELLTBL-ARG38  PIC X(100).
            02  SHELLTBL-ARG39  PIC X(100).
            02  SHELLTBL-ARG40  PIC X(100).
