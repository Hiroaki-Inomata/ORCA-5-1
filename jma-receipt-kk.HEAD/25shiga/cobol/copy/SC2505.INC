      *******************************************************************
      * Project code name "ORCA"
      * ϊγWZvg\tgiJMA standard receipt softwarej
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
        01  SC05.
      *γΓ@ΦR[h
            02  SC05-HOSPCD-TBL.
              03  SC05-HOSPCD PIC X(1) OCCURS  10 TIMES.
      *γΓ@Φ|ΌΜ
            02  SC05-HOSPNAME   PIC X(100).
            02  SC05-KAISETU    PIC X(40).
      *tNϊ
            02  SC05-SOUYY  PIC X(8).
            02  SC05-SOUMM  PIC X(4).
            02  SC05-SOUDD  PIC X(4).
      *fΓN
            02  SC05-SRYYM  PIC X(16).
      *v
            02  SC05-GKENSU PIC X(7).
      *Ώ_v
            02  SC05-GTENSU PIC X(11).
      *γΓοͺ_v
            02  SC05-GFUKUSHITEN    PIC X(11).
      *κSΰv
            02  SC05-GITBFTN    PIC X(11).
      *γΓοΙWικSΰv
            02  SC05-GFITBFTN   PIC X(11).
