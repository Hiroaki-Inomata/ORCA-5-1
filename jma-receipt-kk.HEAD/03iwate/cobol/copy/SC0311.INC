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
       01  SC11.
      *ζͺ
           03  SC11-KBN                 PIC X(02)  OCCURS 3.
      *fΓN
           03  SC11-SRYYM               PIC X(16).
      *ΒΚR[h
           03  SC11-TENHYOKBN           PIC X(02)  OCCURS 4.
      *γΓ@ΦR[h
           03  SC11-HOSPCD              PIC X(20).
      *s¬ΊΌ
           03  SC11-CITYNAME            PIC X(10).
      *s¬ΊΤ
           03  SC11-CITYNUM-TBL.
               05  SC11-CITYNUM         PIC X(02)  OCCURS 2.
      *γΓ@Φέn
           03  SC11-HOSPADRS            PIC X(100).
      *γΓ@ΦΌΜ
           03  SC11-HOSPNAME            PIC X(100).
      *γΓ@ΦdbΤ
           03  SC11-HOSPTEL             PIC X(15).
      *Ώe[u
           03  SC11-SEIKYU-TBL          OCCURS 2.
      *
               05  SC11-KENSU           PIC X(05).
      *_iΰzj
               05  SC11-TENSU           PIC X(10).
      *Sΰz
               05  SC11-ITBFTN          PIC X(09).
      *ΑLR[h
           03  SC11-TOKKI-TBL           OCCURS 15.
               05  SC11-TOKKI-NYUGAI-TBL    OCCURS 2.
                   07  SC11-TOKKIKENSU  PIC X(03).
      *φοSγΓ
           03  SC11-KOHI-TBL            OCCURS 15.
               05  SC11-KOHI-NYUGAI-TBL     OCCURS 2.
                   07  SC11-KOHIKENSU   PIC X(03).
