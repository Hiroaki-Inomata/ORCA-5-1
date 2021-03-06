      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
       01  SC13.
      *¿N
           03  SC13-SEIYM               PIC X(16).
      *ãÃ@Ö¼Ì
           03  SC13-HOSPNAME            PIC X(100).
      *ãÃ@ÖR[h
           03  SC13-HOSPCD-TBL.
               05  SC13-HOSPCD          PIC X(02)  OCCURS 10.
      *s¬º¼
           03  SC13-CITYNAME            PIC X(10).
      *s¬ºÔ
           03  SC13-CITYNUM-TBL.
               05  SC13-CITYNUM         PIC X(02)  OCCURS 3.
      *¾×e[u
           03  SC13-MEISAI-TBL          OCCURS 5.
      *óÒØÔ
               05  SC13-JKYSNUM-TBL.
                   07  SC13-JKYSNUM     PIC X(02)  OCCURS 11.
      *Û¯ÒÔ
               05  SC13-HKNJANUM-TBL.
                   07  SC13-HKNJANUM    PIC X(02)  OCCURS 8.
      *L
               05  SC13-KIGO            PIC X(20).
      *Ô
               05  SC13-NUM             PIC X(20).
      *¼
               05  SC13-NAME            PIC X(40).
      *«Ê
               05  SC13-SEX             PIC X(04).
      *¶Nú
               05  SC13-BIRTHDAY        PIC X(16).
      *fÃN
               05  SC13-SRYYM           PIC X(16).
      *üO
               05  SC13-NYUGAIKBN       PIC X(06).
      *êSà
               05  SC13-ITBFTN          PIC X(09).
      *ûâ³­sãÃ@Ö
               05  SC13-SYOHOSEN-TBL.
                   07  SC13-SYOHOSEN    PIC X(02)  OCCURS 10.
      *ûSz
               05  SC13-KINGAKU         PIC X(09).
      *s¬ºvF
           03  SC13-GKENSU              PIC X(05).
      *s¬ºvFêSà
           03  SC13-GITBFTN             PIC X(10).
      *s¬ºvFûSz
           03  SC13-GKINGAKU            PIC X(10).
      *s¬ºvFÅ
           03  SC13-PAGE                PIC X(03).
      *s¬ºvFÅv
           03  SC13-GPAGE               PIC X(03).
