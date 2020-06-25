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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    吉川         16/04/07  公費の項目追加
      ********************************************************************
       01 SC11.
           03  SC11-NO                PIC X(10).
           03  SC11-HOSPCDN           PIC X(07).
           03  SC11-HOSPNAME          PIC X(100).
           03  SC11-IKA               PIC X(02).
           03  SC11-SIKA              PIC X(02).
           03  SC11-TYOUZAI           PIC X(02).
           03  SC11-JUUSEI            PIC X(02).
           03  SC11-HMNKANGO          PIC X(02).
           03  SC11-PRTYM             PIC X(16).
           03  SC11-NYUIN             PIC X(02).
           03  SC11-NYUINGAI          PIC X(02).
           03  SC11-KANJA-TBL   OCCURS 10.
               05  SC11-JKYSNUM       PIC X(14).
               05  SC11-HKNNUM        PIC X(02) OCCURS 6.
               05  SC11-HKN-HONNIN    PIC X(02).
               05  SC11-HKN-KAZOKU    PIC X(02).
               05  SC11-NAME          PIC X(30).
               05  SC11-SEX-G OCCURS 2.
                   07  SC11-SEX       PIC X(02).
               05  SC11-TANJOY        PIC X(02).
               05  SC11-TANJOM        PIC X(02).
               05  SC11-TANJOD        PIC X(02).
               05  SC11-SEIKYUTEN     PIC X(07).
               05  SC11-ITBFTNKIN     PIC X(09).
               05  SC11-IRYOHI        PIC X(11).
               05  SC11-SRYNISSU      PIC X(03).
               05  SC11-KZKRYOYOH     PIC X(02).
               05  SC11-KOHI          PIC X(02).
               05  SC11-SRYYY         PIC X(02).
               05  SC11-SRYMM         PIC X(02).
           03  SC11-TAL-TBL.
               05  SC11-TAL-SEIKYU    PIC X(09).
               05  SC11-TAL-ITBFTNKIN PIC X(09).
               05  SC11-TAL-IRYOHI    PIC X(11).
               05  SC11-TAL-SRYNISSU  PIC X(04).
