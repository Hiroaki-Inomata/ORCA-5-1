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
      *    指定診療行為件数調サブパラメタ　COPY (CPORCHCG016S01.INC)
      *                                   使用プログラム (ORCHCG016)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  HCG016S01-AREA.
           03  HCG016S01-IN-AREA.
               05  HCG016S01-HOSPNUM       PIC 9(02).
               05  HCG016S01-PTID          PIC 9(10).
               05  HCG016S01-KJNYMD        PIC X(08).
           03  HCG016S01-OT-AREA.
               05  HCG016S01-HKNCOMBI      PIC X(04).
               05  HCG016S01-HKNNUM        PIC X(03).
               05  HCG016S01-HKNCOMBIMEI   PIC X(100).
               05  HCG016S01-SRYKA         PIC X(02).
               05  HCG016S01-LASTYMD       PIC X(08).
               05  HCG016S01-BRMNUM        PIC X(10).
