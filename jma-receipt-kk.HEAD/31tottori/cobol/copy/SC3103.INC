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
        01  SC03.
            03  SC03-KOHKBN         PIC X(02)    OCCURS  7 TIMES.
            03  SC03-YUKOSTYY       PIC X(04).
            03  SC03-YUKOSTMM       PIC X(04).
            03  SC03-YUKOSTDD       PIC X(04).
            03  SC03-YUKOEDYY       PIC X(04).
            03  SC03-YUKOEDMM       PIC X(04).
            03  SC03-YUKOEDDD       PIC X(04).
            03  SC03-CITYNUM        PIC X(12).
            03  SC03-CITYNAME       PIC X(18).
            03  SC03-SEIYMD         PIC X(22).
            03  SC03-HOSPCD         PIC X(14).
            03  SC03-ADRS           PIC X(100).
            03  SC03-HOSPNAME       PIC X(100).
            03  SC03-KAISETU        PIC X(30).
            03  SC03-SRYYM          PIC X(16).
            03  SC03-NYUGAIKBN      PIC X(02)    OCCURS  2 TIMES.
            03  SC03-JKYSKIGO       PIC X(04).
            03  SC03-JKYSNUM        PIC X(14).
            03  SC03-KIGO           PIC X(18).
            03  SC03-NUM            PIC X(20).
            03  SC03-NAME           PIC X(24).
            03  SC03-SEX            PIC X(02)    OCCURS  2 TIMES.
            03  SC03-GENGO          PIC X(02)    OCCURS  4 TIMES.
            03  SC03-TANJOYY        PIC X(02).
            03  SC03-HKNSYUBETU     PIC X(02)    OCCURS  3 TIMES.
            03  SC03-HKNJANUMG.
              05  SC03-HKNJANUM
                                    PIC X(02)    OCCURS  8  TIMES.
            03  SC03-HKNJANAME      PIC X(30).
            03  SC03-FTNRATE        PIC X(02)    OCCURS  4 TIMES.
            03  SC03-NNISSU         PIC X(02).
            03  SC03-NYUINYY        PIC X(04).
            03  SC03-NYUINMM        PIC X(04).
            03  SC03-NYUINDD        PIC X(04).
            03  SC03-TAIINYY        PIC X(04).
            03  SC03-TAIINMM        PIC X(04).
            03  SC03-TAIINDD        PIC X(04).
            03  SC03-GNISSU         PIC X(02).
            03  SC03-TENSU          PIC X(16).
            03  SC03-KESETEN        PIC X(14).
            03  SC03-YKZFTN         PIC X(07).
            03  SC03-SIHARAIGK      PIC X(06).
            03  SC03-SIHARAIKAISU   PIC X(02).
            03  SC03-SIHARAINISSU1  PIC X(02).
            03  SC03-SIHARAINISSU2  PIC X(02).
            03  SC03-SHOKUJINISSU   PIC X(02).
            03  SC03-SFTNGK         PIC X(06).
            03  SC03-SEIKYUGK       PIC X(09).
            03  SC03-ETC-KBN        PIC X(02)    OCCURS  11 TIMES.
