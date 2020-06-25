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
        01  SC05.
            03  SC05-KOHKBN         PIC X(02)    OCCURS  7 TIMES.
            03  SC05-YUKOSTYY       PIC X(04).
            03  SC05-YUKOSTMM       PIC X(04).
            03  SC05-YUKOSTDD       PIC X(04).
            03  SC05-YUKOEDYY       PIC X(04).
            03  SC05-YUKOEDMM       PIC X(04).
            03  SC05-YUKOEDDD       PIC X(04).
            03  SC05-CITYNUM        PIC X(12).
            03  SC05-CITYNAME       PIC X(18).
            03  SC05-SEIYMD         PIC X(22).
            03  SC05-HOSPCD         PIC X(14).
            03  SC05-ADRS           PIC X(100).
            03  SC05-HOSPNAME       PIC X(100).
            03  SC05-KAISETU        PIC X(30).
            03  SC05-SRYYM          PIC X(16).
            03  SC05-NYUGAIKBN      PIC X(02)    OCCURS  2 TIMES.
            03  SC05-JKYSKIGO       PIC X(04).
            03  SC05-JKYSNUM        PIC X(14).
            03  SC05-KIGO           PIC X(12).
            03  SC05-NUM            PIC X(20).
            03  SC05-NAME           PIC X(24).
            03  SC05-SEX            PIC X(02)    OCCURS  2 TIMES.
            03  SC05-GENGO          PIC X(02)    OCCURS  4 TIMES.
            03  SC05-TANJOYY        PIC X(02).
            03  SC05-HKNSYUBETU     PIC X(02)    OCCURS  4 TIMES.
            03  SC05-HKNJANUMG.
              05  SC05-HKNJANUM
                                    PIC X(02)    OCCURS  8 TIMES.
            03  SC05-HKNJANAME      PIC X(30).
            03  SC05-FTNRATE        PIC X(02)    OCCURS  4 TIMES.
            03  SC05-NNISSU         PIC X(02).
            03  SC05-NYUINYY        PIC X(04).
            03  SC05-NYUINMM        PIC X(04).
            03  SC05-NYUINDD        PIC X(04).
            03  SC05-TAIINYY        PIC X(04).
            03  SC05-TAIINMM        PIC X(04).
            03  SC05-TAIINDD        PIC X(04).
            03  SC05-GNISSU         PIC X(02).
            03  SC05-TENSU          PIC X(16).
            03  SC05-KESETEN        PIC X(14).
            03  SC05-YKZFTN         PIC X(07).
            03  SC05-SIHARAIGK      PIC X(06).
            03  SC05-SIHARAIKAISU   PIC X(02).
            03  SC05-SIHARAINISSU1  PIC X(02).
            03  SC05-SIHARAINISSU2  PIC X(02).
            03  SC05-SHOKUJINISSU   PIC X(02).
            03  SC05-SFTNGK         PIC X(06).
            03  SC05-SEIKYUGK       PIC X(09).
            03  SC05-ETC-KBN        PIC X(02)    OCCURS  11 TIMES.
