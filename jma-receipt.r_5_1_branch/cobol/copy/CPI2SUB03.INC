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
       01  I2SUB03-AREA.
           03  I2SUB03-FROM                                PIC X(16).
           03  I2SUB03-BASEYMD                             PIC X(08).
           03  I2SUB03-ITEM-LIST-AREA.
               05  I2SUB03-SRYKALST-G.
                   07  I2SUB03-SRYKALST-MAX                PIC 9(05).
                   07  I2SUB03-SRYKALST                    OCCURS  99.
                       09  I2SUB03-SRYKAL                  PIC X(02).
                       09  I2SUB03-SRYKAFLL                PIC X(01).
                       09  I2SUB03-SRYKAMEIL               PIC X(20).
               05  I2SUB03-BTUNUMLST-G.
                   07  I2SUB03-BTUNUMLST-MAX               PIC 9(05).
                   07  I2SUB03-BTUNUMLST                   OCCURS  50.
                       09  I2SUB03-BTUNUML                 PIC X(02).
                       09  I2SUB03-BTUNUMFLL               PIC X(01).
                       09  I2SUB03-BTUNUMMEIL              PIC X(20).
               05  I2SUB03-JOTAILST-G.
                   07  I2SUB03-JOTAILST-MAX                PIC 9(05).
                   07  I2SUB03-JOTAILST                    OCCURS  5.
                       09  I2SUB03-JOTAIL                  PIC X(01).
                       09  I2SUB03-JOTAIFLL                PIC X(01).
                       09  I2SUB03-JOTAIMEIL               PIC X(20).
               05  I2SUB03-SEIKYUJOTAILST-G.
                   07  I2SUB03-SEIKYUJOTAILST-MAX          PIC 9(05).
                   07  I2SUB03-SEIKYUJOTAILST              OCCURS  2.
                       09  I2SUB03-SEIKYUJOTAIL            PIC X(01).
                       09  I2SUB03-SEIKYUJOTAIFLL          PIC X(01).
                       09  I2SUB03-SEIKYUJOTAIMEIL         PIC X(20).
               05  I2SUB03-DEATHKBNLST-G.
                   07  I2SUB03-DEATHKBNLST-MAX             PIC 9(05).
                   07  I2SUB03-DEATHKBNLST                 OCCURS  2.
                       09  I2SUB03-DEATHKBNL               PIC X(01).
                       09  I2SUB03-DEATHKBNFLL             PIC X(01).
                       09  I2SUB03-DEATHKBNMEIL            PIC X(08).
               05  I2SUB03-TSTPTLST-G.
                   07  I2SUB03-TSTPTLST-MAX                PIC 9(05).
                   07  I2SUB03-TSTPTLST                    OCCURS  10.
                       09  I2SUB03-TSTPTL                  PIC X(01).
                       09  I2SUB03-TSTPTFLL                PIC X(01).
                       09  I2SUB03-TSTPTMEIL               PIC X(30).
               05  I2SUB03-TAIINCDLST-G.
                   07  I2SUB03-TAIINCDLST-MAX              PIC 9(05).
                   07  I2SUB03-TAIINCDLST                  OCCURS  50.
                       09  I2SUB03-TAIINCDL                PIC X(02).
                       09  I2SUB03-TAIINCDFLL              PIC X(01).
                       09  I2SUB03-TAIINCDMEIL             PIC X(20).
              05   I2SUB03-SORTLST-G.
                   07  I2SUB03-SORTLST-MAX                  PIC 9(05).
                   07  I2SUB03-SORTLST                      OCCURS 10.
                       09  I2SUB03-SORTL                    PIC X(01).
                       09  I2SUB03-SORTFLL                  PIC X(01).
                       09  I2SUB03-SORTMEIL                 PIC X(20).
