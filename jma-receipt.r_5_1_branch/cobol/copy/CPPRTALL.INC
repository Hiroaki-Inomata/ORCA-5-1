      *******************************************************************
      * Project code name "ORCA"
      * $BF|0eI8=`%l%;%W%H%=%U%H!J(BJMA standard receipt software$B!K(B
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
      *    $B0u:~>pJs(B                     COPY  (CPPRTINF.INC)     *
      ************************************************************
      ************************************************************
      *  $B%3%T!<6g=$@5MzNr(B
      * Maj/Min/Rev  $B=$@5<T(B       $BF|IU(B      $BFbMF(B
      *  XX.XX.XX    NACL-XXXX    99/99/99 
      ************************************************************
       01  PRT-REC.
           03  PRT-PRTID               PIC X(30).
           03  PRT-PRTNM               PIC X(20).
           03  PRT-OFFSET-X            PIC X(05).
           03  PRT-OFFSET-Y            PIC X(05).
           03  PRT-SITEKBN             PIC X(01).
           03  PRT-PRTKBN              PIC X(01).
           03  PRT-PSFILE-AREA.
               05  PRT-OUT             PIC X(165).
               05  PRT-TBL-KEY         PIC X(08).
               05  PRT-RENNUM          PIC 9(04).
               05  PRT-TBL-GROUP       PIC X(14).
               05  PRT-SHORI-RENNUM    PIC 9(04).
               05  PRT-PAGE            PIC 9(05).
               05  PRT-UUID            PIC X(36).
           03  PRT-DATA-TYPE           PIC X(16).
           03  PRT-PRTDATA             PIC X(20000).

