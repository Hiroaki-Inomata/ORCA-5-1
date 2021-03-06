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
      ******************************************************************
      *    `ohNGXgό@γΓζͺE`ck_ξρ 
      *    COPY (CPHSPTEVALV2REQ.INC)
      ******************************************************************
        01  XML-HSPTEVALV2REQ.
            03  HSPTEVALREQ-CONTEXT          PIC S9(9)   BINARY.
            03  HSPTEVALREQ-OBJECT           PIC X(36).
            03  HSPTEVALREQ-MODE             PIC S9(9)   BINARY.
            03  HSPTEVALREQ-RECORDNAME       PIC X(128).
      *
            03  HSPTEVALREQ-PRIVATE-OBJECTS.
                05  HSPTEVALREQ-PTNUM        PIC X(20).
                05  HSPTEVALREQ-SRYYM        PIC X(07).
