      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      *    £Á£Ð£É¥ì¥¹¥Ý¥ó¥¹¿ÇÎÅ²Ê¾ðÊó°ìÍ÷
      *                                     COPY (CPDEPARTMETV2RES.INC)
      ******************************************************************
        01  XML-DEPARTMENTRES.
            02  DEPTRES-CONTEXT            PIC S9(9)   BINARY.
            02  DEPTRES-OBJECT             PIC X(36).
            02  DEPTRES-MODE               PIC S9(9)   BINARY.
            02  DEPTRES-RECORDNAME         PIC X(128).
            02  DEPTRES-APPOINTRES.
      *       ¼Â»ÜÆü
              03  DEPTRES-INFORMATION-DATE    PIC X(10).
      *       ¼Â»Ü»þ´Ö
              03  DEPTRES-INFORMATION-TIME    PIC X(8).
      *       ÊÖµÑ¥³¡¼¥É
              03  DEPTRES-API-RESULT          PIC X(2).
      *       ÊÖµÑ¥á¥Ã¥»¡¼¥¸
              03  DEPTRES-API-RESULT-MSG      PIC X(100).
      *
              03  DEPTRES-RESKEY              PIC X(100).
      *       ´ð½àÆü
              03  DEPTRES-BASE-DATE           PIC X(10).
      *       ¿ÇÎÅ²Ê°ìÍ÷
              03  DEPTRES-DEPARTMET-INFOMATION.
                 05  DEPTRES-DEPARTMET-INFO      OCCURS   100.
      *               ¿ÇÎÅ²Ê¥³¡¼¥É
                      07  DEPTRES-DEP-CODE        PIC X(2).
      *               ¿ÇÎÅ²ÊÌ¾¾Î
                      07  DEPTRES-SRYKANAME       PIC X(80).
      *               Ã»½ÌÌ¾¾Î£±
                      07  DEPTRES-SRYKANAME1      PIC X(20).
      *               Ã»½ÌÌ¾¾Î£²
                      07  DEPTRES-SRYKANAME2      PIC X(12).
      *               Ã»½ÌÌ¾¾Î£³
                      07  DEPTRES-SRYKANAME3      PIC X(04).
      *               ¥ì¥»ÅÅ¿ÇÎÅ²Ê¥³¡¼¥É
                      07  DEPTRES-RECESRYKA       PIC X(02).
      *
