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
      *    £Á£Ð£É¥ê¥¯¥¨¥¹¥ÈÍ½Ìó¾ðÊó¡¡¡¡       COPY (CPAPPTXMLV2REQ.INC)
      ******************************************************************
        01  XML-APPOINTREQ.
            02  APNTREQ-CONTEXT            PIC S9(9)   BINARY.
            02  APNTREQ-OBJECT             PIC X(36).
            02  APNTREQ-MODE               PIC S9(9)   BINARY.
            02  APNTREQ-RECORDNAME         PIC X(128).
            02  APNTREQ-APPOINT.
      *       ´µ¼ÔÈÖ¹æ
              03  APNTREQ-PATIENTID    PIC X(20).
      *       Í½Ìó»áÌ¾
              03  APNTREQ-APPOINT-NAME     PIC X(100).
      *       Í½Ìó¥«¥Ê»áÌ¾
              03  APNTREQ-APPOINT-KANANAME PIC X(100).
      *       Í½ÌóÆü
              03  APNTREQ-APPOINT-DATE PIC X(10).
      *       Í½Ìó»þ´Ö
              03  APNTREQ-APPOINT-TIME PIC X(8).
      *       Í½Ìó£É£Ä
              03  APNTREQ-APPOINT-ID   PIC X(5).
      *       ¿ÇÎÅ²Ê
              03  APNTREQ-APPOINT-DEP-CODE PIC X(2).
      *       ¥É¥¯¥¿¡¼¥³¡¼¥É
              03  APNTREQ-APPOINT-DR-CODE  PIC X(5).
      *       ¿ÇÎÅÆâÍÆ¶èÊ¬¥³¡¼¥É
              03  APNTREQ-APPOINT-DEP-INFO PIC X(2).
      *       Í½ÌóÆâÍÆ¥³¡¼¥É
              03  APNTREQ-APPOINT-INFO PIC X(2).
      *       Í½Ìó¥á¥â
              03  APNTREQ-APPOINT-MEMO PIC X(100).
