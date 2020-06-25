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
      ************************************************************
      *    ¾Ò²ðÍúÎò¡¡        ¡¡ COPY (CPSYKRRK.INC)     *
      ************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ************************************************************
      *01  SYKRRK-REC.
      *¥­¡¼
           03  SYK-KEY.
      *°åÎÅµ¡´Ø£É£Ä
               05  SYK-HOSPNUM              PIC 9(2).
      *´µ¼Ô£É£Ä
               05  SYK-PTID                PIC  9(10).
      *¾Ò²ð¶èÊ¬
               05  SYK-SYKKBN              PIC  X(01).
      *¾Ò²ðÇ¯·îÆü
               05  SYK-SYKYMD              PIC  X(08).
      *¾Ò²ð°åÎÅµ¡´Ø£É£Ä
           03  SYK-SYKIRYID                PIC  X(10).
      *¾Ò²ð°åÎÅµ¡´ØÌ¾¾Î
           03  SYK-SYKIRYNAME              PIC  X(100).
      *¾Ò²ð²Ê
           03  SYK-SYKKA                   PIC  X(02).
      *£Ó£Ï£Á£Ð
           03  SYK-SOAP                    PIC  X(100).
      *Ã¼Ëö£É£Ä
           03  SYK-TERMID                  PIC  X(16).
      *¥ª¥Ú¥ì¡¼¥¿£É£Ä
           03  SYK-OPID                    PIC  X(16).
      *ºîÀ®Ç¯·îÆü
           03  SYK-CREYMD                  PIC  X(08).
      *¹¹¿·Ç¯·îÆü
           03  SYK-UPYMD                   PIC  X(08).
      *¹¹¿·»þ´Ö
           03  SYK-UPHMS                   PIC  X(06).
