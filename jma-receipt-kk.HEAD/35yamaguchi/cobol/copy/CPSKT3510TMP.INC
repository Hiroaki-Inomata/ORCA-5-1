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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT3500TMP.INC)            *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    Ä¹²°   08/04/28      Ä¢É¼ÍÍ¼°¤ËÈ¼¤¦½¤Àµ
      *  01.00.02    ÆâÅÄ   09/08/25      Ãæ¹ñ¶å½£ËÌÉô¹ë±«ÂÐ±þ¡ÊÂÐ¾Ý¼ÔÊÌ½¸·×¡Ë
      *  01.00.03    µÈÀî   11/04/19      ÅìËÌÃÏÊýÂÀÊ¿ÍÎ²­ÃÏ¿ÌÂÐ±þ
      *  01.00.04    ÇëÌî   19/01/11      ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT3510TMP-REC.
           03      SKT3510TMP-KEY.
             05    SKT3510TMP-SYUTURYOKU            PIC  9(01).
             05    SKT3510TMP-ZAITAKU               PIC  X(01).
             05    SKT3510TMP-HKNJANUM              PIC  X(06).
             05    SKT3510TMP-SRYYM                 PIC  9(06).
             05    SKT3510TMP-SRYKA                 PIC  X(02).
      *    Ãæ¹ñ¶å½£ËÌÉô¹ë±«¸ºÌÈ
             05    SKT3510TMP-GENMEN                PIC  X(05).
           03    SKT3510TMP-PAGE-FLG1               PIC  9(01).
           03    SKT3510TMP-PAGE-FLG2               PIC  9(01).
           03    SKT3510TMP-SEIKYU-TBL              OCCURS 11.
               05    SKT3510TMP-NYUGAI-TBL          OCCURS 2.
                 07    SKT3510TMP-KENSU             PIC 9(05).
                 07    SKT3510TMP-NISSU             PIC 9(05).
                 07    SKT3510TMP-TENSU             PIC 9(09).
                 07    SKT3510TMP-ITBFTN            PIC 9(09).
               05    SKT3510TMP-SKENSU              PIC 9(05).
               05    SKT3510TMP-SNISSU              PIC 9(05).
               05    SKT3510TMP-SKINGK              PIC 9(09).
               05    SKT3510TMP-SFTNGK              PIC 9(09).
           03  SKT3510TMP-KOH-AREA.
               05  SKT3510TMP-KOH-TBL               OCCURS  20.
                   07  SKT3510TMP-KOHNUM            PIC  X(03).
                   07  SKT3510TMP-K-NYUGAI-TBL      OCCURS  2.
                       09  SKT3510TMP-K-KENSU       PIC  9(05).
                       09  SKT3510TMP-K-NISSU       PIC  9(05).
                       09  SKT3510TMP-K-TENSU       PIC  9(09).
                       09  SKT3510TMP-K-ITBFTN      PIC  9(09).
                   07  SKT3510TMP-K-SKENSU          PIC  9(05).
                   07  SKT3510TMP-K-SNISSU          PIC  9(05).
                   07  SKT3510TMP-K-SKINGK          PIC  9(09).
                   07  SKT3510TMP-K-SFTNGK          PIC  9(09).
