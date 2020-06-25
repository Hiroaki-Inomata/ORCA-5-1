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
      *----------------------------------------------------------*
      *    Æþ±¡ÎÁÊñ³ç¿ÇÎÅ¥Á¥§¥¯¥Ñ¥é¥á¥¿
      *                                      COPY (CPORCSCHKTALL.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ************************************************************
      *
       01  ORCSCHKTALLAREA.
      *    ½èÍý¶èÊ¬¡Ê£±¡§Æþ±¡Êñ³ç¥Á¥§¥Ã¥¯¡Ë
           03  ORCSCHKTALL-KBN             PIC X(01).
      *    ½èÍýÆüÉÕ
           03  ORCSCHKTALL-SRYYMD.
               05  ORCSCHKTALL-SRYYM       PIC X(06).
               05  ORCSCHKTALL-SRYDD       PIC X(02).
      *    ´µ¼Ô£É£Ä
           03  ORCSCHKTALL-PTID            PIC 9(10).
      *    ÂÐ¾Ý¤Î¿ÇÎÅ¶èÊ¬
           03  ORCSCHKTALL-HKTSRYKBN       PIC X(02).
           03  ORCSCHKTALL-HKTSRYSYUKBN    PIC X(03).
      *    ÂÐ¾Ý¤Î¿ÇÎÅ¥³¡¼¥É
           03  ORCSCHKTALL-HKTSRYCD-G.
               05  ORCSCHKTALL-HKTSRYCD    PIC X(09)
                                           OCCURS   10.
      *
           03  ORCSCHKTALL-OUTAREA.
      *        £±¡§Æþ±¡ÎÁÊñ³ç¤¢¤ê
               05  ORCSCHKTALL-NYUHKTKBN       PIC 9(01).
      *        £±¡§·îÆâ¤ËÊñ³ç¤¢¤ê
               05  ORCSCHKTALL-NYUHKTCHK       PIC 9(01).
               05  ORCSCHKTALL-NYUHKT-G.
                   07  ORCSCHKTALL-NYUHKT-OCC  OCCURS   31.
      *                £°¡§Êñ³ç¤Ê¤·¡¢£±¡§Êñ³ç¤¢¤ê¡¢£¹¡§Êñ³ç¥¨¥é¡¼
                       09  ORCSCHKTALL-HKTCHKFLG   PIC 9(01).
      *                Êñ³ç¶èÊ¬
                       09  ORCSCHKTALL-NYUHKTCD    PIC X(03).
      *        Æþ±¡ÎÁ»»ÄêÌµ¤·
               05   ORCSCHKTALL-NYUERR         PIC 9(01).
      *
