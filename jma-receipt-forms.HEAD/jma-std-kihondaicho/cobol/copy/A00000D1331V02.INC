      *******************************************************************
      * Project code name "ORCA"
      * ����W�����Z�v�g�\�t�g�iJMA standard receipt software�j
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
      *  �R�s�[��C������
      * Maj/Min/Rev  �C����       ���t      ���e
      * 01.00.01     @@@@-�c��    10/12/13  �R�����g�����폜
      * 01.00.02     @@@@-���    19/01/24  ���ו��̌����C��
      ********************************************************************
       01  D1331.
           03  D1331-TITLE.
               05  D1331-SUBTITLE     PIC X(30).
               05  D1331-JOBMSG       PIC X(60).
               05  D1331-PAGE         PIC X(3).
               05  D1331-SYSYMD       PIC X(22).
               05  D1331-FRYMD        PIC X(22).
               05  D1331-TOYMD        PIC X(22).
               05  D1331-SRYKA        PIC X(20).
               05  D1331-DRNAME       PIC X(30).
               05  D1331-TJIHI-G      OCCURS  10 TIMES.
                 07  D1331-TJIHI      PIC X(12).
           03  D1331-DATA.
               05  D1331-YOSO-G       OCCURS  15 TIMES.
                 07  D1331-YOSO-OCC   OCCURS  11 TIMES.
                   09  D1331-YOSO     PIC X(11).