      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *******************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev  ������      ����      ����
      *   01.00.01    @@@@-����   11/01/05  ������������
      *******************************************************************
       01  K145TMP-REC.
           03  K145TMP-KEY.
               05  K145TMP-NYUGAIKBN       PIC  X(01).
               05  K145TMP-BTUINDEX        PIC  X(02).
           03  K145TMP-SRY-TBL OCCURS 56.
               05  K145TMP-TENSU           PIC  9(11).
               05  K145TMP-KAISU           PIC  9(11).