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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------------*
      *    ����饤��Ģɼ��������                                      *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  ORCSMKPRTAREA.
      *    �ӣȣţ̣�̾
           03  MKPRT-ID                 PIC  X(20).
      *    Ģɼ̾
           03  MKPRT-DIA                PIC  X(50).
      *    �ģţ�̾
           03  MKPRT-DEF                PIC  X(34).
      *    �ǡ����μ���
           03  MKPRT-TYPE               PIC  X(16).
      *    �ǡ�������
           03  MKPRT-DAT                PIC  X(1024).
      *    ��������
           03  MKPRT-PRTNM              PIC  X(16).
      *    ��������
           03  MKPRT-INFO               PIC  X(100).
      *    ���ե��å�
      **     03  MKPRT-OFFSET-X           PIC  S9(2)V9(1).
      **     03  MKPRT-OFFSET-Y           PIC  S9(2)V9(1).
      *
      *    ����������
           03  MKPRT-RC                 PIC  9(04).
      *
