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
      ********************************************************************
      ************************************************************
      *    �ģ´�������    �� COPY  (CPPGKANRI.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PGKANRI-REC.
      *����
           03  PGKANRI-KEY.
      *�ǥӥ���С������
               05  PGKANRI-DEBIANVERSION   PIC  X(24).
      *�����ƥ�С������
               05  PGKANRI-SYSTEMVERSION   PIC  X(10).
      *�ץ�������ֹ�
               05  PGKANRI-PGNO            PIC  X(03).
      *��꡼��ǯ����
           03  PGKANRI-RELEASEYMD          PIC  X(08).
      *�ץ������̾
           03  PGKANRI-PGNAME              PIC  X(64).
      *�����ե饰
           03  PGKANRI-UPDATEFLG           PIC  X(01).
      *�Ƶ�ư�ե饰
           03  PGKANRI-RESTARTFLG          PIC  X(01).
      *���󥹥ȡ������
           03  PGKANRI-INSTALLDIR          PIC  X(256).
      *����
           03  PGKANRI-COMMENT             PIC  X(400).
      *ü���ɣ�
           03  PGKANRI-TERMID              PIC  X(32).
      *���ڡ��졼���ɣ�
           03  PGKANRI-OPID                PIC  X(16).
      *����ǯ����
           03  PGKANRI-CREYMD              PIC  X(08).
      *����ǯ����
           03  PGKANRI-UPYMD               PIC  X(08).
      *��������
           03  PGKANRI-UPHMS               PIC  X(06).