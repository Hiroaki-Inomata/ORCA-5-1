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
      *    �ģ´�������    �� COPY  (CPPGKANRIC.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PGKANRIC-REC.
      *�ǥӥ���С������
           03  PGKANRIC-DEBIANVERSION       PIC  X(20).
      *�����ƥ�С������
           03  PGKANRIC-SYSTEMVERSION       PIC  X(10).
      *�ץ�������ֹ�
           03  PGKANRIC-PGNO                PIC  X(03).
           03  PGKANRIC-YOBI1               PIC  X(01).
      *��꡼��ǯ����
           03  PGKANRIC-RELEASEYMD          PIC  X(08).
           03  PGKANRIC-YOBI2               PIC  X(01).
      *�ץ������̾
           03  PGKANRIC-PGNAME              PIC  X(32).
      *�Ƶ�ư�ե饰
           03  PGKANRIC-RESTARTFLG          PIC  X(01).
           03  PGKANRIC-YOBI3               PIC  X(01).
      *���󥹥ȡ������
           03  PGKANRIC-INSTALLDIR          PIC  X(64).
      *����
           03  PGKANRIC-COMMENT             PIC  X(200).