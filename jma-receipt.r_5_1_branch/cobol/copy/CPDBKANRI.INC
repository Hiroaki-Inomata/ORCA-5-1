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
      *    �ģ´�������    �� COPY  (CPDBKANRI.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  DBKANRI-REC.
      *����
           03  DBKANRI-KEY.
      *����������
               05  DBKANRI-KANRICD         PIC  X(08).
      *�С������
           03  DBKANRI-VERSION             PIC  X(64).
      *�ģ¹�¤�С������
           03  DBKANRI-DBSVERSION1         PIC  X(64).
      *�ģ¹�¤�С������
           03  DBKANRI-DBSVERSION2         PIC  X(64).
      *ü���ɣ�
           03  DBKANRI-TERMID              PIC  X(32).
      *���ڡ��졼���ɣ�
           03  DBKANRI-OPID                PIC  X(16).
      *����ǯ����
           03  DBKANRI-CREYMD              PIC  X(08).
      *����ǯ����
           03  DBKANRI-UPYMD               PIC  X(08).
      *��������
           03  DBKANRI-UPHMS               PIC  X(06).