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
      *****************************************************************
      *  ͽ�󥳥��ȥޥ�����       (CPYYKCOM.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  YYKCOM-REC.
           03  YYKCOM-KEY.
      *���ŵ��أɣ�
               05  YYKCOM-HOSPNUM          PIC 9(2).
      *��������
               05  YYKCOM-SRYNAIYO         PIC X(02).
      *�ɥ�����������
               05  YYKCOM-DRCD             PIC X(05).
      *ͽ��ǯ����
               05  YYKCOM-YYKYMD           PIC X(08).
      *����ͽ�����
               05  YYKCOM-KEYYYKTIME       PIC 9(04).
      *ͽ��ɣ�
               05  YYKCOM-YYKID            PIC 9(02).
      *Ϣ�ֹ�
               05  YYKCOM-RENNUM           PIC 9(01).
      *���
           03  YYKCOM-COMMENT              PIC X(100).
      *ü���ɣ�
           03  YYKCOM-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  YYKCOM-OPID                 PIC  X(16).
      *����ǯ����
           03  YYKCOM-CREYMD               PIC  X(08).
      *����ǯ����
           03  YYKCOM-UPYMD                PIC  X(08).
      *��������
           03  YYKCOM-UPHMS                PIC  X(06).