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
      *  �������ƥǡ���      CPJYUSIN.INC 
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  JYU-JYUSIN-REC.
           03  JYU-KEY.
      *���ŵ��إ�����
               05  JYU-HOSPID          PIC X(24).
      *���ԣɣ�
               05  JYU-PTID            PIC 9(10).
      *����ǯ����
               05  JYU-UKEYMD          PIC X(08).
      *���ջ���
               05  JYU-UKETIME         PIC X(06).
      *���Ի�̾
           03  JYU-KNJNAME             PIC X(50).
      *���Ų�
           03  JYU-SRYKA               PIC X(02).
      *�ɥ�����������
           03  JYU-TANTDR              PIC X(05).
      *��������
           03  JYU-SRYNAIYO            PIC X(02).
      *��׻���
           03  JYU-KAIKEITIME          PIC 9(04).
      *����ͽ�����
           03  JYU-KYYKTIME            PIC 9(04).
      *ͽ�����
           03  JYU-YYKTIME             PIC 9(04).
      *ͽ��ɣ�
           03  JYU-YYKID               PIC 9(02).
      *ɽ����
           03  JYU-SORT                PIC 9(04).
      *ü���ɣ�
           03  JYU-ADDYMD              PIC X(08).
      *���ڥ졼���ɣ�
           03  JYU-UPDYMD              PIC X(08).
      *����ǯ����
           03  JYU-UPDYMD              PIC X(08).
      *����ǯ����
           03  JYU-UPDYMD              PIC X(08).
      *��������
           03  JYU-UPDYMD              PIC X(08).