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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2010/10/01   ��������������б�
      *  01.00.02    ����      2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2050.
      *    ����ǯ��
           03  SKT2050-SRYYM                PIC X(16).
      *    �ݸ���̾
           03  SKT2050-HKNJANAME            PIC X(20).
      *    ����ǯ����
           03  SKT2050-SEIYMD               PIC X(22).
      *    ���ŵ��ؽ���
           03  SKT2050-ADRS                 PIC X(100).
      *    ���ŵ���̾��
           03  SKT2050-HOSPNAME             PIC X(100).
      *    ���߼�̾
           03  SKT2050-KAISETUNAME          PIC X(80).
      *    �����ֹ�
           03  SKT2050-TEL                  PIC X(15).
      *    �ݸ����ֹ�
           03  SKT2050-HKNJANUM             PIC X(12).
      *    ���ŵ��إ�����
           03  SKT2050-HOSPCD               PIC X(14).
      *    ����
           03  SKT2050-ZAI1                 PIC X(02).
           03  SKT2050-ZAI2                 PIC X(02).
      *    �ڡ����ֹ�
           03  SKT2050-NO                   PIC X(04).
      *    ����������
           03  SKT2050-TOKUBETU             PIC X(10).
      *    ����ơ��֥�
           03  SKT2050-SEIKYU-TBL  OCCURS  7.
      *    ���
               05  SKT2050-KENSU            PIC X(05).
      *    ������
               05  SKT2050-NISSU            PIC X(05).
      *    ����
               05  SKT2050-TENSU            PIC X(11).
      *    ������ô��
               05  SKT2050-ITBFTN           PIC X(11).
      *    ����ơ��֥�
           03  SKT2050-KOH-TBL  OCCURS  5 .
      *    �����ֹ�
               05  SKT2050-KOHNUM           PIC X(02).
      *    ����̾
               05  SKT2050-KOHNAME          PIC X(10).
      *    ���
               05  SKT2050-K-KENSU          PIC X(05).
      *    ������
               05  SKT2050-K-NISSU          PIC X(05).
      *    ����
               05  SKT2050-K-TENSU          PIC X(11).
      *    ������ô��
               05  SKT2050-K-ITBFTN         PIC X(11).
