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
      *  01.00.01    ����      2010/11/01   ��������������б�
      *  01.00.02    ����      2020/02/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2320.
      *    ����ǯ��
           03  SKT2320-PRTYM       PIC  X(16).
      *    ����ǯ����
           03  SKT2320-SEIYMD      PIC  X(22).
      *    �ݸ����ֹ�
           03  SKT2320-HKNJANUM    PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT2320-HOSPCD      PIC  X(14).
      *    ���ŵ��ؽ���
           03  SKT2320-HOSPADRS    PIC  X(100).
      *    ���ŵ���̾��
           03  SKT2320-HOSPNAME    PIC  X(100).
      *    ���ŵ��������ֹ�
           03  SKT2320-HOSPTEL     PIC  X(15).
      *    ���ŵ��س��߼�̾
           03  SKT2320-KAISETU     PIC  X(40).
      *    ����
           03  SKT2320-ZAI1        PIC  X(02).
           03  SKT2320-ZAI2        PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT2320-NO          PIC  X(04).
      *    ����������
           03  SKT2320-TOKUBETU    PIC  X(10).
      *    ���ܤε��եơ��֥�
           03  SKT2320-RYOUYOU-TBL OCCURS 2.
      *    �����ơ��֥�
               05  SKT2320-NYUGAI-TBL OCCURS 2.
      *    ���
                   07  SKT2320-KENSU      PIC X(05).
      *    ����
                   07  SKT2320-NISSU      PIC X(05).
      *    ����
                   07  SKT2320-TENSU      PIC X(09).
      *    ������ô��
                   07  SKT2320-ITBFTN     PIC X(09).
      *    ����������ơ��֥�
           03  SKT2320-SHOKUJI-TBL OCCURS 2.
      *    �����ݷ��
               05  SKT2320-S-KENSU         PIC X(05).
      *    ����������
               05  SKT2320-S-NISSU         PIC X(05).
      *    �����ݶ��
               05  SKT2320-S-MONEY         PIC X(09).
      *    ������ɸ����ô��
               05  SKT2320-S-HJNFTN        PIC X(09).
      *    ������������
           03  SKT2320-GENBAKU            PIC X(09).
