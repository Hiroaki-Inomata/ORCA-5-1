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
      *  01.00.01    ����      2010/12/09   ��������������б�
      *  01.00.02    ����      2019/01/11   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT3510.
      *����ǯ��
           03  SKT3510-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT3510-SEIYMD              PIC  X(22).
      *���ŵ��إ�����
           03  SKT3510-HOSPCD              PIC  X(20).
      *�ݸ����ֹ�
           03  SKT3510-HKNJANUM            PIC  X(12).
      *�ݸ���̾
           03  SKT3510-HKNJANAME           PIC  X(24).
      *���ŵ��ؽ����
           03  SKT3510-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT3510-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT3510-TEL                 PIC  X(20).
      *���߼Ի�̾
           03  SKT3510-KAISETUNAME         PIC  X(40).
      *���Ų�CD
           03  SKT3510-SRYKACD             PIC  X(02).
      *���Ų�̾
           03  SKT3510-SRYKANAME           PIC  X(20).
      *��
           03  SKT3510-ZAI1                PIC  X(02).
           03  SKT3510-ZAI2                PIC  X(02).
      *����������
           03  SKT3510-TOKUBETU            PIC  X(10).
      *�ڡ����ֹ�
           03  SKT3510-NO                  PIC  X(04).
      *�������
           03  SKT3510-SEIKYU  OCCURS 9.
               05  SKT3510-NYUGAI-TBL  OCCURS  2.
      *���ܤε��ա����
                   07  SKT3510-KENSU       PIC  X(05).
      *���ܤε��ա����ż�����
                   07  SKT3510-NISSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT3510-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT3510-ITBFTN      PIC  X(09).
      *�������ܡ����
               05  SKT3510-SKENSU         PIC  X(05).
      *�������ܡ�����
               05  SKT3510-SNISSU         PIC  X(05).
      *�������ܡ����
               05  SKT3510-SKINGK         PIC  X(09).
      *�������ܡ�ɸ����ô��
               05  SKT3510-SFTNGK         PIC  X(09).
