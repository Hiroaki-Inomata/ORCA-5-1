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
      *  �ץ�����ཤ������
      * Maj/Min/Rev 	������	����		����
      *  01.00.01    Ĺ��    08/04/27    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����    10/12/09    ��������������б�
      *  01.00.03    ����    19/01/11    �������򥳥��Ⱥ��
      *****************************************************************
       01  SKT3520.
      *����ǯ��
           03  SKT3520-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT3520-SEIYMD              PIC  X(22).
      *���ŵ��إ�����
           03  SKT3520-HOSPCD              PIC  X(20).
      *�ݸ����ֹ�
           03  SKT3520-HKNJANUM            PIC  X(12).
      *�ݸ���̾
           03  SKT3520-HKNJANAME           PIC  X(24).
      *���ŵ��ؽ����
           03  SKT3520-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT3520-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT3520-TEL                 PIC  X(20).
      *���߼Ի�̾
           03  SKT3520-KAISETUNAME         PIC  X(40).
      *���Ų�CD
           03  SKT3520-SRYKACD             PIC  X(02).
      *���Ų�̾
           03  SKT3520-SRYKANAME           PIC  X(20).
      *��
           03  SKT3520-ZAI1                PIC  X(02).
           03  SKT3520-ZAI2                PIC  X(02).
      *����������
           03  SKT3520-TOKUBETU            PIC  X(10).
      *�ڡ����ֹ�
           03  SKT3520-NO                  PIC  X(04).
      *�������
           03  SKT3520-SEIKYU  OCCURS 2.
               05  SKT3520-NYUGAI-TBL  OCCURS  2.
      *���ܤε��ա����
                   07  SKT3520-KENSU       PIC  X(05).
      *���ܤε��ա����ż�����
                   07  SKT3520-NISSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT3520-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT3520-ITBFTN      PIC  X(09).
      *�������ܡ����
               05  SKT3520-SKENSU          PIC  X(05).
      *�������ܡ�����
               05  SKT3520-SNISSU          PIC  X(05).
      *�������ܡ����
               05  SKT3520-SKINGK          PIC  X(09).
      *�������ܡ�ɸ����ô��
               05  SKT3520-SFTNGK          PIC  X(09).
      *�����������
           03  SKT3520-K-SEIKYU  OCCURS 7.
      *û������̾
               05  SKT3520-K-KOHNAME          PIC  X(08).
               05  SKT3520-K-NYUGAI-TBL  OCCURS  2.
      *���ܤε��ա����
                   07  SKT3520-K-KENSU       PIC  X(05).
      *���ܤε��ա����ż�����
                   07  SKT3520-K-NISSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT3520-K-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT3520-K-ITBFTN      PIC  X(09).
      *�������ܡ����
               05  SKT3520-K-SKENSU          PIC  X(05).
      *�������ܡ�����
               05  SKT3520-K-SNISSU          PIC  X(05).
      *�������ܡ����
               05  SKT3520-K-SKINGK          PIC  X(09).
      *�������ܡ�ɸ����ô��
               05  SKT3520-K-SFTNGK          PIC  X(09).
