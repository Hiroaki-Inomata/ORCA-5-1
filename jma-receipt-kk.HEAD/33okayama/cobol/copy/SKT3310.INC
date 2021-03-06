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
      *  01.00.02    ���      2019/01/10   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT3310.
      *    ����ǯ��
           03       SKT3310-SRYYM                 PIC  X(16).
      *    ���ŵ��ؽ���
           03       SKT3310-ADRS                  PIC  X(100).
      *    ���ŵ���̾
           03       SKT3310-HOSPNAME              PIC  X(100).
      *    �����ֹ�
           03       SKT3310-TEL                   PIC  X(15).
      *    ���߼�̾
           03       SKT3310-KAISETUNAME           PIC  X(40).
      *    �ݸ���̾
           03       SKT3310-HKNJANAME             PIC  X(26).
      *    ����ǯ����
           03       SKT3310-SEIYMD                PIC  X(22).
      *    �ݸ����ֹ�
           03       SKT3310-HKNJANUM              PIC  X(12).
      *    ���ŵ��إ�����
           03       SKT3310-HOSPCD                PIC  X(14).
      *    ����������
           03       SKT3310-TOKUBETU              PIC  X(10).
      *    �ڡ����ֹ�
           03       SKT3310-NO                    PIC  X(04).
      *    ����ơ��֥�
           03       SKT3310-HKN-TBL.
               05  SKT3310-PAN-TBL  OCCURS  9.      
                   07  SKT3310-NYUGAI-TBL  OCCURS  2.
                       09  SKT3310-KENSU              PIC  X(05).
                       09  SKT3310-NISSU              PIC  X(05).
                       09  SKT3310-TENSU              PIC  X(09).
                       09  SKT3310-ITBFTN             PIC  X(09).
                   07  SKT3310-SKENSU                 PIC  X(05).
                   07  SKT3310-SNISSU                 PIC  X(05).
                   07  SKT3310-SKINGK                  PIC  X(09).
                   07  SKT3310-SFTNGK                  PIC  X(09).
           03       SKT3310-ZAITAKU-TBL   OCCURS 4.
               05  SKT3310-ZKENSU                     PIC  X(05).
               05  SKT3310-ZNISSU                     PIC  X(05).
               05  SKT3310-ZTENSU                     PIC  X(09).
               05  SKT3310-ZITBFTN                    PIC  X(09).
