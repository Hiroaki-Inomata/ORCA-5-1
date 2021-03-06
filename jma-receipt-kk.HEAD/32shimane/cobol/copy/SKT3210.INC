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
       01  SKT3210.
      *    ����ǯ��
           03  SKT3210-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT3210-SEIYMD              PIC  X(22).
      *    ����
           03  SKT3210-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT3210-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT3210-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT3210-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT3210-HKNJANUM            PIC  X(12).
      *    �ݸ���̾
           03  SKT3210-HKNJANAME           PIC  X(40).
      *    ���ŵ��إ�����
           03  SKT3210-HOSPCD              PIC  X(14).
      *    ��������
           03  SKT3210-ZAIMARU             PIC  X(02).
      *    ����ʸ��
           03  SKT3210-ZAIMOJI             PIC  X(02).
      *    ����������
           03  SKT3210-TOKUBETU            PIC  X(10).
      *    NO
           03  SKT3210-NO                  PIC  X(04).
      *    ����ơ��֥�
           03  SKT3210-SEIKYU-TBL  OCCURS  9.
      *    �����ơ��֥�
               05  SKT3210-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3210-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3210-NISSU       PIC  X(05).
      *    ����
                   07  SKT3210-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3210-ITBFTN      PIC  X(09).
      *    Ĺ�����ܷ��
                   07  SKT3210-TYOUKEN     PIC  X(03).
      *    �������ܡ����
               05  SKT3210-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3210-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3210-S-KINGAKU       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3210-S-FTNGAKU       PIC  X(09).
