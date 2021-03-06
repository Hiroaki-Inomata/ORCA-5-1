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
      *  01.00.02    ���      2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2535.
      *    NO
           03  SKT2535-NO                  PIC  X(04).
      *    �ݸ����ֹ�
           03  SKT2535-HKNJANUM            PIC  X(06).
      *    ���ŵ��إ�����
           03  SKT2535-HOSPCD              PIC  X(07).
      *    ����������
           03  SKT2535-TOKUBETU            PIC  X(10).
      *    ���ճ��
      *    Ϸ������ơ��֥�
           03  SKT2535-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT2535-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2535-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2535-NISSU       PIC  X(05).
      *    ����
                   07  SKT2535-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2535-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2535-SKENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT2535-SNISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT2535-SKINGK          PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2535-SFTNGK          PIC  X(09).
      *
      *    ʡ������ơ��֥�
           03  SKT2535-FUKUSHI-TBL.
      *    �����ơ��֥�
               05  SKT2535-FNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2535-F-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2535-F-NISSU       PIC  X(05).
      *    ����
                   07  SKT2535-F-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2535-F-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2535-F-SKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT2535-F-SNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT2535-F-SKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2535-F-SFTNGK         PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT2535-KOUHI-TBL  OCCURS  4.
      *    ����̾
               05  SKT2535-KOHNAME         PIC  X(12).
      *    �����ơ��֥�
               05  SKT2535-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2535-K-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2535-K-NISSU       PIC  X(05).
      *    ����
                   07  SKT2535-K-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2535-K-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2535-K-SKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT2535-K-SNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT2535-K-SKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2535-K-SFTNGK         PIC  X(09).
      *    Ĺ����ۼ��·��
           03  SKT2535-TYOKI-KENSU          PIC  X(05).
      *    Ϸ�͡�����ʻ�ѷ��
           03  SKT2535-GENBK-KENSU          PIC  X(05).
