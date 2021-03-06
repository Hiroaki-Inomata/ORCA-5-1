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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2011/01/12  ��������������б�
      *  01.00.02    ����    2019/02/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4220.
      *    ����ǯ��
           03  SKT4220-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4220-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4220-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4220-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4220-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4220-TEL                 PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT4220-HOSPCD              PIC  X(07).
      *    �ݸ����ֹ�
           03  SKT4220-HKNJANUM            PIC  X(06).
      *    �ݸ���̾
           03  SKT4220-HKNJANAME           PIC  X(20).
      *    �����ǡ��߰���
           03  SKT4220-ZAI1                PIC  X(02).
           03  SKT4220-ZAI2                PIC  X(02).
      *    Ĺ��
           03  SKT4220-TYOKI1              PIC  X(02).
           03  SKT4220-TYOKI2              PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4220-NO                  PIC  X(04).
      *    ����������
           03  SKT4220-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT4220-SEIKYU-TBL  OCCURS  4.
      *    �����ơ��֥�
               05  SKT4220-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4220-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4220-NISSU       PIC  X(05).
      *    ����
                   07  SKT4220-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT4220-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4220-SKENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT4220-SNISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT4220-SKINGK          PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4220-SFTNGK          PIC  X(09).
      *    ����ơ��֥�
           03  SKT4220-KOH-TBL  OCCURS 4.
      *    ˡ���ֹ�
               05  SKT4220-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT4220-KOHNAME         PIC  X(08).
      *    �����ơ��֥�
               05  SKT4220-K-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4220-K-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4220-K-NISSU       PIC  X(05).
      *    ����
                   07  SKT4220-K-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT4220-K-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4220-K-SKENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT4220-K-SNISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT4220-K-SKINGK          PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4220-K-SFTNGK          PIC  X(09).
