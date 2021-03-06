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
       01  SKT4230.
      *    ���ŵ��إ�����
           03  SKT4230-HOSPCD              PIC  X(07).
      *    �����ǡ��߰���
           03  SKT4230-ZAI1                PIC  X(02).
           03  SKT4230-ZAI2                PIC  X(02).
      *    Ĺ��
           03  SKT4230-TYOKI1              PIC  X(02).
           03  SKT4230-TYOKI2              PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4230-NO                  PIC  X(04).
      *    ����������
           03  SKT4230-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�(����������)
           03  SKT4230-GENBK-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT4230-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4230-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4230-NISSU       PIC  X(05).
      *    ����
                   07  SKT4230-TENSU       PIC  X(09).
      *    ���ް�����ô��
                   07  SKT4230-YKZFTN      PIC  X(09).
      *    ������ô��
                   07  SKT4230-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4230-SKENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT4230-SNISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT4230-SKINGK          PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4230-SFTNGK          PIC  X(09).
      *    ����ơ��֥�(����¾����)
           03  SKT4230-KOH-TBL  OCCURS  8.
      *    ˡ��
               05  SKT4230-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT4230-KOHNAME         PIC  X(08).
      *    ˡ��
               05  SKT4230-HOUSEI          PIC  X(02).
      *    �����ơ��֥�
               05  SKT4230-K-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4230-K-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4230-K-NISSU       PIC  X(05).
      *    ����
                   07  SKT4230-K-TENSU       PIC  X(09).
      *    ���ް�����ô��
                   07  SKT4230-K-YKZFTN      PIC  X(09).
      *    ������ô��
                   07  SKT4230-K-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4230-K-SKENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT4230-K-SNISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT4230-K-SKINGK          PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4230-K-SFTNGK          PIC  X(09).
