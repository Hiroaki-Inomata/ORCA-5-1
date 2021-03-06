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
      *  01.00.02    ���      2018/11/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT1930.
      *    NO
           03  SKT1930-NO                  PIC  X(04).
      *    ����
           03  SKT1930-ZAI1                PIC  X(02).
           03  SKT1930-ZAI2                PIC  X(02).
      *    ����������
           03  SKT1930-TOKUBETU            PIC  X(10).
      *    Ϸ������ơ��֥�
           03  SKT1930-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT1930-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT1930-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT1930-NISSU       PIC  X(05).
      *    ����
                   07  SKT1930-TENSU       PIC  X(09).
      *    ���ް�����ô��
                   07  SKT1930-YKZFTN      PIC  X(07).
      *    ������ô��
                   07  SKT1930-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT1930-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT1930-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT1930-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT1930-S-FTNGK         PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT1930-KOUHI-TBL  OCCURS  3.
      *    ˡ���ֹ�
               05  SKT1930-HBTNUM           PIC  X(02).
      *    ����̾
               05  SKT1930-KOHNAME          PIC  X(10).
      *    ���
               05  SKT1930-KKENSU           PIC  X(05).
      *    ���ż�����
               05  SKT1930-KNISSU           PIC  X(05).
      *    ����   
               05  SKT1930-KTENSU           PIC  X(09).
      *    ���ް�����ô��
               05  SKT1930-KYKZFTN          PIC  X(07).
      *    ������ô��
               05  SKT1930-KITBFTN          PIC  X(09).
      *    �������ܡ����
               05  SKT1930-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT1930-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT1930-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT1930-S-KFTNGK         PIC  X(09).
