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
      *  �������������ʲ�������     COPY (SKT3315.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT3315.
      *    ���ŵ��إ�����
           03       SKT3315-HOSPCD                PIC  X(14).
      *    �ڡ����ֹ�
           03       SKT3315-NO                    PIC  X(04).
      *    �ݸ���̾
           03       SKT3315-HKNJANAME             PIC  X(20).
      *    ����ǯ��
           03       SKT3315-SRYYM                 PIC  X(16).
      *    ����ǯ����
           03       SKT3315-SEIYMD                PIC  X(22).
      *    ���� 
           03       SKT3315-ADRS                  PIC  X(100).
      *    ���ŵ���̾
           03       SKT3315-HOSPNAME              PIC  X(100).
      *    ���߼�̾
           03       SKT3315-KAISETUNAME           PIC  X(40).
      *    �����ֹ�
           03       SKT3315-TEL                   PIC  X(15).
      *    ����ơ��֥�
           03       SKT3315-HKN-TBL.
               05  SKT3315-SEIKYU-TBL  OCCURS  2.      
                   07  SKT3315-NYUGAI-TBL  OCCURS  2.
                       09  SKT3315-KENSU                PIC  X(05).
                       09  SKT3315-NISSU                PIC  X(05).
                       09  SKT3315-TENSU                PIC  X(09).
                       09  SKT3315-ITBFTN               PIC  X(09).
                   07  SKT3315-S-KENSU                  PIC  X(05).
                   07  SKT3315-S-NISSU                  PIC  X(05).
                   07  SKT3315-S-KINGK                  PIC  X(09).
                   07  SKT3315-S-FTNGK                  PIC  X(09).
               05  SKT3315-ZAITAKU-TBL OCCURS 2.
                   07  SKT3315-ZKENSU                   PIC  X(03).
                   07  SKT3315-ZNISSU                   PIC  X(03).
                   07  SKT3315-ZTENSU                   PIC  X(09).
                   07  SKT3315-ZITBFTN                  PIC  X(09).
               05  SKT3315-KOH-TBL  OCCURS 5.
                   07  SKT3315-HBTNUM                  PIC  X(02).
                   07  SKT3315-KOHNAME                 PIC  X(10).
                   07  SKT3315-KNYUGAI-TBL  OCCURS  2.
                       09  SKT3315-KKENSU             PIC  X(05).
                       09  SKT3315-KNISSU             PIC  X(05).
                       09  SKT3315-KTENSU             PIC  X(09).
                       09  SKT3315-KITBFTN            PIC  X(09).
                   07  SKT3315-S-KKENSU                PIC  X(05).
                   07  SKT3315-S-KNISSU                PIC  X(05).
                   07  SKT3315-S-KKINGK                PIC  X(09).
                   07  SKT3315-S-KFTNGK                PIC  X(09).