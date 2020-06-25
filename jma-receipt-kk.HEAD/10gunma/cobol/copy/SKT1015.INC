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
      *  �������������ʷ��ϸ���     COPY (SKT1015.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/05/22  ����̾�ϰ������ʤ�
      *  01.00.02    ����    2013/07/09  ��������η������
      ********************************************************************
       01  SKT1015.
      *    �ݸ����ֹ�
           03       SKT1015-HKNJANUM              PIC  X(04).
      *    �ݸ���̾
           03       SKT1015-HKNJANAME             PIC  X(20).
      *    ���ŵ��إ�����
           03       SKT1015-HOSPCD                PIC  X(14).
      *    �ڡ����ֹ�
           03       SKT1015-NO                    PIC  X(04).
      *    ����ǯ��
           03       SKT1015-SRYYM                 PIC  X(16).
      *    ����ǯ����
           03       SKT1015-SEIYMD                PIC  X(22).
      *    ���� 
           03       SKT1015-ADRS                  PIC  X(100).
      *    ���ŵ���̾
           03       SKT1015-HOSPNAME              PIC  X(100).
      *    ���߼�̾
           03       SKT1015-KAISETUNAME           PIC  X(40).
      *    �����ֹ�
           03       SKT1015-TEL                   PIC  X(15).
      *    ������
           03       SKT1015-ZAI1                  PIC  X(02).
           03       SKT1015-ZAI2                  PIC  X(02).
      *    ����ơ��֥�
           03       SKT1015-HKN-TBL.
               05  SKT1015-ROUHO-TBL  OCCURS  2.      
                   07  SKT1015-NYUGAI-TBL  OCCURS  2.
                       09  SKT1015-KENSU                PIC  X(05).
                       09  SKT1015-NISSU                PIC  X(05).
                       09  SKT1015-TENSU                PIC  X(09).
                       09  SKT1015-ITBFTN               PIC  X(09).
                   07  SKT1015-S-KENSU                  PIC  X(05).
                   07  SKT1015-S-NISSU                  PIC  X(06).
                   07  SKT1015-S-KINGK                  PIC  X(09).
                   07  SKT1015-S-FTNGK                  PIC  X(09).
               05  SKT1015-KOH-TBL  OCCURS 4.
                   07  SKT1015-HBTNUM                  PIC  X(02).
                   07  SKT1015-NYUGAI-TBL  OCCURS  2.
                       09  SKT1015-KKENSU             PIC  X(05).
                       09  SKT1015-KNISSU             PIC  X(05).
                       09  SKT1015-KTENSU             PIC  X(09).
                       09  SKT1015-KITBFTN            PIC  X(09).
                   07  SKT1015-S-KKENSU                PIC  X(05).
                   07  SKT1015-S-KNISSU                PIC  X(06).
                   07  SKT1015-S-KKINGK                PIC  X(09).
                   07  SKT1015-S-KFTNGK                PIC  X(09).