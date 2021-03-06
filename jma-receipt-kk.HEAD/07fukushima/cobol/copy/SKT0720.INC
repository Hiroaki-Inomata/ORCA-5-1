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
      *  01.00.01    ����    2011/02/09  ��������������б�
      *  01.00.02    ����    2018/10/10  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0720.
      *    ����ǯ��
      *     03  SKT0720-SRYYM             PIC  X(16).
      *    �ݸ����ֹ�
           03  SKT0720-HKNJANUM          PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT0720-HOSPCD            PIC  X(14).
      *    ����
           03  SKT0720-ZAI1              PIC  X(02).
           03  SKT0720-ZAI2              PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT0720-NO                PIC  X(04).
      *    ����������
           03  SKT0720-TOKUBETU          PIC  X(10).
      *    ����ơ��֥�
           03  SKT0720-KOH-TBL     OCCURS  14.
      *    ����̾
               05  SKT0720-KOHNAME       PIC  X(10).
      *    �����ơ��֥�
               05  SKT0720-NYUGAI-TBL     OCCURS  2.
      *    ���
                   07  SKT0720-KENSU     PIC  X(05).
      *    ������
                   07  SKT0720-NISSU     PIC  X(05).
      *    ����
                   07  SKT0720-TENSU     PIC  X(10).
      *    ������ô��
                   07  SKT0720-ITBFTN    PIC  X(09).
      *    �������ܡ����
               05  SKT0720-S-KENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT0720-S-NISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT0720-S-KINGAKU     PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0720-S-FTNGAKU     PIC  X(09).
