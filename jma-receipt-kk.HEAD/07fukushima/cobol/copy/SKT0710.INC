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
      *  �ץ�����ཤ������
      * Maj/Min/Rev 	������	����		����
      *  01.00.01    Ĺ��    08/04/29    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����    11/02/09    ��������������б�
      *  01.00.03    ����    18/10/10    �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0710.
      *    ����ǯ��
           03  SKT0710-SRYYM                PIC  X(16).
      *    ���ŵ��ؽ���
           03  SKT0710-ADRS                 PIC  X(100).
      *    ���ŵ���̾
           03  SKT0710-HOSPNAME             PIC  X(100).
      *    �����ֹ�
           03  SKT0710-TEL                  PIC  X(15).
      *    ���߼�̾
           03  SKT0710-KAISETUNAME          PIC  X(40).
      *    �ݸ���̾
           03  SKT0710-HKNJANAME            PIC  X(20).
      *    ����ǯ����
           03  SKT0710-SEIYMD               PIC  X(22).
      *    �ݸ����ֹ�
           03  SKT0710-HKNJANUM             PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT0710-HOSPCD               PIC  X(14).
      *    ����
           03  SKT0710-ZAI1                 PIC  X(02).
           03  SKT0710-ZAI2                 PIC  X(02).
      *    ��Ϸ��
           03  SKT0710-ROUJIN               PIC  X(10).
      *    �ڡ����ֹ�
           03  SKT0710-NO                   PIC  X(04).
      *    �죷���аʾ壹���б�
           03  SKT0710-KYUHANTEI            PIC  X(02).
      *    ����������
           03  SKT0710-TOKUBETU             PIC  X(10).
      *    ����ơ��֥�
           03  SKT0710-SEIKYU-TBL     OCCURS  14.
      *    �����ơ��֥�
               05  SKT0710-NYUGAI-TBL     OCCURS  2.
      *    ���
                   07  SKT0710-KENSU        PIC  X(05).
      *    ������
                   07  SKT0710-NISSU        PIC  X(05).
      *    ����
                   07  SKT0710-TENSU        PIC  X(10).
      *    ������ô��
                   07  SKT0710-ITBFTN       PIC  X(09).
      *    �������ܡ����
               05  SKT0710-S-KENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT0710-S-NISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT0710-S-KINGAKU        PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0710-S-FTNGAKU        PIC  X(09).
      *    ����ơ��֥�
           03  SKT0710-KOH-TBL     OCCURS  5.
      *    ����̾
               05  SKT0710-KOHNAME          PIC  X(08).
      *    �����ơ��֥�
               05  SKT0710-K-NYUGAI-TBL     OCCURS  2.
      *    ���
                   07  SKT0710-KKENSU       PIC  X(05).
      *    ������
                   07  SKT0710-KNISSU       PIC  X(05).
      *    ����
                   07  SKT0710-KTENSU       PIC  X(10).
      *    ������ô��
                   07  SKT0710-KITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT0710-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT0710-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT0710-S-KKINGAKU       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0710-S-KFTNGAKU       PIC  X(09).
