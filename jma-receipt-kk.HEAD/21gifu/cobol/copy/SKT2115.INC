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
      *  �������������ʴ��츩��     COPY (SKT2115.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT2115.
      *    NO
           03  SKT2115-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT2115-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT2115-SEIYMD              PIC  X(22).
      *    ����
           03  SKT2115-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT2115-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT2115-KAISETUNAME         PIC  X(60).
      *    �����ֹ�
           03  SKT2115-TEL                 PIC  X(15).
      *    �ݸ���̾
           03  SKT2115-HKNJANAME           PIC  X(100).
      *    �ݸ����ֹ�
           03  SKT2115-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT2115-HOSPCD              PIC  X(14).
      *    ����
           03  SKT2115-ZAI1                PIC  X(02).
           03  SKT2115-ZAI2                PIC  X(02).
      *    ���Ų�
           03  SKT2115-SRYKA               PIC  X(04).
      *    ����ơ��֥�
           03  SKT2115-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT2115-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2115-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2115-NISSU       PIC  X(05).
      *    ����
                   07  SKT2115-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2115-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2115-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT2115-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT2115-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2115-S-FTNGK         PIC  X(09).
      *    ����ơ��֥�
           03  SKT2115-KOH-TBL  OCCURS  3.
      *    ����̾
               05  SKT2115-KOHNAME         PIC  X(10).
      *    �����ơ��֥�
               05  SKT2115-KNYUGAI-TBL   OCCURS  2.
      *    ���
                   07  SKT2115-KKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT2115-KNISSU      PIC  X(05).
      *    ����
                   07  SKT2115-KTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT2115-KITBFTN     PIC  X(09).
      *    �������ܡ����
               05  SKT2115-S-KKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT2115-S-KNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT2115-S-KKINGK        PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2115-S-KFTNGK        PIC  X(09).
      *    Ĺ�������
           03  SKT2115-TYOKI-TBL  OCCURS  2.
               05  SKT2115-TKENSU          PIC  X(02).
      *    Ĺ�衧���
           03  SKT2115-TYOKIT-TBL  OCCURS  2.
               05  SKT2115-TTKENSU         PIC  X(02).
