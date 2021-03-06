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
       01  SKT3420.
      *    NO
           03  SKT3420-NO                  PIC  X(04).
      *    ���ŵ��إ�����
           03  SKT3420-HOSPCD              PIC  X(20).
      *    �ݸ����ֹ�
           03  SKT3420-HKNJANUM            PIC  X(06).
      *    ����������
           03  SKT3420-TOKUBETU            PIC  X(10).
      *    Ϸ������ơ��֥�
           03  SKT3420-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT3420-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3420-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3420-NISSU       PIC  X(05).
      *    ����
                   07  SKT3420-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3420-ITBFTN      PIC  X(09).
      *    �������
                   07  SKT3420-GENBAKU     PIC  X(03).
      *    �������ܡ����
               05  SKT3420-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3420-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3420-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3420-S-FTNGK         PIC  X(09).
      *
      *    �����ơ��֥�
           03  SKT3420-GSEIKYU-TBL.
      *    �����ơ��֥�
               05  SKT3420-GNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3420-GKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT3420-GNISSU      PIC  X(05).
      *    ����
                   07  SKT3420-GTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT3420-GITBFTN     PIC  X(09).
      *    �������ܡ����
               05  SKT3420-S-GKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT3420-S-GNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT3420-S-GKINGK        PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3420-S-GFTNGK        PIC  X(09).
      *
      *    ʡ����ťơ��֥�
           03  SKT3420-FSEIKYU-TBL  OCCURS  5.
      *    �����ơ��֥�
               05  SKT3420-FNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3420-FKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT3420-FNISSU      PIC  X(05).
      *    ����
                   07  SKT3420-FTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT3420-FITBFTN     PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT3420-KSEIKYU-TBL OCCURS 3.
      *    �����ơ��֥�
               05  SKT3420-KNYUGAI-TBL OCCURS 2.
      *    ���
                   07  SKT3420-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3420-KNISSU       PIC  X(05).
      *    ����
                   07  SKT3420-KTENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3420-KITBFTN      PIC  X(09).
      *    ����ˡ���ֹ�
               05  SKT3420-HBTNUM           PIC  X(02).
      *    ����̾��
               05  SKT3420-KNAME            PIC  X(10).
      *    �������ܡ����
               05  SKT3420-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3420-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3420-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3420-S-KFTNGK         PIC  X(09).
      *
      *    Ĺ���������������ơ��֥�
           03  SKT3420-TYKSEIKYU-TBL.
      *    �ɾ���(���̡��࿦)�ơ��֥�
               05  SKT3420-KANSYO-TBL OCCURS 2.
      *    ���
                   07  SKT3420-TKENSU       PIC  X(05).
      *    ���
                   07  SKT3420-S-TKINGK     PIC  X(09).
