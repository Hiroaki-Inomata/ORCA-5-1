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
       01  SKT3430.
      *    NO
           03  SKT3430-NO                  PIC  X(04).
      *    ���ŵ��إ�����
           03  SKT3430-HOSPCD              PIC  X(20).
      *    �ݸ����ֹ�
           03  SKT3430-HKNJANUM            PIC  X(06).
      *    ����������
           03  SKT3430-TOKUBETU            PIC  X(10).
      *    ��ˣ���������ơ��֥�
           03  SKT3430-34-SEIKYU-TBL .
      *    �����ơ��֥�
               05  SKT3430-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3430-34KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3430-34NISSU       PIC  X(05).
      *    ����
                   07  SKT3430-34TENSU       PIC  X(09).
      *    �������ܡ����
               05  SKT3430-S-34KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3430-S-34NISSU         PIC  X(05).
      *    ��ˣ���������ơ��֥�
           03  SKT3430-35-SEIKYU-TBL.
      *    ���
               05  SKT3430-35KENSU       PIC  X(05).
      *    ���ż�����
               05  SKT3430-35NISSU       PIC  X(05).
      *    ����
               05  SKT3430-35TENSU       PIC  X(09).
      *    ������ô��
               05  SKT3430-35ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT3430-S-35KENSU     PIC  X(05).
      *    �������ܡ�����
               05  SKT3430-S-35NISSU     PIC  X(05).
      *    �������ܡ����
               05  SKT3430-S-35KINGK     PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT3430-KSEIKYU-TBL OCCURS 6.
      *    �����ơ��֥�
               05  SKT3430-KNYUGAI-TBL OCCURS 2.
      *    ���
                   07  SKT3430-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3430-KNISSU       PIC  X(05).
      *    ����
                   07  SKT3430-KTENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3430-KITBFTN      PIC  X(09).
      *    ����ˡ���ֹ�
               05  SKT3430-HBTNUM           PIC  X(02).
      *    ����ˡ���ֹ�(Ϸ����)
               05  SKT3430-RHBTNUM1         PIC  X(02).
               05  SKT3430-RHBTNUM2         PIC  X(02).
               05  SKT3430-RSEN             PIC  X(02).
      *    ����̾��
               05  SKT3430-KNAME            PIC  X(10).
      *    �������ܡ����
               05  SKT3430-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3430-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3430-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3430-S-KFTNGK         PIC  X(09).
