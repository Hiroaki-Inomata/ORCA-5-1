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
      *  01.00.02    ����      2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2030.
      *    �ݸ����ֹ�
           03  SKT2030-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT2030-HOSPCD              PIC  X(14).
      *    ����
           03  SKT2030-ZAI1                PIC  X(02).
           03  SKT2030-ZAI2                PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT2030-NO                  PIC  X(04).
      *    ����������
           03  SKT2030-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT2030-SEIKYU-TBL     OCCURS  2.
      *    �����ơ��֥�
               05  SKT2030-NYUGAI-TBL      OCCURS   2.
      *    ���
                   07  SKT2030-KENSU       PIC  X(08).
      *    ������
                   07  SKT2030-NISSU       PIC  X(08).
      *    ����
                   07  SKT2030-TENSU       PIC  X(16).
      *    ������ô��
                   07  SKT2030-ITBFTN      PIC  X(16).
      *    �������ܡ����
               05  SKT2030-SKENSU          PIC  X(08).
      *    �������ܡ�����
               05  SKT2030-SNISSU          PIC  X(08).
      *    �������ܡ����
               05  SKT2030-SKINGK          PIC  X(16).
      *    �������ܡ�ɸ����ô��
               05  SKT2030-SFTNGK          PIC  X(16).
      *    ����ơ��֥�
           03  SKT2030-KOH-TBL     OCCURS  6.
      *    ����ơ��֥�
               05  SKT2030-KOHNUM          PIC  X(04).
      *    ����̾
               05  SKT2030-KOHNAME         PIC  X(12).
      *    �����ơ��֥�
               05  SKT2030-K-NYUGAI-TBL      OCCURS   2.
      *    ���
                   07  SKT2030-K-KENSU     PIC  X(08).
      *    ������
                   07  SKT2030-K-NISSU     PIC  X(08).
      *    ����
                   07  SKT2030-K-TENSU     PIC  X(16).
      *    ������ô��
                   07  SKT2030-K-ITBFTN    PIC  X(16).
      *    �������ܡ����
               05  SKT2030-K-SKENSU        PIC  X(08).
      *    �������ܡ�����
               05  SKT2030-K-SNISSU        PIC  X(08).
      *    �������ܡ����
               05  SKT2030-K-SKINGK        PIC  X(16).
      *    �������ܡ�ɸ����ô��
               05  SKT2030-K-SFTNGK        PIC  X(16).
      *    Ĺ������
           03  SKT2030-TYOKI-TBL  OCCURS  3.
               05  SKT2030-T-KENSU         PIC  X(06).
