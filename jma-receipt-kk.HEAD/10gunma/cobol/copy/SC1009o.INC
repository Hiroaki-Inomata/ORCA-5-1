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
       01  SC09o.
      *    ��������
           03  SC09o-PRTYM               PIC  X(16).
      *    ��Į¼̾
           03  SC09o-CITYNAME            PIC  X(20).
      *    ��������
           03  SC09o-SEIYMD              PIC  X(22).
      *    ͹���ֹ�
      *     03  SC09o-POST                PIC  X(18).
      *    �����
           03  SC09o-ADRS                PIC  X(100).
      *    ���ŵ���̾��
           03  SC09o-HOSPNAME            PIC  X(100).
      *    �����ֹ�
           03  SC09o-TEL                 PIC  X(15).
      *    ���߼Ի�̾
           03  SC09o-KAISETUNAME         PIC  X(40).
      *    �ݸ����ֹ�
           03  SC09o-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SC09o-HOSPCD              PIC  X(14).
      *    ����ɽ��ʬ
           03  SC09o-TENHYOKBN-TBL        OCCURS  4.
               05  SC09o-TENHYOKBN       PIC  X(02).
      *    ����¾�����ֹ�
           03  SC09o-ETCKOHNUM           PIC  X(02).
      *    ����ơ��֥�
           03  SC09o-SEIKYU-TBL      OCCURS  22.
      *    ���
               05  SC09o-KENSU           PIC  X(05).
      *    ����
               05  SC09o-TENSU           PIC  X(09).
      *    ʡ������������
               05  SC09o-FKSGAKU         PIC  X(10).
      *    �������ܡ����
               05  SC09o-S-KENSU         PIC  X(05).
      *    �������ܡ�ɸ����ô��
               05  SC09o-S-SKJFTN        PIC  X(09).
