      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         15/01/05  ô��������ɲ�
      *  01.00.02    ���         19/04/17  �����б�
      ********************************************************************
       01  SC09.
      *    ����ǯ����
           03  SC09-SEIYMD             PIC  X(22).
      *    ���ǯ
           03  SC09-SEIY               PIC  X(04).
      *    ��з�
           03  SC09-SEIM               PIC  X(04).
      *    ���ŵ��إ�����
           03  SC09-HOSPCD             PIC  X(20).
      *    ͹���ֹ�
           03  SC09-POST               PIC  X(10).
      *    ���ŵ��ؽ���
           03  SC09-ADRS               PIC  X(100).
      *    ���ŵ���̾��
           03  SC09-HOSPNAME           PIC  X(100).
      *    ���߼Ի�̾
           03  SC09-KAISETUNAME        PIC  X(40).
      *    �����ֹ�
           03  SC09-TEL                PIC  X(15).
      *    ����ơ��֥�
           03  SC09-SEIKYU-TBL  OCCURS  27.
      *    ������ô���ֹ�
               05  SC09-FTNJANUM       PIC  X(04).
      *    ��Į¼̾
               05  SC09-CITYNAME       PIC  X(12).
      *    ��׷��
               05  SC09-KENSU          PIC  X(05).
      *    �������
               05  SC09-TENSU          PIC  X(09).
      *    ����ס����
           03  SC09-GKENSU             PIC  X(06).
      *    ����ס�����
           03  SC09-GTENSU             PIC  X(10).
      *    ô����
           03  SC09-TANTOU             PIC  X(40).
      *    ����
           03  SC09-SEIGENGO           PIC  X(04).
