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
       01  SC07.
      *    ����ǯ����
           03  SC07-SEIYMD             PIC  X(22).
      *    ���ǯ
           03  SC07-SEIY               PIC  X(04).
      *    ��з�
           03  SC07-SEIM               PIC  X(04).
      *    ���ŵ��إ�����
           03  SC07-HOSPCD             PIC  X(20).
      *    ͹���ֹ�
           03  SC07-POST               PIC  X(10).
      *    ���ŵ��ؽ���
           03  SC07-ADRS               PIC  X(100).
      *    ���ŵ���̾��
           03  SC07-HOSPNAME           PIC  X(100).
      *    ���߼Ի�̾
           03  SC07-KAISETUNAME        PIC  X(40).
      *    �����ֹ�
           03  SC07-TEL                PIC  X(15).
      *    ����ơ��֥�
           03  SC07-SEIKYU-TBL  OCCURS 4.
      *    �쥻�ץȼ̤����
               05  SC07-KENSU          PIC  X(06).
      *    �������
               05  SC07-TENSU          PIC  X(10).
      *    ô����
           03  SC07-TANTOU             PIC  X(40).
      *    ����
           03  SC07-SEIGENGO           PIC  X(04).