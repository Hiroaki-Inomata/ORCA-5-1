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
      *  01.00.01    ���    2019/05/21  �����б�
      ********************************************************************
       01  SC07.
      *����ǯ��
           03  SC07-SRYGG             PIC  X(04).
           03  SC07-SRYYY             PIC  X(04).
           03  SC07-SRYMM             PIC  X(04).
      *��������
           03  SC07-SEIYMD            PIC  X(22).
      *���ŵ��إ�����
           03  SC07-HOSPCD            PIC  X(14).
      *��Į¼�ֹ�
           03  SC07-CITYNUM           PIC  X(12).
      *��Į¼̾
           03  SC07-CITYNAME          PIC  X(20).
      *���ŵ��ؽ���
           03  SC07-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC07-HOSPNAME          PIC  X(100).
      *�����ֹ�
           03  SC07-TEL               PIC  X(15).
      *���߼�̾
           03  SC07-KAISETU           PIC  X(40).
      *���
           03  SC07-MAISU             PIC  X(12).
      *���
           03  SC07-KENSU             PIC  X(12).
      *������
           03  SC07-TENSU             PIC  X(16).
      *������ô��
           03  SC07-ITBFTN            PIC  X(14).
      *ɸ����ô��
           03  SC07-FTNGK             PIC  X(16).
