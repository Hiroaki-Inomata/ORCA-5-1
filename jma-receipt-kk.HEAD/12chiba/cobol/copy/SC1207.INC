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
       01  SC07.
      *���ŵ��إ�����
           03  SC07-HOSPCD            PIC  X(14).
      *��������
           03  SC07-PRTYY             PIC  X(02).
           03  SC07-PRTMM             PIC  X(02).
      *���ŵ��ؽ���
           03  SC07-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC07-HOSPNAME          PIC  X(100).
      *���ŵ��������ֹ�
           03  SC07-TEL               PIC  X(15).
      *���ŵ��س��߼Ի�̾
           03  SC07-KAISETU           PIC  X(40).
      *����ɼ��ʬ
           03  SC07-TENSUHYOG         OCCURS 2.
               05  SC07-TENSUHYO      PIC  X(02).
      *�����ơ��֥�
           03  SC07-SEIKYU-TBL.
               05  SC07-NYUGAI-TBL OCCURS 2.
      *�����
                   07  SC07-KENSU     PIC  X(07).
      *������
                   07  SC07-TENSU     PIC  X(11).
      *��ô��
                   07  SC07-FTNMONEY  PIC  X(10).
      *����������-���
               05  SC07-S-KENSU        PIC  X(07).
      *����������-���
               05  SC07-S-KINGK        PIC  X(11).
      *����������-ɸ����ô
               05  SC07-S-FTNGK        PIC  X(10).
      *��ץơ��֥�
           03  SC07-GOKEI-TBL.
      *��������
               05  SC07-GKENSU         PIC  X(07).
      *���������
               05  SC07-GTENSU         PIC  X(11).
      *�����ô��
               05  SC07-GFTNMONEY      PIC  X(10).
      *�������ܹ�׷��
               05  SC07-S-GKENSU       PIC  X(07).
      *�������ܹ�׶��
               05  SC07-S-GKINGK       PIC  X(11).
      *�������ܹ��ɸ����ô��
               05  SC07-S-GFTNGK       PIC  X(10).
      *��ʬ
           03  SC07-KBN-TBL OCCURS 4.
               05  SC07-KBN            PIC  X(02).

