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
       01  SC03.
      *ɽ��ǯ��
              03  SC03-PRTYM            PIC  X(16).
      *����ǯ����
              03  SC03-SEIYMD           PIC  X(22).
      *���ŵ��إ�����
              03  SC03-HOSPCDN          PIC  X(20).
      *�ڡ����ֹ�
              03  SC03-PAGE             PIC  X(02).
      *����
              03  SC03-ADRS             PIC  X(100).
      *���ŵ���̾
              03  SC03-HOSPNAME	        PIC  X(100).
      *������̾
              03  SC03-KAISETU          PIC  X(40).
      *�����ֹ�
              03  SC03-TEL              PIC  X(15).
      *�ơ��֥�
              03  SC03-SRY-TBL          OCCURS  10.
      *��ô���ֹ�(�売��)
                  05  SC03-FTNJANUM1    PIC  X(04).
      *��ô���ֹ�(������)
                  05  SC03-FTNJANUM2    PIC  X(08).
      *������ֹ�
                  05  SC03-JKYSNUM      PIC  X(14).
      *����Ի�̾
                  05  SC03-NAME         PIC  X(20).
      *���ݸ��Ծڡ����桦�ֹ�
                  05  SC03-KIGO	        PIC  X(20).
                  05  SC03-NUM          PIC  X(20).
      *��������/ǯ��
                  05  SC03-NYUGAI-TBL   OCCURS  2.
                      07  SC03-AGE-TBL  OCCURS  5.
                          09  SC03-KBN  PIC  X(02).
      *����ǯ��
                  05  SC03-SRYYM        PIC  X(08).
      *����
                  05  SC03-NISSU        PIC  X(02).
      *��������
                  05  SC03-TENSU        PIC  X(07).
      *����ʬ�������������������
                  05  SC03-KOHTENSU     PIC  X(07).
      *������ô��
                  05  SC03-ITBFTN       PIC  X(06).
      *��������
                  05  SC03-SNISSU       PIC  X(02).
      *����������
                  05  SC03-SKINGK       PIC  X(06).
      *����������ɸ����ô��
                  05  SC03-SFTNGK       PIC  X(06).
      *Ĺ������
                  05  SC03-TYOKI        PIC  X(02).
      *����
                  05  SC03-BIKO1        PIC  X(02).
                  05  SC03-BIKO2        PIC  X(02).
                  05  SC03-BIKO3        PIC  X(02).
                  05  SC03-BIKO4        PIC  X(02).
                  05  SC03-BIKO5        PIC  X(06).
