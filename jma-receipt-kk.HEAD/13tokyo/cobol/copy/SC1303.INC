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
      *���ŵ��إ�����
              03  SC03-HOSPCDN         PIC  X(20).
      *����ǯ��
              03  SC03-PRTYM           PIC  X(16).
      *����ǯ����
              03  SC03-SEIYMD          PIC  X(22).
      *������ʬɽ�������
              03  SC03-TENHYOKBN1      PIC  X(02).
      *������ʬɽ��������
              03  SC03-TENHYOKBN2      PIC  X(02).
      *������ʬɽ����Ĵ��
              03  SC03-TENHYOKBN3      PIC  X(02).
      *������ʬɽ����ˬ��
              03  SC03-TENHYOKBN4      PIC  X(02).
      *����
              03  SC03-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC03-HOSPNAME        PIC  X(100).
      *�����ֹ�
              03  SC03-TEL             PIC  X(15).
      *������̾
              03  SC03-KAISETUNAME     PIC  X(40).
      *����ơ��֥�
              03  SC03-SEIKYU-TBL   OCCURS  10.
      *��ô���ֹ�
                  05  SC03-FTNJANUM    PIC  X(08).
      *������ֹ�
                  05  SC03-JKYSNUM     PIC  X(12).
      *�ݸ����ֹ�
                  05  SC03-CITYNUM     PIC  X(06).
                  05  SC03-HKNJANUM    PIC  X(16).
      *����Ի�̾
                  05  SC03-KANANAME    PIC  X(20).
                  05  SC03-NAME        PIC  X(20).
      *��ô���
                  05  SC03-KYURATE1    PIC  X(02).
                  05  SC03-KYURATE2    PIC  X(02).
      *������ʬ
                  05  SC03-NYUGAIKBN1  PIC  X(02).
                  05  SC03-NYUGAIKBN2  PIC  X(02).
      *��������
                  05  SC03-NISSU       PIC  X(02).
      *�����
                  05  SC03-SEIKYU      PIC  X(07).
      *������������ôʬ
                  05  SC03-SYKFTN1     PIC  X(06).
      *������
                  05  SC03-TENSU       PIC  X(07).
      *������ô��
                  05  SC03-ITBFTN      PIC  X(07).
      *����:����
                  05  SC03-BIKO1-1     PIC  X(02).
                  05  SC03-BIKO1-2     PIC  X(02).
      *���͡�����
                  05  SC03-BIKO2-1     PIC  X(02).
                  05  SC03-BIKO2-2     PIC  X(02).
      *��׷��
              03  SC03-SKENSU	       PIC  X(02).
      *��������
              03  SC03-SSEIKYU	       PIC  X(07).
      *��������
              03  SC03-SYOKU-TBL OCCURS 10.
                  05  SC03-SYOKU       PIC  X(02).

