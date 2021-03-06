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
       01  SC05.
      *���ŵ��إ�����
              03  SC05-HOSPCDN         PIC  X(20).
      *����ǯ��
              03  SC05-PRTYM           PIC  X(16).
      *����ǯ����
              03  SC05-SEIYMD          PIC  X(22).
      *������ʬɽ�������
              03  SC05-TENHYOKBN1      PIC  X(02).
      *������ʬɽ��������
              03  SC05-TENHYOKBN2      PIC  X(02).
      *������ʬɽ����Ĵ��
              03  SC05-TENHYOKBN3      PIC  X(02).
      *������ʬɽ����ˬ��
              03  SC05-TENHYOKBN4      PIC  X(02).
      *����
              03  SC05-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC05-HOSPNAME        PIC  X(100).
      *�����ֹ�
              03  SC05-TEL             PIC  X(15).
      *������̾
              03  SC05-KAISETUNAME     PIC  X(40).
      *����ơ��֥�
              03  SC05-SEIKYU-TBL   OCCURS  10.
      *��ô���ֹ�
                  05  SC05-FTNJANUM    PIC  X(08).
      *������ֹ�
                  05  SC05-JKYSNUM     PIC  X(12).
      *�ݸ����ֹ�
                  05  SC05-HKNJANUM    PIC  X(16).
      *����Ի�̾
                  05  SC05-KANANAME    PIC  X(20).
                  05  SC05-NAME        PIC  X(20).
      *�ܿͲ�²��ʬ
                  05  SC05-KYURATE1    PIC  X(02).
                  05  SC05-KYURATE2    PIC  X(02).
      *������ʬ
                  05  SC05-NYUGAIKBN1  PIC  X(02).
                  05  SC05-NYUGAIKBN2  PIC  X(02).
      *��������
                  05  SC05-NISSU       PIC  X(02).
      *�����
                  05  SC05-SEIKYU      PIC  X(07).
      *����������������ôʬ
                  05  SC05-SYKYKZFTN   PIC  X(06).
      *������
                  05  SC05-TENSU       PIC  X(07).
      *������ô��
                  05  SC05-ITBFTN      PIC  X(07).
      *��׷��
              03  SC05-SKENSU		PIC  X(02).
      *��������
              03  SC05-SSEIKYU		PIC  X(07).
      *�������ܡ�����Ƚ�̵���
              03  SC05-SYKYKZ-TBL  OCCURS 10.
                  05  SC05-SYKYKZ       PIC  X(02).
      *������
              03  SC05-BIKO-TBL OCCURS 10.
      *����ʸ��
                  05  SC05-BIKO1        PIC  X(04).
      *����ʸ��
                  05  SC05-BIKO2        PIC  X(04).
      *���ʢ�
                  05  SC05-BIKO3        PIC  X(02).
      *���ʢ�
                  05  SC05-BIKO4        PIC  X(02).
