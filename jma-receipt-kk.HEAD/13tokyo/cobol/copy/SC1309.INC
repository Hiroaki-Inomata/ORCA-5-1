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
       01  SC09.
      *���ŵ��إ�����
              03  SC09-HOSPCDN         PIC  X(20).
      *����ǯ��
              03  SC09-PRTYM           PIC  X(16).
      *����ǯ����
              03  SC09-SEIYMD          PIC  X(22).
      *������ʬɽ�������
              03  SC09-TENHYOKBN1      PIC  X(02).
      *����
              03  SC09-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC09-HOSPNAME        PIC  X(100).
      *�����ֹ�
              03  SC09-TEL             PIC  X(15).
      *������̾
              03  SC09-KAISETUNAME     PIC  X(40).
      *
      *��ô���ֹ�
              03  SC09-FTNJA-TBL.
                  05  SC09-FTNJANUM    PIC  X(08) OCCURS 10.
      *������ֹ�
              03  SC09-JKYSNUM-TBL.
                  05  SC09-JKYSNUM     PIC  X(14) OCCURS 10.
      *�ݸ����ֹ�
              03  SC09-HKNJANUM-TBL.
                  05  SC09-HKNJANUM    PIC  X(16) OCCURS 10.
      *���Ի�̾
              03  SC09-KANANAME-TBL.
                  05  SC09-KANANAME    PIC  X(20) OCCURS 10.
              03  SC09-NAME-TBL.
                  05  SC09-NAME        PIC  X(20) OCCURS 10.
      *��ô���
              03  SC09-FTNRATE-TBL.
                  05  SC09-FTNRATE     PIC  X(02) OCCURS 10.
      *����������
              03  SC09-NYUGAI-TBL.
                  05  SC09-NYUGAI      PIC  X(02) OCCURS 10.
      *��������
              03  SC09-NISSU-TBL.
                  05  SC09-NISSU       PIC  X(02) OCCURS 10.
      *�����
              03  SC09-SEIKYUGK-TBL.
                  05  SC09-SEIKYUGK    PIC  X(07) OCCURS 10.
      *����ʬ����
              03  SC09-TENSU-TBL.
                  05  SC09-TENSU       PIC  X(07) OCCURS 10.
      *������
              03  SC09-BIKO-TBL OCCURS 10.
                  05  SC09-BIKO1       PIC  X(02) OCCURS 2.
                  05  SC09-BIKO2       PIC  X(02) OCCURS 2.
      *���
              03  SC09-GOKEI-TBL.
      *��׷��
                  05  SC09-GKENSU      PIC  X(02).
      *��������
                  05  SC09-GSEIKYUGK   PIC  X(07).
