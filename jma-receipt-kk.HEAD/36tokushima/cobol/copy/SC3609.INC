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
      *ɽ��ǯ��
              03  SC09-PRTYY		PIC  X(04).
              03  SC09-PRTMM		PIC  X(04).
      *����ǯ����
              03  SC09-SEIYY            PIC  X(04).
              03  SC09-SEIMM            PIC  X(04).
              03  SC09-SEIDD            PIC  X(04).
      *���ŵ��إ�����
              03  SC09-HOSPCDN		PIC  X(20).
      *͹���ֹ�
              03  SC09-POST             PIC  X(08).
      *���ŵ��ؽ���
              03  SC09-HOSPADRS         PIC  X(100).
      *���ŵ���̾��
              03  SC09-HOSPNAME         PIC  X(100).
      *�����ֹ�
              03  SC09-TEL              PIC  X(15).
      *���߼�̾
              03  SC09-KAISETU          PIC  X(40).
      *����ɽ��ʬ
              03  SC09-TENSUHYO         PIC  X(02) OCCURS 3.
      *��Į¼̾
              03  SC09-CITYNAME         PIC  X(20).
      *��ô���ֹ�
              03  SC09-FTNJANUM         PIC  X(16).
      *����Ի�̾
              03  SC09-KANANAME1        PIC  X(30).
              03  SC09-NAME1            PIC  X(20).
      *���Ļ���̾
              03  SC09-KANANAME2        PIC  X(30).
              03  SC09-NAME2            PIC  X(20).
      *����ǯ����
              03  SC09-TANJOGYMD        PIC  X(22).
      *ͭ������
              03  SC09-YUKOEDYY         PIC  X(04).
              03  SC09-YUKOEDMM         PIC  X(04).
              03  SC09-YUKOEDDD         PIC  X(04).
      *������ֹ�
              03  SC09-JKYSNUM          PIC  X(20).
      *���桦�ֹ�
              03  SC09-KIGO             PIC  X(20).
              03  SC09-BANGO            PIC  X(20).
      *�ݸ���̾
              03  SC09-HKNJANAME        PIC  X(20).
      *���ճ��
              03  SC09-KYURATE          PIC  X(02) OCCURS 3.
      *�ݸ�����
              03  SC09-HKNSYUBETU       PIC  X(02) OCCURS 4.
      *����������ʬ
              03  SC09-KOUGAKU          PIC  X(02) OCCURS 3.
      *��������
              03  SC09-SEIKYU-TBL OCCURS 2.
      *����
                  05  SC09-NISSU        PIC  X(02).
      *����
                  05  SC09-TENSU        PIC  X(07).
      *¾ˡ��ô����
                  05  SC09-TAHO         PIC  X(06).
      *���ٿ��Ⱦ㳲���������������
                  05  SC09-SEIKYUGK     PIC  X(09).
