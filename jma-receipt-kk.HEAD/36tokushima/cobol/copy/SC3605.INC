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
      *ɽ��ǯ��
              03  SC05-PRTYY		PIC  X(04).
              03  SC05-PRTMM		PIC  X(04).
      *����ǯ����
              03  SC05-SEIYY            PIC  X(04).
              03  SC05-SEIMM            PIC  X(04).
              03  SC05-SEIDD            PIC  X(04).
      *���ŵ��إ�����
              03  SC05-HOSPCDN		PIC  X(20).
      *͹���ֹ�
              03  SC05-POST             PIC  X(08).
      *���ŵ��ؽ���
              03  SC05-HOSPADRS         PIC  X(100).
      *���ŵ���̾��
              03  SC05-HOSPNAME         PIC  X(100).
      *�����ֹ�
              03  SC05-TEL              PIC  X(15).
      *���߼�̾
              03  SC05-KAISETU          PIC  X(40).
      *����ɽ��ʬ
              03  SC05-TENSUHYO         PIC  X(02) OCCURS 3.
      *��Į¼̾
              03  SC05-CITYNAME         PIC  X(20).
      *��ô���ֹ�
              03  SC05-FTNJANUM         PIC  X(16).
      *��̾
              03  SC05-KANANAME         PIC  X(30).
              03  SC05-NAME             PIC  X(20).
      *����ǯ
              03  SC05-TANJOGY          PIC  X(08).
      *������ֹ�
              03  SC05-JKYSNUM-KIGO     PIC  X(02).
              03  SC05-JKYSNUM          PIC  X(14).
      *���桦�ֹ�
              03  SC05-KIGO             PIC  X(20).
              03  SC05-BANGO            PIC  X(20).
      *����
              03  SC05-SEX              PIC  X(02).
      *�ݸ���̾
              03  SC05-HKNJANAME        PIC  X(30).
      *���ճ��
              03  SC05-KYURATE          PIC  X(02) OCCURS 4.
      *�ݸ�����
              03  SC05-HKNSYUBETU       PIC  X(02) OCCURS 4.
      *����������ʬ
              03  SC05-KOUGAKU          PIC  X(02) OCCURS 3.
      *��������
              03  SC05-SEIKYU-TBL OCCURS 2.
      *����
                  05  SC05-NISSU        PIC  X(02).
      *����
                  05  SC05-TENSU        PIC  X(07).
      *���ް�����ô��
                  05  SC05-YKZFTN       PIC  X(06).
      *¾ˡ��ô����
                  05  SC05-TAHO         PIC  X(06).
      *���ٿ��Ⱦ㳲���������������
                  05  SC05-SEIKYUGK     PIC  X(09).
