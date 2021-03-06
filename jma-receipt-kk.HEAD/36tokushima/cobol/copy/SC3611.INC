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
       01  SC11.
      *ɽ��ǯ��
              03  SC11-PRTYY		PIC  X(04).
              03  SC11-PRTMM		PIC  X(04).
      *����ǯ����
              03  SC11-SEIYY            PIC  X(04).
              03  SC11-SEIMM            PIC  X(04).
              03  SC11-SEIDD            PIC  X(04).
      *���ŵ��إ�����
              03  SC11-HOSPCDN		PIC  X(20).
      *͹���ֹ�
              03  SC11-POST             PIC  X(08).
      *���ŵ��ؽ���
              03  SC11-HOSPADRS         PIC  X(100).
      *���ŵ���̾��
              03  SC11-HOSPNAME         PIC  X(100).
      *�����ֹ�
              03  SC11-TEL              PIC  X(15).
      *���߼�̾
              03  SC11-KAISETU          PIC  X(40).
      *����ɽ��ʬ
              03  SC11-TENSUHYO         PIC  X(02) OCCURS 3.
      *��Į¼̾
              03  SC11-CITYNAME         PIC  X(20).
      *��ô���ֹ�
              03  SC11-FTNJANUM         PIC  X(16).
      *����Ի�̾
              03  SC11-KANANAME1        PIC  X(30).
              03  SC11-NAME1            PIC  X(20).
      *���Ļ���̾
              03  SC11-KANANAME2        PIC  X(30).
              03  SC11-NAME2            PIC  X(20).
      *����ǯ����
      *        03  SC11-TANJOYY          PIC  X(04).
      *        03  SC11-TANJOMM          PIC  X(04).
      *        03  SC11-TANJODD          PIC  X(04).
              03  SC11-BIRTHDAY         PIC  X(22).
      *ͭ������
              03  SC11-YUKOEDYY         PIC  X(04).
              03  SC11-YUKOEDMM         PIC  X(04).
              03  SC11-YUKOEDDD         PIC  X(04).
      *������ֹ�
              03  SC11-JKYSNUM          PIC  X(20).
      *���桦�ֹ�
              03  SC11-KIGO             PIC  X(20).
              03  SC11-BANGO            PIC  X(20).
      *�ݸ���̾
              03  SC11-HKNJANAME        PIC  X(20).
      *���ճ��
              03  SC11-KYURATE          PIC  X(02) OCCURS 3.
      *�ݸ�����
              03  SC11-HKNSYUBETU       PIC  X(02) OCCURS 4.
      *����������ʬ
              03  SC11-KOUGAKU          PIC  X(02) OCCURS 3.
      *��������
              03  SC11-SEIKYU-TBL OCCURS 2.
      *����
                  05  SC11-NISSU        PIC  X(02).
      *����
                  05  SC11-TENSU        PIC  X(07).
      *¾ˡ��ô����
                  05  SC11-TAHO         PIC  X(06).
      *������ô���
                  05  SC11-FTNMONEY     PIC  X(07).
      *���ٿ��Ⱦ㳲���������������
                  05  SC11-SEIKYUGK     PIC  X(09).
