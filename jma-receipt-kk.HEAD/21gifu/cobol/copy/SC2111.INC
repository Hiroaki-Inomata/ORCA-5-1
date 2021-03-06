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
              03  SC11-PRTYM            PIC  X(16).
      *����ǯ����
              03  SC11-SEIYMD           PIC  X(22).
      *���ŵ��ؽ���
              03  SC11-ADRS             PIC  X(100).
      *���ŵ���̾��
              03  SC11-HOSPNAME         PIC  X(100).
      *���߼�̾
              03  SC11-KAISETU          PIC  X(80).
      *���ŵ��إ�����
              03  SC11-HOSPCDN          PIC  X(20).
      *�ݸ���ʬ
              03  SC11-HKNKBN-TBL OCCURS 2.
                  05  SC11-HKNKBN       PIC  X(02).
      *���ԥơ��֥�
              03  SC11-DATA-TBL         OCCURS  10.
      *������ֹ�
                  05  SC11-JKYSNUM      PIC  X(24).
      *��̾
                  05  SC11-NAME         PIC  X(20).
      *�����������ʬ
                  05  SC11-NYUGAIKBN1   PIC  X(02).
                  05  SC11-NYUGAIKBN2   PIC  X(02).
      *������
                  05  SC11-TOTALTEN     PIC  X(18).
      *��ͽ��������������ô����
                  05  SC11-KOHFTNTEN    PIC  X(18).
      *������ô��
                  05  SC11-ITBFTNGAKU   PIC  X(18).
      *�������
              03  SC11-GOKEI-TBL.
      *���� ���
                  05  SC11-N-GKENSU     PIC  X(08).
      *���� ������
                  05  SC11-N-GTOTALTEN  PIC  X(18).
      *���� ��ͽ������������ô����
                  05  SC11-N-GFTNTEN    PIC  X(18).
      *���� ������ô��
                  05  SC11-N-GFTNGAKU   PIC  X(18).
      *���� ���
                  05  SC11-G-GKENSU     PIC  X(08).
      *���� ������
                  05  SC11-G-GTOTALTEN  PIC  X(18).
      *���� ��ͽ������������ô����
                  05  SC11-G-GFTNTEN    PIC  X(18).
      *���� ������ô��
                  05  SC11-G-GFTNGAKU   PIC  X(18).
      *����
              03  SC11-BIKO-TBL.
                  05  SC11-BIKO         PIC  X(16) OCCURS 10.
