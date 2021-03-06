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
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2012/10/09   �����ֹ��ɲ�
      *  01.00.02    ����      2014/05/12   ǯ���ʬ�ɲ�
      *  01.00.03    ����      2016/06/06   ���ͣ������ͣ��ɲ�
      *  01.00.04    ���      2017/04/10   ���٤�ξ���������ε��ܽ���
      *  01.00.05    ����      2018/10/11   �������򥳥��Ⱥ��
      ********************************************************************
       01  SC09.
      *ɽ��ǯ��
              03  SC09-PRTYM            PIC  X(16).
      *����ǯ����
              03  SC09-SEIYMD           PIC  X(22).
      *���ŵ���͹���ֹ�
              03  SC09-POST             PIC  X(10).
      *���ŵ��ؽ���
              03  SC09-ADRS             PIC  X(100).
      *���ŵ���̾��
              03  SC09-HOSPNAME         PIC  X(100).
      *���߼�̾
              03  SC09-KAISETU          PIC  X(80).
      *���ŵ��إ�����
              03  SC09-HOSPCDN          PIC  X(20).
      *�ݸ���ʬ
              03  SC09-HKNKBN-TBL OCCURS 4.
                  05  SC09-HKNKBN       PIC  X(02).
      *���ԥơ��֥�
              03  SC09-DATA-TBL        OCCURS  10.
      *������ֹ�
                  05  SC09-JKYSNUM      PIC  X(24).
      *��̾
                  05  SC09-NAME         PIC  X(20).
      *�����������ʬ
                  05  SC09-NYUGAIKBN1   PIC  X(02).
                  05  SC09-NYUGAIKBN2   PIC  X(02).
      *������
                  05  SC09-TOTALTEN     PIC  X(18).
      *��ͽ��������������ô����
                  05  SC09-KOHFTNTEN    PIC  X(18).
      *������ô��
                  05  SC09-ITBFTNGAKU   PIC  X(18).
      *�������
              03  SC09-GOKEI-TBL.
      *���� ���
                  05  SC09-N-GKENSU     PIC  X(08).
      *���� ������
                  05  SC09-N-GTOTALTEN  PIC  X(18).
      *���� ��ͽ������������ô����
                  05  SC09-N-GFTNTEN    PIC  X(18).
      *���� ������ô��
                  05  SC09-N-GFTNGAKU   PIC  X(18).
      *���� ���
                  05  SC09-G-GKENSU     PIC  X(08).
      *���� ������
                  05  SC09-G-GTOTALTEN  PIC  X(18).
      *���� ��ͽ������������ô����
                  05  SC09-G-GFTNTEN    PIC  X(18).
      *���� ������ô��
                  05  SC09-G-GFTNGAKU   PIC  X(18).
      *������
              03  SC09-BIKO-TBL OCCURS 10.
                  05  SC09-BIKO         PIC  X(16).
              03  SC09-BIKO-TBL2 OCCURS 10.
                  05  SC09-BIKO2        PIC  X(16).
      *�����ֹ�
              03  SC09-TEL              PIC  X(15).
      *ǯ���ʬ
              03  SC09-AGEKBN-TBL OCCURS 2.
                  05  SC09-AGEKBN       PIC  X(02).
      *�����󣳡���
              03  SC09-BIKO-TBL3 OCCURS 10.
                  05  SC09-BIKO3        PIC  X(16).
              03  SC09-BIKO-TBL4 OCCURS 10.
                  05  SC09-BIKO4        PIC  X(16).
      *������
              03  SC09-BIKO-TBL5 OCCURS 10.
                  05  SC09-BIKO5        PIC  X(22).
