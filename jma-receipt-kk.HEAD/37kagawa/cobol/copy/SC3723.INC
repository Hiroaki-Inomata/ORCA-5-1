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
        01  SC23.
      *����ǯ��
            03  SC23-SRYYM      PIC X(06).
      *��������
            03  SC23-SEIYMD     PIC X(22).
      *���������
            03  SC23-SEIKYUGK-G PIC X(09).
      *���ŵ���̾��
            03  SC23-HOSPNAME   PIC X(100).
      *���ŵ��ؽ����
            03  SC23-ADRS       PIC X(100).
      *���߼Ի�̾
            03  SC23-KAISETU    PIC X(40).
      *���̾
            03  SC23-GINKO      PIC X(12).
      *��Ź̾
            03  SC23-SITEN      PIC X(12).
      *����
            03  SC23-KOUZA      PIC X(08).
      *��ץڡ���
            03  SC23-GPAGE      PIC X(03). 
      *�ڡ���
            03  SC23-PAGE       PIC X(03).
            03  SC23-SEIKYU-TBL         OCCURS  13 TIMES.
      *������ֹ�
                05  SC23-JKYSNUM    PIC X(16).
      *��̾
                05  SC23-NAME       PIC X(32).
      *�ݸ�����
      *(�������� �����Ҳ� �������� �������� �������)
                05  SC23-HKNSYUBETU PIC X(02)    OCCURS  5 TIMES.
      *������ʬ(�������� ����������)
                05  SC23-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
      *������������
                05  SC23-TENSU      PIC X(07).
      *¾ˡ��ô��
                05  SC23-TAHO       PIC X(07).
      *��ô��
                05  SC23-FTNGK      PIC X(07).
      *���ް�����ô��
                05  SC23-YKZFTN     PIC X(07).
      *��������
                05  SC23-SNISSU     PIC X(02).
      *����������
                05  SC23-SKINGK     PIC X(06).
      *����ǯ����
                05  SC23-NYUINYY    PIC X(03).
                05  SC23-NYUINMM    PIC X(02).
                05  SC23-NYUINDD    PIC X(02).
      *��������
                05  SC23-NNISSU     PIC X(02).
      *������
                05  SC23-SEIKYUGK   PIC X(09).
            03  SC23-GOKEI-TBL.
      *�������
                05  SC23-GTENSU     PIC X(09).
      *���¾ˡ��ô��
                05  SC23-GTAHO      PIC X(09).
                05  SC23-GFTNGK     PIC X(09).
                05  SC23-GSEIKYUGK  PIC X(09).
