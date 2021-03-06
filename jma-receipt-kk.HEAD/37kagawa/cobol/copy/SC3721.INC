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
        01  SC21.
      *����ǯ��
            03  SC21-SRYYM      PIC X(06).
      *��������
            03  SC21-SEIYMD     PIC X(22).
      *���������
            03  SC21-SEIKYUGK-G PIC X(09).
      *���ŵ���̾��
            03  SC21-HOSPNAME   PIC X(100).
      *���ŵ��ؽ����
            03  SC21-ADRS       PIC X(100).
      *���߼Ի�̾
            03  SC21-KAISETU    PIC X(40).
      *���̾
            03  SC21-GINKO      PIC X(12).
      *��Ź̾
            03  SC21-SITEN      PIC X(12).
      *�����ֹ�
            03  SC21-KOUZA      PIC X(08).
      *��ץڡ���
            03  SC21-GPAGE      PIC X(03). 
      *�ڡ���
            03  SC21-PAGE       PIC X(03).
            03  SC21-SEIKYU-TBL         OCCURS  12 TIMES.
      *������ֹ�
                05  SC21-JKYSNUM    PIC X(16).
      *��̾
                05  SC21-NAME       PIC X(32).
      *�ݸ�����(�����Ҳ� �������� �������)
                05  SC21-HKNSYUBETU PIC X(02)    OCCURS  3 TIMES.
      *������ʬ(�������� ����������)
                05  SC21-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
      *������������
                05  SC21-TENSU      PIC X(07).
      *��ô��ʬ(�������� ��������)
                05  SC21-FTNRATE    PIC X(02)    OCCURS  2 TIMES.
      *¾ˡ��ô��
                05  SC21-TAHO       PIC X(07).
      *���������
                05  SC21-KOGAKU     PIC X(07).
      *��ô��
                05  SC21-FTNGK      PIC X(07).
      *����ǯ����
                05  SC21-NYUINYY    PIC X(03).
                05  SC21-NYUINMM    PIC X(02).
                05  SC21-NYUINDD    PIC X(02).
      *��������
                05  SC21-NNISSU     PIC X(02).
      *������
                05  SC21-SEIKYUGK   PIC X(09).
            03  SC21-GOKEI-TBL.
      *��������
                05  SC21-GSEIKYUGK  PIC X(09).
