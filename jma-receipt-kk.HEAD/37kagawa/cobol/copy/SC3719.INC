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
        01  SC19.
      *���ŵ���ID
            03  SC19-HOSPCD     PIC X(20).
      *���ŵ���̾��
            03  SC19-HOSPNAME   PIC X(60).
      *���ŵ��ؽ����
            03  SC19-ADRS       PIC X(60).
      *���ŵ��������ֹ�
            03  SC19-TEL        PIC X(15).
      *��ɽ��
            03  SC19-KAISETU    PIC X(60).
      *���̾
            03  SC19-GINKOU     PIC X(20).
      *��Ź̾
            03  SC19-SITEN      PIC X(20).
      *���¶�ʬ
            03  SC19-KOUZAKBN   PIC X(02)    OCCURS  2 TIMES.
      *�����ֹ�
            03  SC19-KOUZANUM   PIC X(10).
      *����ǯ��
            03  SC19-SRYYM      PIC X(16).
      *����ǯ����
            03  SC19-SEIYMD     PIC X(22).
      *�����ʬ
            03  SC19-KOHKBN     PIC X(02)    OCCURS  2 TIMES.
      *��������
            03  SC19-SEIKYUGK-G PIC X(09).
      *��ץڡ���
            03  SC19-GPAGE      PIC X(03).
      *�ڡ���
            03  SC19-PAGE       PIC X(03).
      *���٥��ꥢ
            03  SC19-SEIKYU-TBL         OCCURS  15 TIMES.
      *�����
                05  SC19-JKYSNUM    PIC X(16).
      *��̾
                05  SC19-NAME       PIC X(32).
      *������
                05  SC19-TANJOYY    PIC X(03).
                05  SC19-TANJOMM    PIC X(02).
                05  SC19-TANJODD    PIC X(02).
      *������ʬ
                05  SC19-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
      *���Ų���
                05  SC19-SRYKA      PIC X(08).
      *��ô���
                05  SC19-FTNRATE    PIC X(02)    OCCURS  2 TIMES.
      *��������
                05  SC19-SHOHOUKBN  PIC X(02)    OCCURS  2 TIMES.
      *��������
                05  SC19-NISSU      PIC X(02).
      *����
                05  SC19-TENSU      PIC X(09).
      *������������
                05  SC19-SEIKYUGK   PIC X(09).
      *���ס���ץ��ꥢ
            03  SC19-GOKEI-TBL          OCCURS  2 TIMES.
                05  SC19-GKENSU     PIC X(03).
                05  SC19-GSEIKYUGK  PIC X(10).
