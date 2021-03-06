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
      *  01.00.01    ����         11/05/13  �����ֹ���ɲ�
      *  01.00.02    ����         12/07/31  ���ض�ʬ���Ѥ�ʸ������ɲ�
      *  01.00.03    ����         18/10/10  �������򥳥��Ⱥ��
      ********************************************************************
        01  SC091.
      *����ǯ����
            03  SC091-SEIYMD              PIC X(22).
      *���ŵ��إ�����
            03  SC091-HOSPCD              PIC X(14).
      *���ض�ʬ
            03  SC091-SYUGAKU             PIC X(06).
            03  SC091-SYUGAKU2            PIC X(10).

      *������ֹ�
            03  SC091-JKYSNUM-TBL             OCCURS  8 TIMES.
              05  SC091-JKYSNUM           PIC X(14).
      *����ǯ����
            03  SC091-TANJOYMD-TBL            OCCURS  8 TIMES.
              05  SC091-TANJOG            PIC X(2).
              05  SC091-TANJOYMD          PIC X(12).
      *����ǯ��
            03  SC091-SRYYM-TBL               OCCURS  8 TIMES.
              05  SC091-SRYG              PIC X(2).
              05  SC091-SRYYM             PIC X(8).
      *��̾
            03  SC091-NAME-TBL                OCCURS  8 TIMES.
              05  SC091-NAME              PIC X(20).
      *�ݸ����ֹ�
            03  SC091-HKNJANUM-TBL            OCCURS  8 TIMES.
              05  SC091-HKNJANUM          PIC X(16).
      *���Ų�
            03  SC091-SRYKA-TBL               OCCURS  8 TIMES.
              05  SC091-SRYKA             PIC X(4).
      *������ʬ
            03  SC091-NYUGAIKBN-TBL           OCCURS  8 TIMES.
              05  SC091-NYUGAIKBN         PIC X(2).
      *ˡ���ֹ�(������ô����)
            03  SC091-HBTNUM-TBL              OCCURS  8 TIMES.
              05  SC091-HBTNUM            PIC X(4).
      *Ĺ��(������¤���)
            03  SC091-TYOKI-TBL               OCCURS  8 TIMES.
              05  SC091-TYOKI             PIC X(2).
      *����
            03  SC091-TENSU-TBL               OCCURS  8 TIMES.
              05  SC091-TENSU             PIC X(14).
      *������ô��
            03  SC091-SEIKYUGK-TBL            OCCURS  8 TIMES.
              05  SC091-SEIKYUGK          PIC X(14).
      *��������
            03  SC091-SNISSU-TBL              OCCURS  8 TIMES.
              05  SC091-SNISSU            PIC X(4).
      *ɸ����ô��
            03  SC091-FTNGK-TBL               OCCURS  8 TIMES.
              05  SC091-FTNGK             PIC X(12).
      *����
            03  SC091-BIKO-TBL                OCCURS  8 TIMES.
              05  SC091-BIKO              PIC X(40).
      *�����ֹ�
            03  SC091-SEIRINUM-TBL            OCCURS  8 TIMES.
              05  SC091-SEIRINUM          PIC X(4).
