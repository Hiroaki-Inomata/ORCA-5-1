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
      *  04.08.01    ����         16/03/11  Ĺ�����2�Х��Ȥ��ѹ�
      *  04.08.02    ���         18/12/12  �������򥳥��Ⱥ��
      *  04.08.02    ���         18/12/12  �������򥳥��Ⱥ��
      *  05.01.01    ���         19/04/23  �����б�
      *  05.01.02    ���         20/03/11  ���츩�ε��ܻ��ͤ��б�
      ********************************************************************
        01  SC03.
      *���ŵ��إ�����
            03  SC03-HOSPCD     PIC X(07).
      *���ŵ��ءݽ���
            03  SC03-ADRS       PIC X(100).
      *���ŵ��ء�̾��
            03  SC03-HOSPNAME   PIC X(100).
      *���ŵ��ءݳ��߼�̾
            03  SC03-KAISETU    PIC X(40).
      *���ŵ��ء������ֹ�
            03  SC03-HOSPTEL    PIC X(15).
      *����ǯ��
            03  SC03-PRTGG      PIC X(04).
            03  SC03-PRTYY      PIC X(02).
            03  SC03-PRTMM      PIC X(02).
      *����ǯ����
            03  SC03-SEIGG      PIC X(04).
            03  SC03-SEIYY      PIC X(02).
            03  SC03-SEIMM      PIC X(02).
            03  SC03-SEIDD      PIC X(02).
      *����ɽ��ʬ
            03  SC03-TENSUHYO   PIC X(01).
      *�ڡ�����
            03  SC03-PAGE       PIC X(03).
            03  SC03-KANJA-TBL          OCCURS  6 TIMES.
      *��̾
                05  SC03-NAME       PIC X(20).
      *����
                05  SC03-SEX        PIC X(01).
      *��ǯ����
                05  SC03-TANJOYMD   PIC X(07).
      *�ݸ����ֹ�
                05  SC03-HKNJANUM   PIC X(08).
      *�����ֹ�(��ô���ֹ�)
                05  SC03-FTNJANUM   PIC X(08)    OCCURS  2 TIMES.
      *������ֹ�
                05  SC03-JKYSNUM    PIC X(07)    OCCURS  2 TIMES.
      *����
                05  SC03-KIGO       PIC X(16).
      *�ֹ�
                05  SC03-NUM        PIC X(24). 
      *����ǯ��
                05  SC03-SRYYY      PIC X(02).
                05  SC03-SRYMM      PIC X(02).
      *���ճ��
                05  SC03-KYURATE    PIC X(02).
      *�ܿͲ�²��ʬ
                05  SC03-HONKZKKBN  PIC X(01).
      *����
                05  SC03-NISSU      PIC X(02).
      *����
                05  SC03-TENSU      PIC X(07).
      *������ô��
                05  SC03-ITBFTN     PIC X(07).
      *����ʬ����
                05  SC03-KOHTENSU   PIC X(07)    OCCURS  2 TIMES.
      *�����оݴ�����ô��
                05  SC03-KOHPTFTN   PIC X(07)    OCCURS  2 TIMES.
      *Ĺ��
                05  SC03-TYOKI      PIC X(02).
      *Ϸ�Ͷ�ʬ
                05  SC03-RJNKBN     PIC X(02).
      *¾ˡ�����ֹ�
                05  SC03-TAHONUM    PIC X(02).
      *�����ֹ�
                05  SC03-SRYKA      PIC X(02).
      *��ץ��ꥢ
            03  SC03-GOKEI-TBL. 
                05  SC03-GKENSU     PIC X(03).
                05  SC03-GTENSU     PIC X(07).
                05  SC03-GITBFTN    PIC X(07).
                05  SC03-GKOHTENSU  PIC X(07)    OCCURS  2 TIMES.
                05  SC03-GKOHPTFTN  PIC X(07)    OCCURS  2 TIMES.
      *���ŵ��إ����ɡʴ��츩�ѡ�
            03  SC03-HOSPCD-P21     PIC X(03).
