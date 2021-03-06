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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ���    2019/05/21  �����б�
      ********************************************************************
        01  SC09.
      *����ǯ��
            03  SC09-PRTGG          PIC X(04).
            03  SC09-PRTYY          PIC X(04).
            03  SC09-PRTMM          PIC X(04).
      *���ŵ��إ�����
            03  SC09-HOSPCD         PIC X(14).
      *��Į¼�ֹ�
            03  SC09-CITYNUM        PIC X(12).
      *��Į¼̾
            03  SC09-CITYNAME       PIC X(20).
      *���ŵ��ؽ���
            03  SC09-ADRS           PIC X(100).
      *���ŵ���̾��
            03  SC09-HOSPNAME       PIC X(100).
      *�����ֹ�
            03  SC09-TEL            PIC X(15).
      *���߼�̾
            03  SC09-KAISETU        PIC X(40).
      *�ڡ�����
            03  SC09-PAGE           PIC X(10).
      *������ֹ�
            03  SC09-JKYSNUM-TBL        OCCURS  7 TIMES.
                05  SC09-JKYSNUM    PIC X(20).
      *����
            03  SC09-SEX-TBL            OCCURS  7 TIMES.
                05  SC09-SEX        PIC X(02).
      *����ǯ����
            03  SC09-TANJOYMD-TBL       OCCURS  7 TIMES.
                05  SC09-TANJOG     PIC X(2).
                05  SC09-TANJOYMD   PIC X(12).
      *����ǯ��
            03  SC09-SRYYY-TBL          OCCURS  7 TIMES.
                05  SC09-SRYYY      PIC X(04).
            03  SC09-SRYMM-TBL          OCCURS  7 TIMES.
                05  SC09-SRYMM      PIC X(04).
      *���ʻ�̾
            03  SC09-KANANAME-TBL       OCCURS  7 TIMES.
                05  SC09-KANANAME   PIC X(30).
      *�ݸ����ֹ�
            03  SC09-HKNJANUM-TBL       OCCURS  7 TIMES.
                05  SC09-HKNJANUM   PIC X(16).
      *���Ų�
            03  SC09-SRYKA-TBL          OCCURS  7 TIMES.
                05  SC09-SRYKA      PIC X(04).
      *������ʬ
            03  SC09-NYUGAIKBN-TBL      OCCURS  7 TIMES.
                05  SC09-NYUGAIKBN  PIC X(02).
      *ˡ���ֹ�
            03  SC09-HBTNUM-TBL         OCCURS  7 TIMES.
                05  SC09-HBTNUM     PIC X(04).
      *Ĺ��
            03  SC09-TYOKI-TBL          OCCURS  7 TIMES.
                05  SC09-TYOKI      PIC X(02).
      *����
            03  SC09-NISSU-TBL          OCCURS  7 TIMES.
                05  SC09-NISSU      PIC X(04).
      *����
            03  SC09-TENSU-TBL          OCCURS  7 TIMES.
                05  SC09-TENSU      PIC X(14).
      *������ô��
            03  SC09-SEIKYUGK-TBL       OCCURS  7 TIMES.
                05  SC09-SEIKYUGK   PIC X(14).
      *��������
            03  SC09-SNISSU-TBL         OCCURS  7 TIMES.
                05  SC09-SNISSU     PIC X(04).
      *ɸ����ô��
            03  SC09-FTNGK-TBL          OCCURS  7 TIMES.
                05  SC09-FTNGK      PIC X(14).
      *����
            03  SC09-BIKO-TBL           OCCURS  7 TIMES.
                05  SC09-BIKO       PIC X(40).
      *����
            03  SC09-GTENSU         PIC X(14).
            03  SC09-GSEIKYUGK      PIC X(14).
            03  SC09-GFTNGK         PIC X(14).
