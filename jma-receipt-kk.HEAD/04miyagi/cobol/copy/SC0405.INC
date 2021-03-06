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
        01  SC05.
      *     ����ǯ��
            03  SC05-SRYYM          PIC X(16).
      *     ����ǯ����
            03  SC05-SEIYMD         PIC X(22).
      *     ���ŵ��إ�����
            03  SC05-HOSPCD         PIC X(14).
      *     ���ŵ��ؽ����
            03  SC05-ADRS           PIC X(100).
      *     ���ŵ���̾��
            03  SC05-HOSPNAME       PIC X(100).
      *     ���ŵ��������ֹ�
            03  SC05-TEL            PIC X(15).
      *     ���ŵ��س��߼Ի�̾
            03  SC05-KAISETU        PIC X(40).
      *     ��Į¼̾
            03  SC05-CITYNAME       PIC X(26).
      *     �ڡ�����
            03  SC05-PAGE           PIC X(03).
            03  SC05-SEIKYU-TBL         OCCURS  10 TIMES.
      *         ������ʬ
                05  SC05-NYUGAIKBN  PIC X(01).
      *         ��ô���ֹ�
                05  SC05-FTNJANUM   PIC X(08).
      *         ������ֹ�
                05  SC05-JKYSNUM    PIC X(14).
      *         �ݸ����ֹ�
                05  SC05-HKNJANUM   PIC X(16).
      *         ����ǯ����
                05  SC05-TANJOGG    PIC X(02).
                05  SC05-TANJOYY    PIC X(02).
                05  SC05-TANJOMM    PIC X(02).
                05  SC05-TANJODD    PIC X(02).
      *         ��̾
                05  SC05-NAME       PIC X(26).
      *         ����
                05  SC05-SEX        PIC X(01).
      *         ��������
                05  SC05-NISSU      PIC X(02).
      *         ��������
                05  SC05-TENSU      PIC X(07).
      *         ���Ļ������������
                05  SC05-SEIKYUGK   PIC X(07).
      *         ��������
                05  SC05-SNISSU     PIC X(02).
      *         �����ݸ������
                05  SC05-SKINGK     PIC X(07).
      *         ����ɸ����ô��
                05  SC05-SFTNGK     PIC X(07).
      *         �����ֹ�
                05  SC05-KOHNUM     PIC X(02).
      *         �ޥ�Ĺ
                05  SC05-TYOKI      PIC X(02).
      *
            03  SC05-GOKEI-TBL.
      *         ��׷��
                05  SC05-GKENSU     PIC X(02).
      *         �������
                05  SC05-GTENSU     PIC X(09).
      *         ������Ļ������������
                05  SC05-GSEIKYUGK  PIC X(09).
      *         ��׿����ݸ������
                05  SC05-GSKINGK    PIC X(09).
      *         ��׿���ɸ����ô��
                05  SC05-GSFTNGK    PIC X(09).
