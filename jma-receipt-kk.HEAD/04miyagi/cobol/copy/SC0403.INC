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
        01  SC03.
      *     ����ǯ��
            03  SC03-SRYYM          PIC X(16).
      *     ���ŵ��إ�����
            03  SC03-HOSPCD         PIC X(14).
      *     ���ŵ��ؽ����
            03  SC03-ADRS           PIC X(100).
      *     ���ŵ���̾��
            03  SC03-HOSPNAME       PIC X(100).
      *     ���ŵ��������ֹ�
            03  SC03-TEL            PIC X(15).
      *     ���ŵ��س��߼Ի�̾
            03  SC03-KAISETU        PIC X(40).
      *
            03  SC03-SEIKYU-TBL         OCCURS  3 TIMES.
      *         ���ܤε��ա������
                05  SC03-KENSU      PIC X(05).
      *         ���ܤε��ա�������
                05  SC03-TENSU      PIC X(09).
      *         ���ܤε��ա������Ļ������������
                05  SC03-SEIKYUGK   PIC X(09).
      *         ����������������
                05  SC03-SKENSU     PIC X(05).
      *         ��������������ݸ������
                05  SC03-SKINGK     PIC X(09).
      *         �������������ɸ����ô��
                05  SC03-SFTNGK     PIC X(09).
