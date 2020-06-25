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
      ********************************************************************
      *    �����������ɽ��ʡ�縩��
      *
      *  ��������    ��ȼ�        ����
      *  08/04/26    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
        01  SKT0705.
            03  SKT0705-TBL     OCCURS  2 TIMES.
      *����ǯ��
              05  SKT0705-SRYYM   PIC X(16).
      *���ŵ��ؽ���
              05  SKT0705-ADRS    PIC X(100).
      *���ŵ���̾��
              05  SKT0705-HOSPNAME    PIC X(100).
      *���߼�̾
              05  SKT0705-KAISETUNAME PIC X(40).
      *���ŵ��إ�����
              05  SKT0705-HOSPCD  PIC X(28).
      *ɸ�ֿ��Ų�̾
              05  SKT0705-SRYKANAME1  PIC X(30).
              05  SKT0705-SRYKANAME2  PIC X(30).
              05  SKT0705-SRYKANAME3  PIC X(30).
      *���
              05  SKT0705-GINKO   PIC X(20).
      *Ź̾
              05  SKT0705-TENMEI  PIC X(20).
      *�����ݸ��Կ�
              05  SKT0705-KENNAI-HKNJSU   PIC X(11).
      *�����ݸ��Կ�
              05  SKT0705-KENGAI-HKNJSU   PIC X(11).
      *����ơ��֥�
              05  SKT0705-SEIKYU-TBL      OCCURS  2 TIMES.
      *�����ơ��֥�
                07  SKT0705-NYUGAI-TBL      OCCURS  2 TIMES.
      *���
                  09  SKT0705-KENSU   PIC X(11).
      *����
                  09  SKT0705-NISSU   PIC X(11).
      *����
                  09  SKT0705-TENSU   PIC X(14).
      *���������
                07  SKT0705-S-KENSU   PIC X(5).
      *����������
                07  SKT0705-S-NISSU   PIC X(5).
      *���������
                07  SKT0705-S-KINGK   PIC X(11).
      *��ץơ��֥�
                07  SKT0705-GOKEI-TBL.
      *��ס����
                  09  SKT0705-G-KENSU PIC X(11).
      *��ס�����
                  09  SKT0705-G-NISSU PIC X(11).
      *��ס�����
                  09  SKT0705-G-TENSU PIC X(14).
      *��ס��������
                  09  SKT0705-G-SKENSU    PIC X(6).
      *��ס���������
                  09  SKT0705-G-SNISSU    PIC X(6).
      *��ס��������
                  09  SKT0705-G-SKINGK    PIC X(11).