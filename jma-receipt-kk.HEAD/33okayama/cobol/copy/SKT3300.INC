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
      *  01.00.01    ����      2010/12/09   ��������������б�
      *  01.00.02    ���      2019/01/10   �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT3300.
            02  SKT3300-SEIYM   PIC X(16).
            02  SKT3300-HOSPCD  PIC X(14).
            02  SKT3300-KAMOKU  PIC X(26).
            02  SKT3300-ADRS    PIC X(100).
            02  SKT3300-HOSPNAME    PIC X(100).
            02  SKT3300-KAISETUNAME PIC X(40).
            02  SKT3300-TEL PIC X(15).
            02  SKT3300-SEIKYUYMD   PIC X(22).
            02  SKT3300-TOKUBETU    PIC X(10).
            02  SKT3300-SYUKEI-TBL      OCCURS  3 TIMES.
              03  SKT3300-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT3300-HKN-TBL         OCCURS  4 TIMES.
                  05  SKT3300-KENSU   PIC X(5).
                  05  SKT3300-TENSU   PIC X(9).
              03  SKT3300-KINGK   PIC X(9).
              03  SKT3300-RKINGK  PIC X(9).
