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
      *  01.00.02    ����      2012/06/06   Ģɼ�ͼ����ѹ�
      *  01.00.03    ���      2019/01/10   �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT3200.
            02  SKT3200-PRTYM   PIC X(16).
            02  SKT3200-HOSPCD  PIC X(7).
            02  SKT3200-HOSPNAME    PIC X(100).
            02  SKT3200-IKENSU
                    PIC X(5)    OCCURS  36 TIMES.
            02  SKT3200-TKENSU
                    PIC X(5)    OCCURS  36 TIMES.
            02  SKT3200-RKENSU
                    PIC X(5)    OCCURS  36 TIMES.
            02  SKT3200-KNKENSU PIC X(7).
            02  SKT3200-KGKENSU PIC X(7).
            02  SKT3200-FKKENSU PIC X(7).
            02  SKT3200-FRKENSU PIC X(7).
            02  SKT3200-NYKENSU PIC X(7).
            02  SKT3200-HKNJANAME
                    PIC X(10)   OCCURS  14 TIMES.
            02  SKT3200-KOUKI           OCCURS  13 TIMES.
              03  SKT3200-KOU-KEN PIC X(10).
              03  SKT3200-KOU-KENSU   PIC X(5).
            02  SKT3200-KOUKENSU    PIC X(7).
            02  SKT3200-TOKUBETU    PIC X(10).
