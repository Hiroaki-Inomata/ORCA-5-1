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
      ************************************************************
      *    ���ܴ����в����������������ɼ������֥ѥ�᥿����COPY (CPORCSEIYOUKKS.INC)
      *                                ���ѥץ������ (ORCSEIYOUKKS)
      *    �������
      *      ������������
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSEIYOUKKS-AREA.
      *
      *����ǯ��
           03  LNK-EIYOUKKS-SANTEIYM               PIC  X(06).
      *������������
           03  LNK-EIYOUKKS-SRYCD                  PIC  X(09).
      *�ֵѹ���
           03  LNK-EIYOUKKS-HENKYAKU-AREA.
      *�в����������������ɡ�ʿ������ǯ����ޤ��ֵѡ�
              05  LNK-EIYOUKKS-CHGSRYCD            PIC  X(09).
      *�в����������������ɡ�ʿ������ǯ����ʹߤΥ쥻�ץ�ά�ε����Ѥ��ֵѡ�
              05  LNK-EIYOUKKS-RECE-CHGSRYCD       PIC  X(09).
      *���������ɡʣ����в������������ֵѤ��ꡡ�����в������������ֵѤʤ���
           03  LNK-EIYOUKKS-RC                     PIC S9(9) BINARY.
      *
