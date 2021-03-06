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
      *    �ౡ�����������������ѥ�᥿����COPY (CPORCHCN02.INC)
      *                              ���ѥץ������ (ORCHCN02.CBL)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ������
      *      �ౡ��
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCHCN02SUB-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-ORCHCN02SUB-HOSPNUM                PIC 9(2).
      *���ԣɣ�
           03  LNK-ORCHCN02SUB-PTID                   PIC  9(10).
      *������
           03  LNK-ORCHCN02SUB-NYUINYMD               PIC  X(08).
      *�ౡ��
           03  LNK-ORCHCN02SUB-TAIINYMD               PIC  X(08).
      *�ֵѹ���
           03  LNK-ORCHCN02SUB-HENKYAKU-AREA.
      *�����ֵѥơ��֥�
             04  LNK-ORCHCN02SUB-HENKYAKU-TABLE       OCCURS  20.
      *������������
               05  LNK-ORCHCN02SUB-SYUBETUCD          PIC  X(09).
      *������̾��
               05  LNK-ORCHCN02SUB-SYUBETUMEI         PIC  X(50).
      *��������
               05  LNK-ORCHCN02SUB-NISSU              PIC  9(05).
      *��������
             04  LNK-ORCHCN02SUB-KIKAN                PIC  9(05).
      *�������ֳ�����
             04  LNK-ORCHCN02SUB-KIKANSTRYMD          PIC  X(08).
      *�������ֽ�λ��
             04  LNK-ORCHCN02SUB-KIKANENDYMD          PIC  X(08).
      *������������
             04  LNK-ORCHCN02SUB-TKIKAN               PIC  9(05).
      *
      *���������ɡʣ����ѥ�᥿���顼������������פʤ�
      *            �����̻��������顼��
           03  LNK-ORCHCN02SUB-RC                     PIC S9(9) BINARY.
      *
