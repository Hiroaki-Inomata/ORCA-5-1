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
      ************************************************************
      *    ���Խ��������Խ��ѥ�᥿
      *    COPY  (CPORCSFTNRATE.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSFTNRATEAREA.
      *������
      *    ���ŵ��أɣ�
           03  LNK-FTNRATE-HOSPNUM         PIC  9(02).
      *    ���ԣɣ�
           03  LNK-FTNRATE-PTID            PIC  9(10).
      *    ����ǯ����
           03  LNK-FTNRATE-SRYYMD          PIC  X(08).
      *    �ݸ��ȹ礻�ֹ�
           03  LNK-FTNRATE-HKNCOMBINUM     PIC  X(04).
      *    �ݸ��ȹ礻�����Ѥߡ�1:�����Ѥߡ�
           03  LNK-FTNRATE-KBN             PIC  X(01).
      *
      *�ֵѹ���
           03  LNK-FTNRATE-HENKYAKU-AREA.
               05  LNK-FTNRATE-NYUIN       PIC  9(03).
               05  LNK-FTNRATE-GAIRAI      PIC  9(03).
      *
      *����������
           03  LNK-FTNRATE-RC              PIC S9(09) BINARY.
      *