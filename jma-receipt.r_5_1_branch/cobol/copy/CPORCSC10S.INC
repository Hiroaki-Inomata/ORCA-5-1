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
      *----------------------------------------------------------*
      *    ���Ź԰ٿǻ�����ưȯ���Խ��ѥ�᥿��  COPY (CPORCSC10S.INC)
      *                              (ORCSC10S�ǻ��ѡ�
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSC10SAREA.
      *    ������ʬ�ʣ��������ȯ��������Ʊ���ƿǡ���������������
      *              �������⡢�������ؤ����������úƿǿ��ؤ���
           03  LNK-SC10S-SYORI          PIC X(01).
      *    �ǽ��ơ��֥����
           03  LNK-SC10S-IDX2           PIC 9(04).
      *!   �ǻ����Խ��ʤ�(1:�ǻ�������Ѥߡ�
      *!   03  LNK-SC10S-SINSATU        PIC 9(01).
      *!   03  LNK-SC10S-ZAINUM-AREA.
      *!       05  LNK-SC10S-ZAINUM     PIC  9(08)   OCCURS  5.
      *    ���ϥơ��֥����
           03  LNK-SC10S-STRIDX         PIC 9(04).
      *
