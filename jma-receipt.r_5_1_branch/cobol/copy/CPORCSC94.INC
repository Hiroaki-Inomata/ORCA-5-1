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
      *    ���Ź԰ٲ���ɽ���Խ��ѥѥ�᥿��   COPY (CPORCSC94.INC)
      *                              (ORCSC94 �ǻ��ѡ�
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSC94AREA.
      *    ������ʬ�ʣ��������ͤκ��Խ��ʲ��̤ν��ɽ������ư�������
           03  LNK-SC94-KBN            PIC X(01).
      *    �����У�̾
           03  LNK-SC94-PG             PIC X(03).
      *    �оݹԡ�ɬ�����ϡ�
           03  LNK-SC94-IDX            PIC 9(04).
      *    �����ե饰�������β���Τ߻��ѡ�
           03  LNK-SC94-SYORIFLG       PIC 9(01).
      *