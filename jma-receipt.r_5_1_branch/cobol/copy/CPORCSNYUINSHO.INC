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
      *    �����������������Խ��ѥ�   COPY (CPORCSNYUINSHO.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSNYUINSHOAREA.
      *    ������ʬ�ʣ������Ų��Խ���������������������������ŲʤΤ�)
           03  ORCSNYUINSHO-KBN        PIC X(01).
      *    ��������ɬ�ܡ�
           03  ORCSNYUINSHO-NYUINYMD   PIC X(08).
      *    �ౡ����ɬ�ܡ�
           03  ORCSNYUINSHO-TAIINYMD   PIC X(08).
      *    �������ϡʽ�����ʬ�ᣱ �Ͻ��ϡ��� �� ���Ϲ��ܡ�
           03  ORCSNYUINSHO-SRYKA-AREA.
               05  ORCSNYUINSHO-SRYKA-G    OCCURS   5.
      *            ���Ų�
                   07  ORCSNYUINSHO-SRYKA      PIC X(02).
      *            �ɥ�����
                   07  ORCSNYUINSHO-DRCD       PIC X(05).
               05  ORCSNYUINSHO-SRYKA-MAX      PIC 9(02).
      *    �缣��ʽ�����ʬ�ᣲ �ǥɥ��������ʤ����Τ�ɬ�ܡ�
           03  ORCSNYUINSHO-SYUDRCD    PIC X(05).
      *    Ʊ����������ʬ�ʣ���Ʊ����������
           03  ORCSNYUINSHO-DOUJITSUKBN    PIC X(01).
      *