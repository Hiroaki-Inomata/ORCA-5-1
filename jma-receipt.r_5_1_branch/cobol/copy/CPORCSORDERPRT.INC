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
      *    �����ؼ�����Ģɼ�������������ѥ�   COPY (CPORCSORDERPRT.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.07.00     NACL-¿��Ǽ  11/10/XX  Ʊ���������б�
      ************************************************************
      *
       01  ORCSORDERPRTAREA.
      *    �����У�
           03  LNK-ORDERPRT-PG         PIC X(04).
      *    �оݥե��������
           03  LNK-ORDERPRT-KBN        PIC X(01).
      *    ���������ȯ��
           03  LNK-ORDERPRT-SYOHOU     PIC 9(01).
      *    ���ͽ����ȯ��
           03  LNK-ORDERPRT-CHUSYA     PIC 9(01).
      *    �ؼ��ȯ��
           03  LNK-ORDERPRT-SIJISEN    PIC 9(01).
      *    �ɥ�����������
           03  LNK-ORDERPRT-DRCD       PIC X(05).
      *    Ʊ����������ʬ
           03  LNK-ORDERPRT-DOUJITSUKBN    PIC X(01).
      *
      *    ����
      *    ȯ����
           03  LNK-ORDERPRT-PRINTYMD   PIC X(08).
      *    ȯ�Ի���
           03  LNK-ORDERPRT-PRINTHMS   PIC X(06).
      *
      *    ����������
           03  LNK-ORDERPRT-RCD        PIC 9(02).
      *