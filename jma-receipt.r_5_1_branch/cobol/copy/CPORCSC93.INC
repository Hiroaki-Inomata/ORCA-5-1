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
      *    �ޥ��åȡ������å���   COPY (CPORCSC93.INC)
      *                              (ORCSC93 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  06/07/20  ���������ޤȤ��ɲ�
      ************************************************************
      *
       01  ORCSC93AREA.
      *
      *    ��������
           03  LNK-SC93-PG             PIC X(04).
      *    �����ե饰�ʣ���Ʊ�측�������
           03  LNK-SC93-DELSYORIFLG    PIC  9(01).
      *
      *    ���å����ϡʣˣ����ˤΥ��åȥ�����
           03  LNK-SC93-SETCODE        PIC X(06).
      *    �����ʳ�����Ųʻ���ͭ��
           03  LNK-SC93-SYONIKA-ARI2   PIC  9(01).
      *    �������������ͭ��
           03  LNK-SC93-NETAKIRI-ARI   PIC  9(01).
      *    ���������ޤȤ�ͭ��
           03  LNK-SC93-KENSAARIFLG    PIC  9(01).
      *    �����Ѵ���ʬ
           03  LNK-SC93-HAIKIKBN-FLG   PIC  9(01).
      *
      *    ���������ޤȤᥨ�顼
           03  LNK-SC93-ERRCD          PIC  9(01).
      *