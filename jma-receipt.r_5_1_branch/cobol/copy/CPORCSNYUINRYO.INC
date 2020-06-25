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
      *         ��������������      COPY   (CPORCSNYUINRYO.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SNYUINRYO-AREA.
      *    ������
           03  SNYUINRYO-SRYYMD            PIC X(08).
      *    ��ư�����ʬ
           03  SNYUINRYO-AUTOSET           PIC X(01).
      *    ������ʬ (01:������
           03  SNYUINRYO-SYORIKBN          PIC X(02).
      *    �����ֹ�
           03  SNYUINRYO-BTUNUM            PIC X(02).
      *    �¼��ֹ�
           03  SNYUINRYO-BRMNUM            PIC X(06).
      *    �ݸ��ȹ礻
           03  SNYUINRYO-HKNCOMBI          PIC 9(04).
      *    �ɲå�����
           03  SNYUINRYO-EXTRA-G.
               05  SNYUINRYO-EXTRA-KBN     PIC X(01).
               05  SNYUINRYO-EXTRA-CD      PIC X(02).
      *
      *    ���������
           03  SNYUINRYO-MAX               PIC 9(05).
           03  SNYUINRYO-OCC               OCCURS  100.
      *
      *        Ϣ��
               05  SNYUINRYO-NO            PIC X(02).
      *        ̾��
               05  SNYUINRYO-NAME          PIC X(60).
      *        ����������
               05  SNYUINRYO-NYUINRYO-G.
                   07  SNYUINRYO-NYUINRYO-KBN
                                           PIC X(01).
                   07  SNYUINRYO-NYUINRYO-CD
                                           PIC X(02).
      *        ������������
               05  SNYUINRYO-SRYCD         PIC X(09).
      *        ���㻻�ꥳ����
               05  SNYUINRYO-NASRYCD       PIC X(09).
      *        �������μ���ʽ��ɽ���ѡ�
               05  SNYUINRYO-TYPE          PIC X(02).
      *    ���ť����ɹ��
           03  SNYUINRYO-SRYCDTTL          PIC 9(14).
      *    ��������������������
           03  SNYUINRYO-BTU-KHNSRYCD      PIC X(09).
      *    ����������������
           03  SNYUINRYO-TOKHIGAITOKBN     PIC X(01).
           03  SNYUINRYO-TOKNYUIN-G.
               05  SNYUINRYO-TOKNYUIN-KBN  PIC X(01).
               05  SNYUINRYO-TOKNYUIN-CD   PIC X(02).
      *    ������������
      *    ��������������û���ں߰ʳ��Υ����ɤ��ֵѤ�������ϣ���
           03  SNYUINRYO-SELECT            PIC 9(01).