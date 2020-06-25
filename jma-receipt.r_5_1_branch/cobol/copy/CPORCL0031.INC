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
      *    ���У����ӥǡ��������ѥ�᥿����COPY (CPORCL0031.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  LNK-ORCL0031AREA.
      *    ������ʬ��1:API��
           03  LNK-KBN-CODE            PIC X(01).
      *    ������ʬ����1:��̾�Τߡ�
           03  LNK-KBN-CODE2           PIC X(01).
      *    API�С�������1:ver.2)
           03  LNK-KBN-V2              PIC X(01).
      *
      *    �ݸ��ȹ礻�ֹ�����ver.2�Τߡ�
           03  LNK-IN-HKNCOMBI         PIC X(04).
      *    �������ӥǡ����ɲá�ver.2�Τߡ�
           03  LNK-ADDKBN              PIC X(01).
           03  LNK-MAX-ZAINUM          PIC 9(08).
      *
      *    �ݸ��ȹ礻�ֹ�ʺ�����
           03  LNK-OUT-HKNCOMBI        PIC X(04).
      *    ���ӥǡ����������
           03  LNK-OUT-CNT1            PIC 9(04).
      *    ��̾��Ͽ���
           03  LNK-OUT-CNT2            PIC 9(04).
      *
      *    ���ӥǡ����������ֵ�
           03  LNK-OUT01-AREA.
      *        ���顼������
               05  LNK-ERR1-CODE            PIC X(02).
               05  LNK-ERR1-MSG             PIC X(100).
               05  LNK-MEDI-TBL-G           OCCURS   50.
      *        �ٹ�
                   07  LNK-MEDI-CODE            PIC X(03).
                   07  LNK-MEDI-MSG             PIC X(100).
                   07  LNK-MEDI-POSITION        PIC 9(02).
                   07  LNK-MEDI-ITEM            PIC 9(02).
                   07  LNK-MEDI-SRYCD           PIC X(09).
      *    ��̾�������ֵ�
           03  LNK-OUT02-AREA.
      *        ���顼������
               05  LNK-ERR2-CODE            PIC X(02).
               05  LNK-ERR2-MSG             PIC X(100).
               05  LNK-DIAG-TBL-G           OCCURS   50.
      *            �ٹ𥳡���
                   07  LNK-DIAG-WARNING         PIC X(03).
      *            ��å�����
                   07  LNK-DIAG-MSG             PIC X(100).
      *            ���ֹ�
                   07  LNK-DIAG-POSITION        PIC 9(02).
      *            ��̾
                   07  LNK-DIAG-NAME            PIC X(80).
      *            ��̾������
                   07  LNK-DIAG-CODE            PIC X(50).
      *            �ܹ�(�ѻ�)
                   07  LNK-DIAG-CHANGE          PIC X(02).
      *
      *